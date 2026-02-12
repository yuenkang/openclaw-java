package com.openclaw.gateway.auth;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Gateway authentication service.
 * Corresponds to TypeScript's auth.ts: resolveGatewayAuth() +
 * authorizeGatewayConnect().
 */
@Slf4j
public class AuthService {

    private final ConfigService configService;

    /**
     * Track consecutive failures per remote address for cooldown.
     */
    private final Map<String, FailureRecord> failureRecords = new ConcurrentHashMap<>();

    public AuthService(ConfigService configService) {
        this.configService = configService;
    }

    // ─── Types ────────────────────────────────────────────────────────────

    /**
     * Check whether authentication is required.
     * Returns false if neither token nor password is configured.
     */
    public boolean isAuthRequired() {
        return resolveAuth().isConfigured();
    }

    /**
     * Resolve authentication configuration from config file + environment
     * variables.
     * Corresponds to TS resolveGatewayAuth().
     */
    public ResolvedGatewayAuth resolveAuth() {
        OpenClawConfig config = configService.loadConfig();
        OpenClawConfig.GatewayConfig gw = config.getGateway();
        OpenClawConfig.GatewayAuthConfig authCfg = (gw != null) ? gw.getAuth() : null;

        // Config values
        String cfgToken = (authCfg != null) ? authCfg.getToken() : null;
        String cfgPassword = (authCfg != null) ? authCfg.getPassword() : null;
        String cfgMode = (authCfg != null) ? authCfg.getMode() : null;

        // Environment variable fallback (matches TS behavior)
        String token = firstNonBlank(cfgToken,
                System.getenv("OPENCLAW_GATEWAY_TOKEN"),
                System.getenv("CLAWDBOT_GATEWAY_TOKEN"));
        String password = firstNonBlank(cfgPassword,
                System.getenv("OPENCLAW_GATEWAY_PASSWORD"),
                System.getenv("CLAWDBOT_GATEWAY_PASSWORD"));

        // Mode defaults to "token" if not specified,
        // switches to "password" if only password is set
        String mode = cfgMode;
        if (mode == null || mode.isBlank()) {
            mode = isNotBlank(password) && !isNotBlank(token) ? "password" : "token";
        }

        return new ResolvedGatewayAuth(mode, token, password);
    }

    private static String firstNonBlank(String... values) {
        for (String v : values) {
            if (isNotBlank(v)) {
                return v;
            }
        }
        return null;
    }

    // ─── Core API ─────────────────────────────────────────────────────────

    private static boolean isNotBlank(String s) {
        return s != null && !s.isBlank();
    }

    /**
     * Authorize a gateway connection.
     * Corresponds to TS authorizeGatewayConnect().
     *
     * @param connectAuth credentials from the client (may be null)
     * @param remoteAddr  remote address for rate limiting
     * @param isLocal     whether this is a local direct request
     * @return authorization result
     */
    public GatewayAuthResult authorize(ConnectAuth connectAuth, String remoteAddr, boolean isLocal) {
        ResolvedGatewayAuth resolved = resolveAuth();

        if (!resolved.isConfigured()) {
            // No auth configured → allow everything
            return GatewayAuthResult.success("none");
        }

        // Local direct requests bypass auth (matches TS isLocalDirectRequest behavior)
        if (isLocal) {
            return GatewayAuthResult.success("local");
        }

        // Check cooldown
        if (isInCooldown(remoteAddr)) {
            return GatewayAuthResult.failure("rate_limited");
        }

        String clientToken = (connectAuth != null) ? connectAuth.token() : null;
        String clientPassword = (connectAuth != null) ? connectAuth.password() : null;

        // Authorize based on mode (matches TS strict mode logic)
        if ("token".equals(resolved.mode())) {
            if (!isNotBlank(resolved.token())) {
                return GatewayAuthResult.failure("token_missing_config");
            }
            if (!isNotBlank(clientToken)) {
                return GatewayAuthResult.failure("token_missing");
            }
            if (!safeEqual(clientToken, resolved.token())) {
                recordFailure(remoteAddr);
                return GatewayAuthResult.failure("token_mismatch");
            }
            clearFailures(remoteAddr);
            return GatewayAuthResult.success("token");
        }

        if ("password".equals(resolved.mode())) {
            if (!isNotBlank(resolved.password())) {
                return GatewayAuthResult.failure("password_missing_config");
            }
            if (!isNotBlank(clientPassword)) {
                return GatewayAuthResult.failure("password_missing");
            }
            if (!safeEqual(clientPassword, resolved.password())) {
                recordFailure(remoteAddr);
                return GatewayAuthResult.failure("password_mismatch");
            }
            clearFailures(remoteAddr);
            return GatewayAuthResult.success("password");
        }

        return GatewayAuthResult.failure("unauthorized");
    }

    /**
     * Check if a remote address is in cooldown period.
     */
    public boolean isInCooldown(String remoteAddr) {
        if (remoteAddr == null) {
            return false;
        }
        FailureRecord record = failureRecords.get(remoteAddr);
        if (record == null) {
            return false;
        }

        OpenClawConfig config = configService.loadConfig();
        OpenClawConfig.AuthConfig authCfg = config.getAuth();
        int maxFailures = (authCfg != null) ? authCfg.getMaxFailures() : 5;
        int cooldownMs = (authCfg != null) ? authCfg.getCooldownMs() : 10000;

        if (record.count >= maxFailures) {
            long elapsed = System.currentTimeMillis() - record.lastFailureTime;
            if (elapsed < cooldownMs) {
                return true;
            }
            failureRecords.remove(remoteAddr);
        }
        return false;
    }

    // ─── Rate limiting ────────────────────────────────────────────────────

    /**
     * Timing-safe string comparison to prevent timing attacks.
     * Corresponds to TS safeEqual() using timingSafeEqual.
     */
    private static boolean safeEqual(String a, String b) {
        if (a == null || b == null) {
            return false;
        }
        byte[] aBytes = a.getBytes(StandardCharsets.UTF_8);
        byte[] bBytes = b.getBytes(StandardCharsets.UTF_8);
        return MessageDigest.isEqual(aBytes, bBytes);
    }

    private void recordFailure(String remoteAddr) {
        if (remoteAddr == null) {
            return;
        }
        failureRecords.compute(remoteAddr, (k, v) -> {
            if (v == null) {
                v = new FailureRecord();
            }
            v.count++;
            v.lastFailureTime = System.currentTimeMillis();
            return v;
        });
        FailureRecord record = failureRecords.get(remoteAddr);
        log.warn("Auth failure from {} (count={})", remoteAddr, record != null ? record.count : 0);
    }

    private void clearFailures(String remoteAddr) {
        if (remoteAddr != null) {
            failureRecords.remove(remoteAddr);
        }
    }

    // ─── Helpers ──────────────────────────────────────────────────────────

    /**
     * Resolved authentication configuration from config + environment.
     * Corresponds to TS ResolvedGatewayAuth.
     */
    public record ResolvedGatewayAuth(
            /** "token" or "password" */
            String mode,
            /** Resolved token value (config or env) */
            String token,
            /** Resolved password value (config or env) */
            String password) {
        public boolean isConfigured() {
            return isNotBlank(token) || isNotBlank(password);
        }
    }

    /**
     * Result of an authorization attempt.
     * Corresponds to TS GatewayAuthResult.
     */
    public record GatewayAuthResult(
            boolean ok,
            /** "token" | "password" | "local" | null */
            String method,
            /** Reason code on failure (token_missing, token_mismatch, etc.) */
            String reason) {
        public static GatewayAuthResult success(String method) {
            return new GatewayAuthResult(true, method, null);
        }

        public static GatewayAuthResult failure(String reason) {
            return new GatewayAuthResult(false, null, reason);
        }
    }

    /**
     * Auth credentials provided by the connecting client.
     */
    public record ConnectAuth(String token, String password) {
    }

    // ─── Internal types ───────────────────────────────────────────────────

    private static class FailureRecord {
        int count;
        long lastFailureTime;
    }
}

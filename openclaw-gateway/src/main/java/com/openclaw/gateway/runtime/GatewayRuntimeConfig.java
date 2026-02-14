package com.openclaw.gateway.runtime;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * Resolved gateway runtime configuration snapshot.
 * Corresponds to TS {@code server-runtime-config.ts} — resolves bind host,
 * auth mode, control UI, hooks config, etc. from the loaded config.
 */
@Getter
@Builder
@Slf4j
public class GatewayRuntimeConfig {

    private final String bindHost;
    private final int port;
    private final boolean controlUiEnabled;
    private final boolean openAiChatCompletionsEnabled;
    private final boolean openResponsesEnabled;
    private final String controlUiBasePath;
    private final String authMode; // "none"|"token"|"password"
    private final String authToken;
    private final String authPassword;
    private final boolean canvasHostEnabled;
    private final String tailscaleMode; // "off"|"serve"|"funnel"

    /**
     * Resolve the gateway runtime configuration from the loaded config.
     */
    public static GatewayRuntimeConfig resolve(ConfigService configService, int port) {
        OpenClawConfig cfg = configService.loadConfig();
        OpenClawConfig.GatewayConfig gw = cfg.getGateway();

        // Bind host
        String bindMode = "loopback";
        String bindHost = "127.0.0.1";
        if (gw != null) {
            if (gw.getBind() != null && !gw.getBind().isEmpty())
                bindMode = gw.getBind();
            if (gw.getCustomBindHost() != null && !gw.getCustomBindHost().isEmpty()) {
                bindHost = gw.getCustomBindHost();
            } else if ("all".equals(bindMode) || "lan".equals(bindMode)) {
                bindHost = "0.0.0.0";
            }
        }

        // Control UI
        boolean controlUiEnabled = true;
        String controlUiBasePath = "/ui";
        if (gw != null && gw.getControlUi() != null) {
            var cui = gw.getControlUi();
            if (cui.getEnabled() != null)
                controlUiEnabled = cui.getEnabled();
            if (cui.getBasePath() != null && !cui.getBasePath().trim().isEmpty()) {
                controlUiBasePath = normalizeBasePath(cui.getBasePath());
            }
        }

        // HTTP endpoints
        boolean chatCompletionsEnabled = false;
        boolean responsesEnabled = false;
        if (gw != null && gw.getHttp() != null && gw.getHttp().getEndpoints() != null) {
            var endpoints = gw.getHttp().getEndpoints();
            if (endpoints.getChatCompletions() != null && endpoints.getChatCompletions().getEnabled() != null)
                chatCompletionsEnabled = endpoints.getChatCompletions().getEnabled();
            if (endpoints.getResponses() != null && endpoints.getResponses().getEnabled() != null)
                responsesEnabled = endpoints.getResponses().getEnabled();
        }

        // Auth
        String authToken = resolveEnvOrConfig("OPENCLAW_GATEWAY_TOKEN",
                gw != null && gw.getAuth() != null ? gw.getAuth().getToken() : null);
        String authPassword = resolveEnvOrConfig("OPENCLAW_GATEWAY_PASSWORD",
                gw != null && gw.getAuth() != null ? gw.getAuth().getPassword() : null);
        String authMode = "none";
        if (authToken != null)
            authMode = "token";
        else if (authPassword != null)
            authMode = "password";

        // Tailscale
        String tailscaleMode = "off";
        if (gw != null && gw.getTailscale() != null && gw.getTailscale().getMode() != null) {
            tailscaleMode = gw.getTailscale().getMode();
        }

        // Canvas host
        boolean canvasHostEnabled = !"1".equals(System.getenv("OPENCLAW_SKIP_CANVAS_HOST"));

        // Validation
        boolean isLoopback = "127.0.0.1".equals(bindHost) || "localhost".equals(bindHost)
                || "::1".equals(bindHost);
        if (!isLoopback && authToken == null && authPassword == null) {
            log.warn("Gateway binding to {} without auth — set OPENCLAW_GATEWAY_TOKEN", bindHost);
        }
        if ("funnel".equals(tailscaleMode) && !"password".equals(authMode)) {
            throw new IllegalStateException(
                    "tailscale funnel requires auth mode=password");
        }

        return GatewayRuntimeConfig.builder()
                .bindHost(bindHost).port(port)
                .controlUiEnabled(controlUiEnabled)
                .openAiChatCompletionsEnabled(chatCompletionsEnabled)
                .openResponsesEnabled(responsesEnabled)
                .controlUiBasePath(controlUiBasePath)
                .authMode(authMode).authToken(authToken).authPassword(authPassword)
                .canvasHostEnabled(canvasHostEnabled)
                .tailscaleMode(tailscaleMode)
                .build();
    }

    // ---- helpers ----

    private static String normalizeBasePath(String path) {
        if (path == null)
            return "/ui";
        String t = path.trim();
        if (t.isEmpty())
            return "/ui";
        if (!t.startsWith("/"))
            t = "/" + t;
        if (t.endsWith("/") && t.length() > 1)
            t = t.substring(0, t.length() - 1);
        return t;
    }

    private static String resolveEnvOrConfig(String envKey, String configVal) {
        String env = System.getenv(envKey);
        if (env != null && !env.trim().isEmpty())
            return env.trim();
        if (configVal != null && !configVal.trim().isEmpty())
            return configVal.trim();
        return null;
    }
}

package com.openclaw.browser;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Browser configuration resolver — resolves raw config into typed,
 * validated browser configuration with profile support.
 * Corresponds to TypeScript's browser/config.ts.
 */
@Slf4j
public class BrowserConfig {

    // ==================== Resolved Config Types ====================

    /**
     * Fully resolved browser configuration.
     */
    @Data
    @Builder
    public static class ResolvedBrowserConfig {
        private boolean enabled;
        private boolean evaluateEnabled;
        private int controlPort;
        private String cdpProtocol;
        private String cdpHost;
        private boolean cdpIsLoopback;
        private int remoteCdpTimeoutMs;
        private int remoteCdpHandshakeTimeoutMs;
        private String color;
        private String executablePath;
        private boolean headless;
        private boolean noSandbox;
        private boolean attachOnly;
        private String defaultProfile;
        @Builder.Default
        private Map<String, ResolvedBrowserProfile> profiles = new LinkedHashMap<>();
    }

    /**
     * Resolved profile configuration.
     */
    @Data
    @Builder
    public static class ResolvedBrowserProfile {
        private String name;
        private int cdpPort;
        private String cdpUrl;
        private String cdpHost;
        private boolean cdpIsLoopback;
        private String color;
        @Builder.Default
        private String driver = "openclaw";
    }

    // ==================== Resolution ====================

    /**
     * Resolve browser config from the raw OpenClawConfig.
     */
    public static ResolvedBrowserConfig resolve(OpenClawConfig config) {
        OpenClawConfig.BrowserConfig raw = (config != null) ? config.getBrowser() : null;

        boolean enabled = BrowserConstants.DEFAULT_BROWSER_ENABLED;
        boolean evaluateEnabled = BrowserConstants.DEFAULT_EVALUATE_ENABLED;
        int controlPort = BrowserConstants.DEFAULT_CONTROL_PORT;
        String cdpProtocol = BrowserConstants.DEFAULT_CDP_PROTOCOL;
        String cdpHost = BrowserConstants.DEFAULT_CDP_HOST;
        int remoteCdpTimeoutMs = BrowserConstants.DEFAULT_REMOTE_CDP_TIMEOUT_MS;
        int remoteCdpHandshakeTimeoutMs = BrowserConstants.DEFAULT_REMOTE_CDP_HANDSHAKE_TIMEOUT_MS;
        String color = BrowserConstants.DEFAULT_PROFILE_COLOR;
        String executablePath = null;
        boolean headless = false;
        boolean noSandbox = false;
        boolean attachOnly = false;
        String defaultProfile = BrowserConstants.DEFAULT_PROFILE_NAME;

        if (raw != null) {
            if (raw.getEnabled() != null) enabled = raw.getEnabled();
            if (raw.getEvaluateEnabled() != null) evaluateEnabled = raw.getEvaluateEnabled();
            if (raw.getControlPort() != null) controlPort = raw.getControlPort();
            // Parse cdpUrl to extract protocol and host
            if (raw.getCdpUrl() != null && !raw.getCdpUrl().isBlank()) {
                String[] parsed = parseCdpUrl(raw.getCdpUrl());
                cdpProtocol = parsed[0];
                cdpHost = parsed[1];
            }
            if (raw.getRemoteCdpTimeoutMs() != null)
                remoteCdpTimeoutMs = normalizeTimeoutMs(raw.getRemoteCdpTimeoutMs(),
                        BrowserConstants.DEFAULT_REMOTE_CDP_TIMEOUT_MS);
            if (raw.getRemoteCdpHandshakeTimeoutMs() != null)
                remoteCdpHandshakeTimeoutMs = normalizeTimeoutMs(raw.getRemoteCdpHandshakeTimeoutMs(),
                        BrowserConstants.DEFAULT_REMOTE_CDP_HANDSHAKE_TIMEOUT_MS);
            if (raw.getColor() != null) color = normalizeHexColor(raw.getColor());
            if (raw.getExecutablePath() != null) executablePath = raw.getExecutablePath();
            if (raw.getHeadless() != null) headless = raw.getHeadless();
            if (raw.getNoSandbox() != null) noSandbox = raw.getNoSandbox();
            if (raw.getAttachOnly() != null) attachOnly = raw.getAttachOnly();
            if (raw.getDefaultProfile() != null) defaultProfile = raw.getDefaultProfile();
        }

        boolean cdpIsLoopback = isLoopbackHost(cdpHost);

        // Build profiles
        Map<String, ResolvedBrowserProfile> profiles = new LinkedHashMap<>();

        // Ensure a default profile exists
        int defaultCdpPort = BrowserConstants.DEFAULT_CDP_PORT_RANGE_START;
        profiles.put(defaultProfile, ResolvedBrowserProfile.builder()
                .name(defaultProfile)
                .cdpPort(defaultCdpPort)
                .cdpUrl(cdpProtocol + "://" + cdpHost + ":" + defaultCdpPort)
                .cdpHost(cdpHost)
                .cdpIsLoopback(cdpIsLoopback)
                .color(color)
                .driver("openclaw")
                .build());

        // Add user-defined profiles from config
        if (raw != null && raw.getProfiles() != null) {
            int portOffset = 1;
            for (var entry : raw.getProfiles().entrySet()) {
                String profileName = entry.getKey();
                if (profiles.containsKey(profileName)) continue; // don't override default

                var profileConfig = entry.getValue();
                int profilePort = profileConfig.getCdpPort() != null
                        ? profileConfig.getCdpPort() : defaultCdpPort + portOffset++;
                String profileCdpHost = cdpHost;
                String profileCdpProtocol = cdpProtocol;
                // Override from profile's cdpUrl if present
                if (profileConfig.getCdpUrl() != null && !profileConfig.getCdpUrl().isBlank()) {
                    String[] parsed = parseCdpUrl(profileConfig.getCdpUrl());
                    profileCdpProtocol = parsed[0];
                    profileCdpHost = parsed[1];
                    if (!parsed[2].isEmpty()) {
                        try { profilePort = Integer.parseInt(parsed[2]); }
                        catch (NumberFormatException ignored) { /* keep default */ }
                    }
                }
                String profileColor = profileConfig.getColor() != null
                        ? normalizeHexColor(profileConfig.getColor()) : color;
                String profileDriver = profileConfig.getDriver() != null
                        ? profileConfig.getDriver() : "openclaw";

                profiles.put(profileName, ResolvedBrowserProfile.builder()
                        .name(profileName)
                        .cdpPort(profilePort)
                        .cdpUrl(profileCdpProtocol + "://" + profileCdpHost + ":" + profilePort)
                        .cdpHost(profileCdpHost)
                        .cdpIsLoopback(isLoopbackHost(profileCdpHost))
                        .color(profileColor)
                        .driver(profileDriver)
                        .build());
            }
        }

        return ResolvedBrowserConfig.builder()
                .enabled(enabled)
                .evaluateEnabled(evaluateEnabled)
                .controlPort(controlPort)
                .cdpProtocol(cdpProtocol)
                .cdpHost(cdpHost)
                .cdpIsLoopback(cdpIsLoopback)
                .remoteCdpTimeoutMs(remoteCdpTimeoutMs)
                .remoteCdpHandshakeTimeoutMs(remoteCdpHandshakeTimeoutMs)
                .color(color)
                .executablePath(executablePath)
                .headless(headless)
                .noSandbox(noSandbox)
                .attachOnly(attachOnly)
                .defaultProfile(defaultProfile)
                .profiles(profiles)
                .build();
    }

    /**
     * Resolve a profile by name.
     */
    public static ResolvedBrowserProfile resolveProfile(ResolvedBrowserConfig resolved,
                                                         String profileName) {
        if (profileName == null || profileName.isBlank()) {
            return resolved.getProfiles().get(resolved.getDefaultProfile());
        }
        return resolved.getProfiles().get(profileName.trim());
    }

    /**
     * Whether the local browser server should be started.
     */
    public static boolean shouldStartLocalServer(ResolvedBrowserConfig resolved) {
        return resolved.isEnabled();
    }

    // ==================== Helpers ====================

    /**
     * Check if a host string is a loopback address.
     */
    public static boolean isLoopbackHost(String host) {
        if (host == null) return false;
        String h = host.trim().toLowerCase();
        return "localhost".equals(h)
                || "127.0.0.1".equals(h)
                || "::1".equals(h)
                || h.equals("[::1]");
    }

    /**
     * Normalize a hex color string (ensure it starts with #).
     */
    public static String normalizeHexColor(String raw) {
        if (raw == null || raw.isBlank()) {
            return BrowserConstants.DEFAULT_PROFILE_COLOR;
        }
        String trimmed = raw.trim();
        if (!trimmed.startsWith("#")) {
            trimmed = "#" + trimmed;
        }
        return trimmed;
    }

    /**
     * Normalize a timeout value; returns fallback if invalid.
     */
    public static int normalizeTimeoutMs(Integer raw, int fallback) {
        if (raw == null || raw < 0) return fallback;
        return raw;
    }

    /**
     * Parse a CDP URL string like "http://127.0.0.1:9222" into
     * [protocol, host, portString]. Returns defaults for missing parts.
     */
    static String[] parseCdpUrl(String cdpUrl) {
        try {
            java.net.URI uri = java.net.URI.create(cdpUrl.trim());
            String scheme = uri.getScheme() != null ? uri.getScheme() : BrowserConstants.DEFAULT_CDP_PROTOCOL;
            String host = uri.getHost() != null ? uri.getHost() : BrowserConstants.DEFAULT_CDP_HOST;
            String port = uri.getPort() > 0 ? String.valueOf(uri.getPort()) : "";
            return new String[] { scheme, host, port };
        } catch (Exception e) {
            log.warn("Failed to parse CDP URL '{}': {}", cdpUrl, e.getMessage());
            return new String[] { BrowserConstants.DEFAULT_CDP_PROTOCOL, BrowserConstants.DEFAULT_CDP_HOST, "" };
        }
    }
}

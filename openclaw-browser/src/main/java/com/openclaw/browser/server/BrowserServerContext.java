package com.openclaw.browser.server;

import com.openclaw.browser.BrowserConfig;
import com.openclaw.browser.chrome.ChromeManager;
import com.openclaw.browser.chrome.RunningChrome;
import com.openclaw.browser.playwright.PlaywrightSession;
import com.openclaw.browser.relay.ExtensionRelayManager;
import com.openclaw.browser.relay.ExtensionRelayServer;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.ConcurrentHashMap;

/**
 * Browser server context — manages profiles, Chrome instances, Playwright sessions, and relay servers.
 * Corresponds to TypeScript's server-context.ts.
 */
@Slf4j
public class BrowserServerContext implements AutoCloseable {

    @Getter private final BrowserConfig.ResolvedBrowserConfig config;

    /** Profile name → Chrome instance */
    private final ConcurrentHashMap<String, RunningChrome> chromeInstances = new ConcurrentHashMap<>();
    /** Profile name → Playwright session */
    private final ConcurrentHashMap<String, PlaywrightSession> playwrightSessions = new ConcurrentHashMap<>();
    /** Profile name → Extension Relay */
    private final ConcurrentHashMap<String, ExtensionRelayServer> relayServers = new ConcurrentHashMap<>();

    public BrowserServerContext(BrowserConfig.ResolvedBrowserConfig config) {
        this.config = config;
    }

    // ==================== Chrome Management ====================

    /**
     * Get or launch Chrome for a profile.
     */
    public RunningChrome ensureChrome(String profileName) throws Exception {
        BrowserConfig.ResolvedBrowserProfile profile = findProfile(profileName);
        if (profile == null) {
            throw new IllegalArgumentException("Profile not found: " + profileName);
        }

        return chromeInstances.computeIfAbsent(profileName, key -> {
            try {
                return ChromeManager.launchChrome(config, profile);
            } catch (Exception e) {
                throw new RuntimeException("Failed to launch Chrome for profile: " + key, e);
            }
        });
    }

    /**
     * Stop Chrome for a profile.
     */
    public void stopChrome(String profileName) {
        RunningChrome chrome = chromeInstances.remove(profileName);
        if (chrome != null) {
            ChromeManager.stopChrome(chrome);
        }
    }

    /**
     * Check if Chrome is running for a profile.
     */
    public boolean isChromeRunning(String profileName) {
        RunningChrome chrome = chromeInstances.get(profileName);
        return chrome != null && chrome.isAlive();
    }

    // ==================== Playwright Management ====================

    /**
     * Get or create a Playwright session for a profile.
     */
    public PlaywrightSession ensurePlaywrightSession(String profileName) throws Exception {
        BrowserConfig.ResolvedBrowserProfile profile = findProfile(profileName);
        if (profile == null) {
            throw new IllegalArgumentException("Profile not found: " + profileName);
        }

        PlaywrightSession existing = playwrightSessions.get(profileName);
        if (existing != null && existing.isConnected()) {
            return existing;
        }

        PlaywrightSession session = new PlaywrightSession(config);
        session.connect(profile);
        playwrightSessions.put(profileName, session);
        return session;
    }

    /**
     * Close Playwright session for a profile.
     */
    public void closePlaywrightSession(String profileName) {
        PlaywrightSession session = playwrightSessions.remove(profileName);
        if (session != null) {
            session.close();
        }
    }

    // ==================== Extension Relay ====================

    /**
     * Ensure relay server is running for a profile.
     */
    public ExtensionRelayServer ensureRelay(String profileName) throws Exception {
        BrowserConfig.ResolvedBrowserProfile profile = findProfile(profileName);
        if (profile == null) {
            throw new IllegalArgumentException("Profile not found: " + profileName);
        }

        return relayServers.computeIfAbsent(profileName, key -> {
            try {
                return ExtensionRelayManager.ensureRelayServer(profile.getCdpUrl());
            } catch (Exception e) {
                throw new RuntimeException("Failed to start relay for profile: " + key, e);
            }
        });
    }

    /**
     * Stop relay server for a profile.
     */
    public void stopRelay(String profileName) {
        ExtensionRelayServer relay = relayServers.remove(profileName);
        if (relay != null) {
            relay.stop();
        }
    }

    // ==================== Profile lookup ====================

    /**
     * Find a resolved profile by name.
     */
    public BrowserConfig.ResolvedBrowserProfile findProfile(String profileName) {
        if (config.getProfiles() == null) return config.getDefaultProfileObj();
        BrowserConfig.ResolvedBrowserProfile found = config.getProfiles().get(profileName);
        return found != null ? found : config.getDefaultProfileObj();
    }

    /**
     * Get the default profile name.
     */
    public String getDefaultProfileName() {
        BrowserConfig.ResolvedBrowserProfile def = config.getDefaultProfileObj();
        return def != null ? def.getName() : "default";
    }

    // ==================== Health Checks ====================

    /**
     * Check if the Chrome instance for a profile is reachable via WebSocket.
     */
    public boolean isReachable(String profileName, int timeoutMs) {
        RunningChrome chrome = chromeInstances.get(profileName);
        if (chrome == null || !chrome.isAlive()) return false;
        String cdpUrl = chrome.getCdpUrl();
        if (cdpUrl == null) return false;
        // Try to connect via HTTP to the /json/version endpoint
        return isHttpReachable(profileName, timeoutMs);
    }

    /**
     * Check if Chrome is reachable via HTTP to the debugger endpoint.
     */
    public boolean isHttpReachable(String profileName, int timeoutMs) {
        RunningChrome chrome = chromeInstances.get(profileName);
        if (chrome == null) return false;
        String cdpUrl = chrome.getCdpUrl();
        if (cdpUrl == null) return false;
        try {
            // Extract host/port from ws:// URL
            String httpUrl = cdpUrl.replace("ws://", "http://")
                    .replaceAll("/devtools.*", "/json/version");
            java.net.URL url = java.net.URI.create(httpUrl).toURL();
            java.net.HttpURLConnection conn = (java.net.HttpURLConnection) url.openConnection();
            conn.setConnectTimeout(timeoutMs > 0 ? timeoutMs : 1500);
            conn.setReadTimeout(timeoutMs > 0 ? timeoutMs : 1500);
            conn.setRequestMethod("GET");
            int status = conn.getResponseCode();
            conn.disconnect();
            return status == 200;
        } catch (Exception e) {
            return false;
        }
    }

    // ==================== Profile Status ====================

    /**
     * Profile runtime status for listing.
     */
    public record ProfileStatus(
            String name,
            boolean running,
            String cdpUrl,
            boolean connected
    ) {}

    /**
     * List all profiles with their runtime status.
     */
    public java.util.List<ProfileStatus> listProfiles() {
        java.util.List<ProfileStatus> result = new java.util.ArrayList<>();
        if (config.getProfiles() != null) {
            for (var entry : config.getProfiles().entrySet()) {
                String name = entry.getKey();
                result.add(getProfileStatus(name));
            }
        } else {
            // Only default profile
            result.add(getProfileStatus(getDefaultProfileName()));
        }
        return result;
    }

    /**
     * Get the runtime status of a specific profile.
     */
    public ProfileStatus getProfileStatus(String profileName) {
        RunningChrome chrome = chromeInstances.get(profileName);
        PlaywrightSession session = playwrightSessions.get(profileName);
        boolean running = chrome != null && chrome.isAlive();
        String cdpUrl = chrome != null ? chrome.getCdpUrl() : null;
        boolean connected = session != null && session.isConnected();
        return new ProfileStatus(profileName, running, cdpUrl, connected);
    }

    // ==================== Reset ====================

    /**
     * Reset a profile: stop Chrome, close Playwright session, and optionally clean profile data.
     */
    public void resetProfile(String profileName, boolean cleanData) throws Exception {
        // Close Playwright first
        closePlaywrightSession(profileName);
        // Stop relay
        stopRelay(profileName);
        // Stop Chrome
        stopChrome(profileName);
        // Optionally clean profile data directory
        if (cleanData) {
            RunningChrome chrome = chromeInstances.get(profileName);
            if (chrome != null) {
                String userDataDir = chrome.getUserDataDir();
                if (userDataDir != null) {
                    java.nio.file.Path profileDir = java.nio.file.Path.of(userDataDir);
                    if (java.nio.file.Files.exists(profileDir)) {
                        log.info("Cleaning profile data: {}", profileDir);
                        try (var stream = java.nio.file.Files.walk(profileDir)) {
                            stream.sorted(java.util.Comparator.reverseOrder())
                                    .filter(p -> !p.equals(profileDir))
                                    .forEach(p -> {
                                        try { java.nio.file.Files.deleteIfExists(p); } catch (Exception ignored) {}
                                    });
                        }
                    }
                }
            }
        }
        log.info("Profile '{}' reset (cleanData={})", profileName, cleanData);
    }

    // ==================== Lifecycle ====================

    @Override
    public void close() {
        // Close Playwright sessions
        for (PlaywrightSession session : playwrightSessions.values()) {
            try { session.close(); } catch (Exception e) { /* ignore */ }
        }
        playwrightSessions.clear();

        // Stop Chrome instances
        for (RunningChrome chrome : chromeInstances.values()) {
            try { ChromeManager.stopChrome(chrome); } catch (Exception e) { /* ignore */ }
        }
        chromeInstances.clear();

        // Stop relay servers
        for (ExtensionRelayServer relay : relayServers.values()) {
            try { relay.stop(); } catch (Exception e) { /* ignore */ }
        }
        relayServers.clear();

        log.info("BrowserServerContext closed");
    }
}

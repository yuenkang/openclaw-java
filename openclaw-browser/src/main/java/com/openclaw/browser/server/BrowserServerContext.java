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

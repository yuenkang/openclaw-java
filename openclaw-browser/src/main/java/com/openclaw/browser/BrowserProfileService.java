package com.openclaw.browser;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.config.OpenClawConfig.BrowserConfig;
import com.openclaw.common.config.OpenClawConfig.BrowserProfileConfig;
import com.openclaw.common.config.PortDefaults;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * Service for managing browser profiles: CRUD operations and per-profile
 * PlaywrightSession lifecycle.
 * <p>
 * Corresponds to TypeScript's browser/profiles-service.ts.
 * </p>
 */
@Slf4j
@Component
public class BrowserProfileService {

    private static final Pattern HEX_COLOR_RE = Pattern.compile("^#[0-9A-Fa-f]{6}$");

    private final ConfigService configService;
    private final Map<String, PlaywrightSession> sessions = new ConcurrentHashMap<>();

    public BrowserProfileService(ConfigService configService) {
        this.configService = configService;
    }

    // =========================================================================
    // Data classes
    // =========================================================================

    @Data
    public static class ProfileStatus {
        private final String name;
        private final Integer cdpPort;
        private final String cdpUrl;
        private final String color;
        private final boolean running;
        private final boolean isDefault;
    }

    @Data
    public static class CreateProfileResult {
        private final boolean ok;
        private final String profile;
        private final Integer cdpPort;
        private final String cdpUrl;
        private final String color;
    }

    @Data
    public static class DeleteProfileResult {
        private final boolean ok;
        private final String profile;
        private final boolean stopped;
    }

    // =========================================================================
    // Profile CRUD
    // =========================================================================

    /**
     * List all configured profiles with their running status.
     */
    public List<ProfileStatus> listProfiles() {
        OpenClawConfig config = configService.loadConfig();
        BrowserConfig browserConfig = config.getBrowser();
        String defaultProfile = resolveDefaultProfile(browserConfig);

        Map<String, BrowserProfileConfig> profiles = getProfilesMap(browserConfig);

        List<ProfileStatus> result = new ArrayList<>();

        // Always include the default profile even if not explicitly configured
        if (!profiles.containsKey(defaultProfile)) {
            PlaywrightSession session = sessions.get(defaultProfile);
            result.add(new ProfileStatus(
                    defaultProfile, null, null,
                    BrowserProfiles.PROFILE_COLORS.get(0),
                    session != null && session.isRunning(),
                    true));
        }

        for (Map.Entry<String, BrowserProfileConfig> entry : profiles.entrySet()) {
            String name = entry.getKey();
            BrowserProfileConfig cfg = entry.getValue();
            PlaywrightSession session = sessions.get(name);
            result.add(new ProfileStatus(
                    name,
                    cfg.getCdpPort(),
                    cfg.getCdpUrl(),
                    cfg.getColor() != null ? cfg.getColor() : BrowserProfiles.PROFILE_COLORS.get(0),
                    session != null && session.isRunning(),
                    name.equals(defaultProfile)));
        }

        return result;
    }

    /**
     * Create a new browser profile and persist to config.
     */
    public CreateProfileResult createProfile(String nameRaw, String color, String cdpUrl) throws IOException {
        String name = nameRaw.trim();

        if (!BrowserProfiles.isValidProfileName(name)) {
            throw new IllegalArgumentException(
                    "invalid profile name: use lowercase letters, numbers, and hyphens only");
        }

        OpenClawConfig config = configService.loadConfig();
        BrowserConfig browserConfig = ensureBrowserConfig(config);
        Map<String, BrowserProfileConfig> profiles = getProfilesMap(browserConfig);

        if (profiles.containsKey(name)) {
            throw new IllegalArgumentException("profile \"" + name + "\" already exists");
        }

        // Allocate color
        Set<String> usedColors = BrowserProfiles.getUsedColors(profiles);
        String profileColor = (color != null && HEX_COLOR_RE.matcher(color).matches())
                ? color
                : BrowserProfiles.allocateColor(usedColors);

        // Build profile config
        BrowserProfileConfig profileConfig = new BrowserProfileConfig();
        profileConfig.setColor(profileColor);

        Integer allocatedPort = null;
        String resolvedCdpUrl = null;

        if (cdpUrl != null && !cdpUrl.isBlank()) {
            profileConfig.setCdpUrl(cdpUrl.trim());
            resolvedCdpUrl = cdpUrl.trim();
        } else {
            Set<Integer> usedPorts = BrowserProfiles.getUsedPorts(profiles);
            int controlPort = browserConfig.getControlPort() != null
                    ? browserConfig.getControlPort()
                    : PortDefaults.DEFAULT_BROWSER_CONTROL_PORT;
            PortDefaults.PortRange range = PortDefaults.deriveDefaultBrowserCdpPortRange(controlPort);
            Integer cdpPort = BrowserProfiles.allocateCdpPort(usedPorts, range);
            if (cdpPort == null) {
                throw new IllegalStateException("no available CDP ports in range");
            }
            profileConfig.setCdpPort(cdpPort);
            allocatedPort = cdpPort;
        }

        // Persist to config
        Map<String, BrowserProfileConfig> newProfiles = new LinkedHashMap<>(profiles);
        newProfiles.put(name, profileConfig);
        browserConfig.setProfiles(newProfiles);
        configService.saveConfig(config);

        log.info("Created browser profile: {} (port={}, cdpUrl={}, color={})",
                name, allocatedPort, resolvedCdpUrl, profileColor);

        return new CreateProfileResult(true, name, allocatedPort, resolvedCdpUrl, profileColor);
    }

    /**
     * Delete a browser profile. Stops its browser session if running.
     */
    public DeleteProfileResult deleteProfile(String nameRaw) throws IOException {
        String name = nameRaw.trim();
        if (name.isEmpty()) {
            throw new IllegalArgumentException("profile name is required");
        }
        if (!BrowserProfiles.isValidProfileName(name)) {
            throw new IllegalArgumentException("invalid profile name");
        }

        OpenClawConfig config = configService.loadConfig();
        BrowserConfig browserConfig = config.getBrowser();
        if (browserConfig == null) {
            throw new IllegalArgumentException("profile \"" + name + "\" not found");
        }

        Map<String, BrowserProfileConfig> profiles = getProfilesMap(browserConfig);
        if (!profiles.containsKey(name)) {
            throw new IllegalArgumentException("profile \"" + name + "\" not found");
        }

        String defaultProfile = resolveDefaultProfile(browserConfig);
        if (name.equals(defaultProfile)) {
            throw new IllegalArgumentException(
                    "cannot delete the default profile \"" + name
                            + "\"; change browser.defaultProfile first");
        }

        // Stop running session if any
        boolean stopped = false;
        PlaywrightSession session = sessions.remove(name);
        if (session != null) {
            try {
                session.stop();
                stopped = true;
            } catch (Exception e) {
                log.debug("Error stopping session for profile {}: {}", name, e.getMessage());
            }
        }

        // Remove from config
        Map<String, BrowserProfileConfig> newProfiles = new LinkedHashMap<>(profiles);
        newProfiles.remove(name);
        browserConfig.setProfiles(newProfiles);
        configService.saveConfig(config);

        log.info("Deleted browser profile: {} (stopped={})", name, stopped);

        return new DeleteProfileResult(true, name, stopped);
    }

    // =========================================================================
    // Session management
    // =========================================================================

    /**
     * Get or create a PlaywrightSession for the given profile.
     * If profileName is null, uses the default profile.
     */
    public PlaywrightSession getOrCreateSession(String profileName) {
        String name = resolveProfileName(profileName);
        return sessions.computeIfAbsent(name, k -> new PlaywrightSession());
    }

    /**
     * Stop a profile's browser session.
     *
     * @return true if a session was stopped
     */
    public boolean stopSession(String profileName) {
        String name = resolveProfileName(profileName);
        PlaywrightSession session = sessions.get(name);
        if (session != null) {
            return session.stop();
        }
        return false;
    }

    /**
     * Check if a profile has a running browser session.
     */
    public boolean isRunning(String profileName) {
        String name = resolveProfileName(profileName);
        PlaywrightSession session = sessions.get(name);
        return session != null && session.isRunning();
    }

    /**
     * Resolve the effective profile name (null → default profile).
     */
    public String resolveProfileName(String profileName) {
        if (profileName != null && !profileName.isBlank()) {
            return profileName.trim();
        }
        OpenClawConfig config = configService.loadConfig();
        return resolveDefaultProfile(config.getBrowser());
    }

    /**
     * Stop all running browser sessions. Called on shutdown.
     */
    public void stopAll() {
        for (Map.Entry<String, PlaywrightSession> entry : sessions.entrySet()) {
            try {
                entry.getValue().close();
            } catch (Exception e) {
                log.debug("Error closing session {}: {}", entry.getKey(), e.getMessage());
            }
        }
        sessions.clear();
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String resolveDefaultProfile(BrowserConfig browserConfig) {
        if (browserConfig != null && browserConfig.getDefaultProfile() != null
                && !browserConfig.getDefaultProfile().isBlank()) {
            return browserConfig.getDefaultProfile();
        }
        return BrowserProfiles.DEFAULT_PROFILE_NAME;
    }

    private static Map<String, BrowserProfileConfig> getProfilesMap(BrowserConfig browserConfig) {
        if (browserConfig == null || browserConfig.getProfiles() == null) {
            return new LinkedHashMap<>();
        }
        return browserConfig.getProfiles();
    }

    private static BrowserConfig ensureBrowserConfig(OpenClawConfig config) {
        BrowserConfig bc = config.getBrowser();
        if (bc == null) {
            bc = new BrowserConfig();
            config.setBrowser(bc);
        }
        return bc;
    }
}

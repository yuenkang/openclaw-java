package com.openclaw.agent.tools.builtin.browser;

import com.openclaw.common.config.OpenClawConfig.BrowserProfileConfig;
import com.openclaw.common.config.PortDefaults;

import java.net.URI;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Browser profile utilities: name validation, CDP port allocation, color
 * allocation.
 * Corresponds to TypeScript's browser/profiles.ts.
 */
public final class BrowserProfiles {

    private BrowserProfiles() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    private static final Pattern PROFILE_NAME_REGEX = Pattern.compile("^[a-z0-9][a-z0-9-]*$");
    private static final int MAX_PROFILE_NAME_LENGTH = 64;

    /**
     * Pre-defined accent color palette (matching TypeScript).
     */
    public static final List<String> PROFILE_COLORS = List.of(
            "#FF4500", // Orange-red (openclaw default)
            "#0066CC", // Blue
            "#00AA00", // Green
            "#9933FF", // Purple
            "#FF6699", // Pink
            "#00CCCC", // Cyan
            "#FF9900", // Orange
            "#6666FF", // Indigo
            "#CC3366", // Magenta
            "#339966" // Teal
    );

    public static final String DEFAULT_PROFILE_NAME = "chrome";

    // =========================================================================
    // Name validation
    // =========================================================================

    /**
     * Check if a profile name is valid.
     * Only lowercase letters, digits, and hyphens; must start with letter or digit.
     */
    public static boolean isValidProfileName(String name) {
        if (name == null || name.isEmpty() || name.length() > MAX_PROFILE_NAME_LENGTH) {
            return false;
        }
        return PROFILE_NAME_REGEX.matcher(name).matches();
    }

    // =========================================================================
    // CDP Port allocation
    // =========================================================================

    /**
     * Allocate the first unused CDP port from the given range.
     *
     * @return allocated port, or null if all ports are in use
     */
    public static Integer allocateCdpPort(Set<Integer> usedPorts, PortDefaults.PortRange range) {
        if (range == null) {
            range = new PortDefaults.PortRange(
                    PortDefaults.DEFAULT_BROWSER_CDP_PORT_RANGE_START,
                    PortDefaults.DEFAULT_BROWSER_CDP_PORT_RANGE_END);
        }
        int start = range.start();
        int end = range.end();
        if (start <= 0 || end <= 0 || start > end) {
            return null;
        }
        for (int port = start; port <= end; port++) {
            if (!usedPorts.contains(port)) {
                return port;
            }
        }
        return null;
    }

    /**
     * Collect all CDP ports currently in use by existing profiles.
     * Extracts ports from both {@code cdpPort} fields and {@code cdpUrl} fields.
     */
    public static Set<Integer> getUsedPorts(Map<String, BrowserProfileConfig> profiles) {
        if (profiles == null || profiles.isEmpty()) {
            return new HashSet<>();
        }
        Set<Integer> used = new HashSet<>();
        for (BrowserProfileConfig profile : profiles.values()) {
            if (profile.getCdpPort() != null) {
                used.add(profile.getCdpPort());
                continue;
            }
            String rawUrl = profile.getCdpUrl();
            if (rawUrl == null || rawUrl.isBlank()) {
                continue;
            }
            try {
                URI uri = URI.create(rawUrl.trim());
                int port = uri.getPort();
                if (port > 0 && port <= 65535) {
                    used.add(port);
                } else if (port == -1) {
                    // Derive default port from scheme
                    String scheme = uri.getScheme();
                    if ("https".equals(scheme)) {
                        used.add(443);
                    } else if ("http".equals(scheme)) {
                        used.add(80);
                    }
                }
            } catch (Exception e) {
                // ignore invalid URLs
            }
        }
        return used;
    }

    // =========================================================================
    // Color allocation
    // =========================================================================

    /**
     * Allocate the first unused color from the palette.
     * If all colors are used, cycle based on count.
     */
    public static String allocateColor(Set<String> usedColors) {
        for (String color : PROFILE_COLORS) {
            if (!usedColors.contains(color.toUpperCase())) {
                return color;
            }
        }
        // All colors used — cycle
        int index = usedColors.size() % PROFILE_COLORS.size();
        return PROFILE_COLORS.get(index);
    }

    /**
     * Collect all colors currently in use by existing profiles.
     */
    public static Set<String> getUsedColors(Map<String, BrowserProfileConfig> profiles) {
        if (profiles == null || profiles.isEmpty()) {
            return new HashSet<>();
        }
        Set<String> colors = new HashSet<>();
        for (BrowserProfileConfig profile : profiles.values()) {
            if (profile.getColor() != null) {
                colors.add(profile.getColor().toUpperCase());
            }
        }
        return colors;
    }
}

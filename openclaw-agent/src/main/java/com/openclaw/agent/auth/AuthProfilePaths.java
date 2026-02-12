package com.openclaw.agent.auth;

import java.nio.file.Path;

/**
 * Auth store path resolution.
 * Corresponds to TypeScript auth-profiles/paths.ts.
 */
public final class AuthProfilePaths {

    private AuthProfilePaths() {
    }

    /**
     * Resolve the auth store file path.
     *
     * @param agentDir Agent data directory, or null for main agent
     * @return Path to auth-profiles.json
     */
    public static Path resolveAuthStorePath(String agentDir) {
        String base = (agentDir != null && !agentDir.isBlank())
                ? agentDir.trim()
                : resolveDefaultAgentDir();
        return Path.of(base, AuthProfileConstants.AUTH_PROFILE_FILENAME);
    }

    /**
     * Resolve the legacy auth store file path.
     */
    public static Path resolveLegacyAuthStorePath(String agentDir) {
        String base = (agentDir != null && !agentDir.isBlank())
                ? agentDir.trim()
                : resolveDefaultAgentDir();
        return Path.of(base, AuthProfileConstants.LEGACY_AUTH_FILENAME);
    }

    /**
     * Resolve auth store path for display (with ~ for home).
     */
    public static String resolveAuthStorePathForDisplay(String agentDir) {
        Path p = resolveAuthStorePath(agentDir);
        String home = System.getProperty("user.home");
        String pathStr = p.toString();
        if (home != null && pathStr.startsWith(home)) {
            return "~" + pathStr.substring(home.length());
        }
        return pathStr;
    }

    private static String resolveDefaultAgentDir() {
        return Path.of(System.getProperty("user.home"), ".openclaw").toString();
    }
}

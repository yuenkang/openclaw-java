package com.openclaw.common.config;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Configuration paths — state directory, config file, gateway lock dir, OAuth.
 * Corresponds to TypeScript's paths.ts + config-paths.ts.
 */
public final class ConfigPaths {

    private ConfigPaths() {
    }

    // =========================================================================
    // Directory / file name constants
    // =========================================================================

    private static final String[] LEGACY_STATE_DIRNAMES = { ".clawdbot", ".moltbot", ".moldbot" };
    private static final String NEW_STATE_DIRNAME = ".openclaw";
    private static final String CONFIG_FILENAME = "openclaw.json";
    private static final String[] LEGACY_CONFIG_FILENAMES = { "clawdbot.json", "moltbot.json", "moldbot.json" };
    public static final int DEFAULT_GATEWAY_PORT = 18789;
    private static final String OAUTH_FILENAME = "oauth.json";

    // =========================================================================
    // Nix mode detection
    // =========================================================================

    /**
     * Nix mode detection: When OPENCLAW_NIX_MODE=1, the gateway is running under
     * Nix.
     */
    public static boolean isNixMode() {
        return "1".equals(envTrimmed("OPENCLAW_NIX_MODE"));
    }

    // =========================================================================
    // State directory
    // =========================================================================

    /**
     * State directory for mutable data (sessions, logs, caches).
     * Can be overridden via OPENCLAW_STATE_DIR.
     * Default: ~/.openclaw
     */
    public static Path resolveStateDir() {
        return resolveStateDir(System.getenv(), homeDir());
    }

    public static Path resolveStateDir(Map<String, String> env, String homedir) {
        String override = firstNonBlank(envTrimmed(env, "OPENCLAW_STATE_DIR"),
                envTrimmed(env, "CLAWDBOT_STATE_DIR"));
        if (override != null) {
            return resolveUserPath(override);
        }
        Path newDir = Path.of(homedir, NEW_STATE_DIRNAME);
        if (Files.exists(newDir)) {
            return newDir;
        }
        for (String legacy : LEGACY_STATE_DIRNAMES) {
            Path legacyDir = Path.of(homedir, legacy);
            if (Files.exists(legacyDir)) {
                return legacyDir;
            }
        }
        return newDir;
    }

    // =========================================================================
    // Config file path
    // =========================================================================

    /**
     * Canonical config path (used for writes).
     */
    public static Path resolveCanonicalConfigPath() {
        return resolveCanonicalConfigPath(System.getenv(), resolveStateDir());
    }

    public static Path resolveCanonicalConfigPath(Map<String, String> env, Path stateDir) {
        String override = firstNonBlank(envTrimmed(env, "OPENCLAW_CONFIG_PATH"),
                envTrimmed(env, "CLAWDBOT_CONFIG_PATH"));
        if (override != null) {
            return resolveUserPath(override);
        }
        return stateDir.resolve(CONFIG_FILENAME);
    }

    /**
     * Active config path — prefers existing config files over canonical.
     */
    public static Path resolveConfigPath() {
        Map<String, String> env = System.getenv();
        String homedir = homeDir();
        return resolveConfigPath(env, resolveStateDir(env, homedir), homedir);
    }

    public static Path resolveConfigPath(Map<String, String> env, Path stateDir, String homedir) {
        String override = envTrimmed(env, "OPENCLAW_CONFIG_PATH");
        if (override != null) {
            return resolveUserPath(override);
        }
        String stateOverride = envTrimmed(env, "OPENCLAW_STATE_DIR");

        List<Path> candidates = new ArrayList<>();
        candidates.add(stateDir.resolve(CONFIG_FILENAME));
        for (String legacy : LEGACY_CONFIG_FILENAMES) {
            candidates.add(stateDir.resolve(legacy));
        }

        for (Path candidate : candidates) {
            if (Files.exists(candidate)) {
                return candidate;
            }
        }

        if (stateOverride != null) {
            return stateDir.resolve(CONFIG_FILENAME);
        }

        Path defaultStateDir = resolveStateDir(env, homedir);
        if (stateDir.toAbsolutePath().normalize().equals(defaultStateDir.toAbsolutePath().normalize())) {
            return resolveConfigPathCandidate(env, homedir);
        }
        return stateDir.resolve(CONFIG_FILENAME);
    }

    /**
     * Resolve the active config path from default candidates.
     */
    public static Path resolveConfigPathCandidate(Map<String, String> env, String homedir) {
        List<Path> candidates = resolveDefaultConfigCandidates(env, homedir);
        for (Path candidate : candidates) {
            if (Files.exists(candidate)) {
                return candidate;
            }
        }
        return resolveCanonicalConfigPath(env, resolveStateDir(env, homedir));
    }

    /**
     * Resolve default config path candidates across default locations.
     */
    public static List<Path> resolveDefaultConfigCandidates(Map<String, String> env, String homedir) {
        String explicit = firstNonBlank(envTrimmed(env, "OPENCLAW_CONFIG_PATH"),
                envTrimmed(env, "CLAWDBOT_CONFIG_PATH"));
        if (explicit != null) {
            return List.of(resolveUserPath(explicit));
        }

        List<Path> candidates = new ArrayList<>();
        String stateOverride = firstNonBlank(envTrimmed(env, "OPENCLAW_STATE_DIR"),
                envTrimmed(env, "CLAWDBOT_STATE_DIR"));
        if (stateOverride != null) {
            Path resolved = resolveUserPath(stateOverride);
            candidates.add(resolved.resolve(CONFIG_FILENAME));
            for (String legacy : LEGACY_CONFIG_FILENAMES) {
                candidates.add(resolved.resolve(legacy));
            }
        }

        List<String> defaultDirs = new ArrayList<>();
        defaultDirs.add(Path.of(homedir, NEW_STATE_DIRNAME).toString());
        for (String legacy : LEGACY_STATE_DIRNAMES) {
            defaultDirs.add(Path.of(homedir, legacy).toString());
        }

        for (String dir : defaultDirs) {
            candidates.add(Path.of(dir, CONFIG_FILENAME));
            for (String legacy : LEGACY_CONFIG_FILENAMES) {
                candidates.add(Path.of(dir, legacy));
            }
        }
        return candidates;
    }

    // =========================================================================
    // Gateway lock dir & port
    // =========================================================================

    /**
     * Gateway lock directory (ephemeral).
     */
    public static Path resolveGatewayLockDir() {
        String tmpdir = System.getProperty("java.io.tmpdir");
        // On Unix, use UID suffix for isolation
        String uid = System.getProperty("user.name", "openclaw");
        return Path.of(tmpdir, "openclaw-" + uid);
    }

    /**
     * Resolve gateway port from config / env.
     */
    public static int resolveGatewayPort(OpenClawConfig cfg) {
        return resolveGatewayPort(cfg, System.getenv());
    }

    public static int resolveGatewayPort(OpenClawConfig cfg, Map<String, String> env) {
        String envRaw = firstNonBlank(envTrimmed(env, "OPENCLAW_GATEWAY_PORT"),
                envTrimmed(env, "CLAWDBOT_GATEWAY_PORT"));
        if (envRaw != null) {
            try {
                int parsed = Integer.parseInt(envRaw);
                if (parsed > 0)
                    return parsed;
            } catch (NumberFormatException ignored) {
            }
        }
        if (cfg != null && cfg.getGateway() != null) {
            Integer configPort = cfg.getGateway().getPort();
            if (configPort != null && configPort > 0) {
                return configPort;
            }
        }
        return DEFAULT_GATEWAY_PORT;
    }

    // =========================================================================
    // OAuth
    // =========================================================================

    public static Path resolveOAuthDir() {
        return resolveOAuthDir(System.getenv(), resolveStateDir());
    }

    public static Path resolveOAuthDir(Map<String, String> env, Path stateDir) {
        String override = envTrimmed(env, "OPENCLAW_OAUTH_DIR");
        if (override != null) {
            return resolveUserPath(override);
        }
        return stateDir.resolve("credentials");
    }

    public static Path resolveOAuthPath(Map<String, String> env, Path stateDir) {
        return resolveOAuthDir(env, stateDir).resolve(OAUTH_FILENAME);
    }

    // =========================================================================
    // Dot-path config navigation (from config-paths.ts)
    // =========================================================================

    private static final java.util.Set<String> BLOCKED_KEYS = java.util.Set.of("__proto__", "prototype", "constructor");

    /**
     * Parse a dot-notation config path.
     */
    public static ParseResult parseConfigPath(String raw) {
        String trimmed = raw == null ? "" : raw.trim();
        if (trimmed.isEmpty()) {
            return ParseResult.error("Invalid path. Use dot notation (e.g. foo.bar).");
        }
        String[] parts = trimmed.split("\\.");
        for (String part : parts) {
            if (part.isBlank()) {
                return ParseResult.error("Invalid path. Use dot notation (e.g. foo.bar).");
            }
            if (BLOCKED_KEYS.contains(part)) {
                return ParseResult.error("Invalid path segment.");
            }
        }
        return ParseResult.ok(parts);
    }

    public record ParseResult(boolean ok, String[] path, String error) {
        static ParseResult ok(String[] path) {
            return new ParseResult(true, path, null);
        }

        static ParseResult error(String err) {
            return new ParseResult(false, null, err);
        }
    }

    /**
     * Set a value at a dot-path in a nested map structure.
     */
    @SuppressWarnings("unchecked")
    public static void setConfigValueAtPath(Map<String, Object> root, String[] path, Object value) {
        Map<String, Object> cursor = root;
        for (int i = 0; i < path.length - 1; i++) {
            Object next = cursor.get(path[i]);
            if (!(next instanceof Map)) {
                next = new java.util.LinkedHashMap<String, Object>();
                cursor.put(path[i], next);
            }
            cursor = (Map<String, Object>) next;
        }
        cursor.put(path[path.length - 1], value);
    }

    /**
     * Remove a value at a dot-path, pruning empty parent containers.
     */
    @SuppressWarnings("unchecked")
    public static boolean unsetConfigValueAtPath(Map<String, Object> root, String[] path) {
        record StackEntry(Map<String, Object> node, String key) {
        }
        List<StackEntry> stack = new ArrayList<>();
        Map<String, Object> cursor = root;
        for (int i = 0; i < path.length - 1; i++) {
            Object next = cursor.get(path[i]);
            if (!(next instanceof Map)) {
                return false;
            }
            stack.add(new StackEntry(cursor, path[i]));
            cursor = (Map<String, Object>) next;
        }
        String leafKey = path[path.length - 1];
        if (!cursor.containsKey(leafKey)) {
            return false;
        }
        cursor.remove(leafKey);
        // Prune empty parents
        for (int i = stack.size() - 1; i >= 0; i--) {
            StackEntry entry = stack.get(i);
            Object child = entry.node.get(entry.key);
            if (child instanceof Map<?, ?> map && map.isEmpty()) {
                entry.node.remove(entry.key);
            } else {
                break;
            }
        }
        return true;
    }

    /**
     * Get a value at a dot-path in a nested map structure.
     */
    @SuppressWarnings("unchecked")
    public static Object getConfigValueAtPath(Map<String, Object> root, String[] path) {
        Object cursor = root;
        for (String key : path) {
            if (!(cursor instanceof Map)) {
                return null;
            }
            cursor = ((Map<String, Object>) cursor).get(key);
        }
        return cursor;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Resolve a user path — expands ~ to home directory, resolves to absolute.
     */
    public static Path resolveUserPath(String input) {
        if (input == null)
            return Path.of("");
        String trimmed = input.trim();
        if (trimmed.isEmpty())
            return Path.of("");
        if (trimmed.startsWith("~")) {
            String expanded = trimmed.replaceFirst("^~", homeDir());
            return Path.of(expanded).toAbsolutePath().normalize();
        }
        return Path.of(trimmed).toAbsolutePath().normalize();
    }

    private static String homeDir() {
        return System.getProperty("user.home");
    }

    private static String envTrimmed(String key) {
        String val = System.getenv(key);
        return val != null && !val.trim().isEmpty() ? val.trim() : null;
    }

    private static String envTrimmed(Map<String, String> env, String key) {
        String val = env.get(key);
        return val != null && !val.trim().isEmpty() ? val.trim() : null;
    }

    private static String firstNonBlank(String... values) {
        for (String v : values) {
            if (v != null)
                return v;
        }
        return null;
    }
}

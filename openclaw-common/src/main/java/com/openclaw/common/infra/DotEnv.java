package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Loads environment variables from .env files — CWD first, then global fallback
 * (~/.openclaw/.env).
 * Values already present in the environment are NOT overridden.
 * <p>
 * Corresponds to TypeScript's infra/dotenv.ts + infra/env-file.ts.
 */
public final class DotEnv {

    private DotEnv() {
    }

    private static final Logger log = LoggerFactory.getLogger(DotEnv.class);

    /**
     * Load .env files. CWD .env is loaded first; then the global fallback at
     * {@code stateDir/.env} without overriding existing values.
     *
     * @param stateDir the openclaw state directory (e.g. ~/.openclaw)
     * @param target   mutable map to populate (typically {@code System.getenv()}
     *                 doesn't work,
     *                 so callers should maintain their own overlay map)
     */
    public static void loadDotEnv(Path stateDir, Map<String, String> target) {
        loadDotEnv(stateDir, target, false);
    }

    /**
     * Load .env files with optional quieting of log messages.
     */
    public static void loadDotEnv(Path stateDir, Map<String, String> target, boolean quiet) {
        // 1. Load from CWD
        Path cwdEnv = Path.of(System.getProperty("user.dir"), ".env");
        loadFile(cwdEnv, target, true, quiet);

        // 2. Load global fallback (no override)
        if (stateDir != null) {
            Path globalEnv = stateDir.resolve(".env");
            if (Files.exists(globalEnv)) {
                loadFile(globalEnv, target, false, quiet);
            }
        }
    }

    /**
     * Parse a .env file and populate the target map.
     *
     * @param path     path to the .env file
     * @param target   map to populate
     * @param override whether to override existing values
     * @param quiet    suppress log messages
     */
    static void loadFile(Path path, Map<String, String> target, boolean override, boolean quiet) {
        if (!Files.exists(path)) {
            return;
        }
        Map<String, String> parsed = parseEnvFile(path);
        int applied = 0;
        for (Map.Entry<String, String> entry : parsed.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            if (!override && target.containsKey(key) && target.get(key) != null && !target.get(key).isBlank()) {
                continue;
            }
            target.put(key, value);
            applied++;
        }
        if (!quiet && applied > 0) {
            log.debug("dotenv: loaded {} vars from {}", applied, path);
        }
    }

    /**
     * Parse a .env file into key-value pairs.
     * Supports: KEY=value, KEY="quoted value", KEY='quoted value', export
     * KEY=value.
     * Lines starting with # are comments.
     */
    public static Map<String, String> parseEnvFile(Path path) {
        Map<String, String> result = new LinkedHashMap<>();
        if (!Files.exists(path)) {
            return result;
        }
        try (BufferedReader reader = Files.newBufferedReader(path)) {
            String line;
            while ((line = reader.readLine()) != null) {
                String trimmed = line.trim();
                if (trimmed.isEmpty() || trimmed.startsWith("#")) {
                    continue;
                }
                // Strip optional 'export ' prefix
                if (trimmed.startsWith("export ")) {
                    trimmed = trimmed.substring(7).trim();
                }
                int eq = trimmed.indexOf('=');
                if (eq <= 0) {
                    continue;
                }
                String key = trimmed.substring(0, eq).trim();
                String value = trimmed.substring(eq + 1).trim();
                // Remove surrounding quotes
                if (value.length() >= 2) {
                    if ((value.startsWith("\"") && value.endsWith("\""))
                            || (value.startsWith("'") && value.endsWith("'"))) {
                        value = value.substring(1, value.length() - 1);
                    }
                }
                if (!key.isEmpty()) {
                    result.put(key, value);
                }
            }
        } catch (IOException e) {
            log.warn("dotenv: failed to read {}: {}", path, e.getMessage());
        }
        return result;
    }

    /**
     * Upsert a key-value pair in the shared .env file.
     * Creates the file if it doesn't exist. Preserves existing entries.
     *
     * @return the path to the .env file
     */
    public static UpsertResult upsertSharedEnvVar(Path stateDir, String key, String value) {
        Path filepath = stateDir.resolve(".env");
        String trimmedKey = key.trim();

        String raw = "";
        boolean fileExisted = Files.exists(filepath);
        if (fileExisted) {
            try {
                raw = Files.readString(filepath);
            } catch (IOException e) {
                log.warn("dotenv: failed to read {}: {}", filepath, e.getMessage());
            }
        }

        String[] lines = raw.isEmpty() ? new String[0] : raw.split("\\r?\\n", -1);
        StringBuilder result = new StringBuilder();
        boolean replaced = false;
        boolean updated = false;

        String pattern = "^(\\s*(?:export\\s+)?)" + escapeRegExp(trimmedKey) + "\\s*=";
        java.util.regex.Pattern regex = java.util.regex.Pattern.compile(pattern);

        for (String line : lines) {
            java.util.regex.Matcher matcher = regex.matcher(line);
            if (matcher.find()) {
                String prefix = matcher.group(1) != null ? matcher.group(1) : "";
                String next = prefix + trimmedKey + "=" + value;
                if (!next.equals(line)) {
                    updated = true;
                }
                result.append(next).append('\n');
                replaced = true;
            } else {
                result.append(line).append('\n');
            }
        }

        if (!replaced) {
            result.append(trimmedKey).append('=').append(value).append('\n');
            updated = true;
        }

        try {
            Files.createDirectories(filepath.getParent());
            Files.writeString(filepath, result.toString());
        } catch (IOException e) {
            log.warn("dotenv: failed to write {}: {}", filepath, e.getMessage());
        }

        return new UpsertResult(filepath, updated, !fileExisted);
    }

    private static String escapeRegExp(String value) {
        return value.replaceAll("[.*+?^${}()|\\[\\]\\\\]", "\\\\$0");
    }

    public record UpsertResult(Path path, boolean updated, boolean created) {
    }
}

package com.openclaw.hooks;

import lombok.extern.slf4j.Slf4j;

import java.nio.file.*;
import java.time.Instant;
import java.util.*;

/**
 * Hook installation utilities — validation, directory resolution,
 * path-traversal protection, and install record tracking.
 * Corresponds to TypeScript's hooks/install.ts + installs.ts.
 *
 * <p>
 * Note: npm/archive-specific install flows are omitted (Node.js-only).
 * Java hooks are installed by copying directories or via plugin SPI.
 * </p>
 */
@Slf4j
public final class HookInstall {

    private HookInstall() {
    }

    private static final String DEFAULT_HOOKS_DIR = ".openclaw/hooks";

    // =====================================================================
    // Hook ID validation
    // =====================================================================

    /**
     * Validate a hook ID.
     *
     * @return error message, or null if valid
     */
    public static String validateHookId(String hookId) {
        if (hookId == null || hookId.isBlank()) {
            return "invalid hook name: missing";
        }
        if (".".equals(hookId) || "..".equals(hookId)) {
            return "invalid hook name: reserved path segment";
        }
        if (hookId.contains("/") || hookId.contains("\\")) {
            return "invalid hook name: path separators not allowed";
        }
        return null;
    }

    /**
     * Convert a scoped package name to unscoped form.
     * E.g. "@openclaw/hook-test" → "hook-test"
     */
    public static String unscopedPackageName(String name) {
        if (name == null)
            return "";
        String trimmed = name.trim();
        if (trimmed.isEmpty())
            return trimmed;
        if (trimmed.contains("/")) {
            String[] parts = trimmed.split("/");
            return parts[parts.length - 1];
        }
        return trimmed;
    }

    /**
     * Make a directory-safe name from an input string.
     */
    public static String safeDirName(String input) {
        if (input == null)
            return "";
        String trimmed = input.trim();
        if (trimmed.isEmpty())
            return trimmed;
        return trimmed.replace("/", "__").replace("\\", "__");
    }

    // =====================================================================
    // Directory resolution with path-traversal protection
    // =====================================================================

    /**
     * Resolve the installation directory for a hook.
     *
     * @param hookId   hook identifier
     * @param hooksDir optional hooks base directory (defaults to ~/.openclaw/hooks)
     * @return absolute path to the hook install directory
     * @throws IllegalArgumentException if hookId is invalid or path traversal
     *                                  detected
     */
    public static String resolveHookInstallDir(String hookId, String hooksDir) {
        String hooksBase = (hooksDir != null && !hooksDir.isBlank())
                ? hooksDir
                : resolveDefaultHooksDir();
        String hookIdError = validateHookId(hookId);
        if (hookIdError != null) {
            throw new IllegalArgumentException(hookIdError);
        }
        return resolveSafeInstallDir(hooksBase, hookId);
    }

    /**
     * Resolve a safe installation directory, checking for path traversal.
     *
     * @return resolved target directory path
     * @throws IllegalArgumentException if path traversal detected
     */
    static String resolveSafeInstallDir(String hooksDir, String hookId) {
        Path targetDir = Path.of(hooksDir, safeDirName(hookId));
        Path resolvedBase = Path.of(hooksDir).toAbsolutePath().normalize();
        Path resolvedTarget = targetDir.toAbsolutePath().normalize();

        if (!resolvedTarget.startsWith(resolvedBase)) {
            throw new IllegalArgumentException(
                    "invalid hook name: path traversal detected");
        }
        return targetDir.toString();
    }

    // =====================================================================
    // Install record tracking (from installs.ts)
    // =====================================================================

    /**
     * Hook install record.
     */
    public record HookInstallRecord(
            String hookId,
            String source,
            String version,
            String installedAt) {
    }

    /**
     * Record a hook installation in config.
     * Returns a new map of hook installs to be persisted.
     *
     * @param existingInstalls current install records (may be null)
     * @param record           install record to add/update
     * @return updated install records map
     */
    public static Map<String, Map<String, String>> recordHookInstall(
            Map<String, Map<String, String>> existingInstalls,
            HookInstallRecord record) {

        Map<String, Map<String, String>> result = new LinkedHashMap<>();
        if (existingInstalls != null) {
            result.putAll(existingInstalls);
        }

        Map<String, String> entry = new LinkedHashMap<>();
        if (record.source() != null)
            entry.put("source", record.source());
        if (record.version() != null)
            entry.put("version", record.version());
        entry.put("installedAt",
                record.installedAt() != null ? record.installedAt() : Instant.now().toString());

        // Merge with existing entry
        if (result.containsKey(record.hookId())) {
            Map<String, String> existing = new LinkedHashMap<>(result.get(record.hookId()));
            existing.putAll(entry);
            result.put(record.hookId(), existing);
        } else {
            result.put(record.hookId(), entry);
        }
        return result;
    }

    // =====================================================================
    // Hook directory validation
    // =====================================================================

    /**
     * Validate that a hook directory contains required files.
     *
     * @param hookDir path to the hook directory
     * @return list of validation errors (empty if valid)
     */
    public static List<String> validateHookDir(String hookDir) {
        List<String> errors = new ArrayList<>();
        Path dir = Path.of(hookDir);

        if (!Files.isDirectory(dir)) {
            errors.add("hook directory does not exist: " + hookDir);
            return errors;
        }

        Path hookMd = dir.resolve("HOOK.md");
        if (!Files.isRegularFile(hookMd)) {
            errors.add("HOOK.md missing in " + hookDir);
        }

        // Check for handler file
        List<String> handlerCandidates = List.of(
                "handler.sh", "handler.java", "handler.ts", "handler.js",
                "index.ts", "index.js");
        boolean hasHandler = handlerCandidates.stream()
                .anyMatch(c -> Files.isRegularFile(dir.resolve(c)));
        if (!hasHandler) {
            errors.add("no handler file found in " + hookDir);
        }

        return errors;
    }

    // =====================================================================
    // Helpers
    // =====================================================================

    private static String resolveDefaultHooksDir() {
        String home = System.getProperty("user.home");
        return Path.of(home, DEFAULT_HOOKS_DIR).toString();
    }
}

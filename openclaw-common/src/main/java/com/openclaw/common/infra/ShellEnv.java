package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Loads environment variables from the user's login shell.
 * Useful under launchd or other minimal-PATH environments where
 * API keys and tool paths are set in ~/.zshrc / ~/.bashrc but not
 * inherited by the JVM.
 * <p>
 * Corresponds to TypeScript's infra/shell-env.ts.
 */
public final class ShellEnv {

    private ShellEnv() {
    }

    private static final Logger log = LoggerFactory.getLogger(ShellEnv.class);
    private static final long DEFAULT_TIMEOUT_MS = 15_000;
    private static volatile List<String> lastAppliedKeys = List.of();
    private static volatile String cachedShellPath;
    private static volatile boolean shellPathResolved = false;

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Result of a shell env fallback attempt.
     */
    public record ShellEnvResult(boolean ok, List<String> applied, String skippedReason,
            String error) {
        public static ShellEnvResult skipped(String reason) {
            return new ShellEnvResult(true, List.of(), reason, null);
        }

        public static ShellEnvResult success(List<String> applied) {
            return new ShellEnvResult(true, applied, null, null);
        }

        public static ShellEnvResult failure(String error) {
            return new ShellEnvResult(false, List.of(), null, error);
        }
    }

    /**
     * Load missing environment variables from the user's login shell.
     *
     * @param enabled      whether the fallback is enabled
     * @param target       mutable env map to augment
     * @param expectedKeys keys to look for (e.g. ANTHROPIC_API_KEY, OPENAI_API_KEY)
     * @param timeoutMs    max time to wait for shell, or null for default
     * @return result describing what happened
     */
    public static ShellEnvResult loadShellEnvFallback(boolean enabled,
            Map<String, String> target,
            List<String> expectedKeys,
            Long timeoutMs) {
        if (!enabled) {
            lastAppliedKeys = List.of();
            return ShellEnvResult.skipped("disabled");
        }

        // Skip if any of the expected keys already have values
        boolean hasAnyKey = expectedKeys.stream()
                .anyMatch(key -> {
                    String val = target.get(key);
                    return val != null && !val.isBlank();
                });
        if (hasAnyKey) {
            lastAppliedKeys = List.of();
            return ShellEnvResult.skipped("already-has-keys");
        }

        long timeout = timeoutMs != null && timeoutMs > 0 ? timeoutMs : DEFAULT_TIMEOUT_MS;
        String shell = resolveShell();

        Map<String, String> shellEnv;
        try {
            shellEnv = runShellEnv(shell, timeout);
        } catch (Exception e) {
            String msg = e.getMessage() != null ? e.getMessage() : e.toString();
            log.warn("[openclaw] shell env fallback failed: {}", msg);
            lastAppliedKeys = List.of();
            return ShellEnvResult.failure(msg);
        }

        List<String> applied = new ArrayList<>();
        for (String key : expectedKeys) {
            String existing = target.get(key);
            if (existing != null && !existing.isBlank()) {
                continue;
            }
            String value = shellEnv.get(key);
            if (value != null && !value.isBlank()) {
                target.put(key, value);
                applied.add(key);
            }
        }

        lastAppliedKeys = List.copyOf(applied);
        return ShellEnvResult.success(applied);
    }

    /**
     * Check if shell env fallback should be enabled.
     */
    public static boolean shouldEnableShellEnvFallback() {
        return EnvUtils.isTruthy(System.getenv("OPENCLAW_LOAD_SHELL_ENV"));
    }

    /**
     * Get PATH from the user's login shell.
     */
    public static String getShellPathFromLoginShell(Long timeoutMs) {
        if (shellPathResolved) {
            return cachedShellPath;
        }

        if (isWindows()) {
            cachedShellPath = null;
            shellPathResolved = true;
            return null;
        }

        long timeout = timeoutMs != null && timeoutMs > 0 ? timeoutMs : DEFAULT_TIMEOUT_MS;
        String shell = resolveShell();

        try {
            Map<String, String> shellEnv = runShellEnv(shell, timeout);
            String shellPath = shellEnv.get("PATH");
            cachedShellPath = shellPath != null && !shellPath.isBlank() ? shellPath : null;
        } catch (Exception e) {
            cachedShellPath = null;
        }

        shellPathResolved = true;
        return cachedShellPath;
    }

    /**
     * Get the list of keys that were applied by the last fallback call.
     */
    public static List<String> getAppliedKeys() {
        return lastAppliedKeys;
    }

    // =========================================================================
    // Internals
    // =========================================================================

    static String resolveShell() {
        String shell = System.getenv("SHELL");
        return shell != null && !shell.isBlank() ? shell.trim() : "/bin/sh";
    }

    /**
     * Execute a login shell and capture its environment using env -0.
     */
    static Map<String, String> runShellEnv(String shell, long timeoutMs) throws IOException, InterruptedException {
        ProcessBuilder pb = new ProcessBuilder(shell, "-l", "-c", "env -0")
                .redirectErrorStream(false);
        Process process = pb.start();

        byte[] stdout;
        try {
            stdout = process.getInputStream().readAllBytes();
        } finally {
            boolean finished = process.waitFor(timeoutMs, TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                throw new IOException("shell env timed out after " + timeoutMs + "ms");
            }
        }

        return parseShellEnv(stdout);
    }

    /**
     * Parse NUL-delimited env output (from env -0).
     */
    static Map<String, String> parseShellEnv(byte[] stdout) {
        Map<String, String> result = new java.util.LinkedHashMap<>();
        String content = new String(stdout, java.nio.charset.StandardCharsets.UTF_8);
        String[] parts = content.split("\0");
        for (String part : parts) {
            if (part.isEmpty()) {
                continue;
            }
            int eq = part.indexOf('=');
            if (eq <= 0) {
                continue;
            }
            String key = part.substring(0, eq);
            String value = part.substring(eq + 1);
            if (!key.isEmpty()) {
                result.put(key, value);
            }
        }
        return result;
    }

    private static boolean isWindows() {
        String os = System.getProperty("os.name", "").toLowerCase();
        return os.contains("win");
    }

    /**
     * Reset caches for testing.
     */
    public static void resetForTest() {
        lastAppliedKeys = List.of();
        cachedShellPath = null;
        shellPathResolved = false;
    }
}

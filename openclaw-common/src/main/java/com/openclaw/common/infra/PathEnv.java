package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Best-effort PATH bootstrap so skills and tools that require the
 * {@code openclaw}
 * CLI can run under launchd / minimal-PATH environments.
 * <p>
 * Corresponds to TypeScript's infra/path-env.ts.
 */
public final class PathEnv {

    private PathEnv() {
    }

    private static final Logger log = LoggerFactory.getLogger(PathEnv.class);
    private static volatile boolean bootstrapped = false;

    /**
     * Prepend well-known bin directories to the PATH environment variable.
     * Idempotent — does nothing on second and subsequent calls.
     *
     * @param target mutable env map (this method writes PATH to it)
     */
    public static void ensureCliOnPath(java.util.Map<String, String> target) {
        if (bootstrapped) {
            return;
        }
        bootstrapped = true;

        String existing = target.getOrDefault("PATH", System.getenv("PATH"));
        if (existing == null) {
            existing = "";
        }

        List<String> candidates = candidateBinDirs();
        if (candidates.isEmpty()) {
            return;
        }

        String merged = mergePath(existing, candidates);
        if (merged != null && !merged.isEmpty()) {
            target.put("PATH", merged);
        }
    }

    /**
     * Merge PATH: prepend candidates (de-duplicated) before existing entries.
     */
    public static String mergePath(String existing, List<String> prepend) {
        LinkedHashSet<String> seen = new LinkedHashSet<>();
        List<String> merged = new ArrayList<>();

        // Prepend first
        for (String part : prepend) {
            String trimmed = part.trim();
            if (!trimmed.isEmpty() && seen.add(trimmed)) {
                merged.add(trimmed);
            }
        }
        // Then existing
        for (String part : existing.split(File.pathSeparator)) {
            String trimmed = part.trim();
            if (!trimmed.isEmpty() && seen.add(trimmed)) {
                merged.add(trimmed);
            }
        }

        return String.join(File.pathSeparator, merged);
    }

    /**
     * Produce a list of candidate bin directories that are likely to contain
     * the openclaw CLI or important tools (pnpm, bun, brew, etc.).
     */
    public static List<String> candidateBinDirs() {
        String homeDir = System.getProperty("user.home");
        boolean isMac = System.getProperty("os.name", "").toLowerCase().contains("mac");

        List<String> candidates = new ArrayList<>();

        // Project-local node_modules/.bin
        String cwd = System.getProperty("user.dir");
        if (cwd != null) {
            Path localBin = Path.of(cwd, "node_modules", ".bin");
            if (isDirectory(localBin)) {
                candidates.add(localBin.toString());
            }
        }

        // mise shims
        String miseDataDir = System.getenv("MISE_DATA_DIR");
        if (miseDataDir == null) {
            miseDataDir = Path.of(homeDir, ".local", "share", "mise").toString();
        }
        Path miseShims = Path.of(miseDataDir, "shims");
        if (isDirectory(miseShims)) {
            candidates.add(miseShims.toString());
        }

        // Homebrew
        addIfDirectory(candidates, "/opt/homebrew/bin");
        addIfDirectory(candidates, "/usr/local/bin");

        // macOS-specific
        if (isMac) {
            addIfDirectory(candidates, Path.of(homeDir, "Library", "pnpm").toString());
        }

        // XDG bin
        String xdgBin = System.getenv("XDG_BIN_HOME");
        if (xdgBin != null) {
            addIfDirectory(candidates, xdgBin);
        }

        // Common global install locations
        addIfDirectory(candidates, Path.of(homeDir, ".local", "bin").toString());
        addIfDirectory(candidates, Path.of(homeDir, ".local", "share", "pnpm").toString());
        addIfDirectory(candidates, Path.of(homeDir, ".bun", "bin").toString());
        addIfDirectory(candidates, Path.of(homeDir, ".yarn", "bin").toString());

        // Standard system paths
        addIfDirectory(candidates, "/usr/bin");
        addIfDirectory(candidates, "/bin");

        return candidates;
    }

    private static boolean isDirectory(Path path) {
        try {
            return Files.isDirectory(path);
        } catch (Exception e) {
            return false;
        }
    }

    private static void addIfDirectory(List<String> list, String path) {
        if (isDirectory(Path.of(path))) {
            list.add(path);
        }
    }

    /**
     * Reset bootstrap state for testing.
     */
    public static void resetForTest() {
        bootstrapped = false;
    }
}

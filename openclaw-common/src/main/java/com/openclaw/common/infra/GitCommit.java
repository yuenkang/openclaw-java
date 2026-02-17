package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Resolves the current Git commit hash from multiple sources:
 * <ol>
 * <li>Environment variables (GIT_COMMIT, GIT_SHA)</li>
 * <li>.git/HEAD (direct or ref-based)</li>
 * <li>System property (openclaw.git.commit)</li>
 * </ol>
 * Result is cached after first resolution.
 * <p>
 * Port of: infra/git-commit.ts
 */
@Slf4j
public class GitCommit {

    private static volatile String cachedCommit;
    private static volatile boolean resolved = false;

    /**
     * Resolve and return the short (7-char) commit hash, or null if not available.
     */
    public static String resolveCommitHash() {
        return resolveCommitHash(null);
    }

    /**
     * Resolve with a specific working directory.
     *
     * @param cwd the directory to start searching for .git; null for user.dir
     */
    public static String resolveCommitHash(String cwd) {
        if (resolved) {
            return cachedCommit;
        }
        synchronized (GitCommit.class) {
            if (resolved) {
                return cachedCommit;
            }
            cachedCommit = doResolve(cwd);
            resolved = true;
            return cachedCommit;
        }
    }

    /** Reset cache (mainly for testing). */
    public static void resetCache() {
        synchronized (GitCommit.class) {
            cachedCommit = null;
            resolved = false;
        }
    }

    private static String doResolve(String cwd) {
        // 1. Environment variables
        String envCommit = trimOrNull(System.getenv("GIT_COMMIT"));
        if (envCommit == null) {
            envCommit = trimOrNull(System.getenv("GIT_SHA"));
        }
        if (envCommit != null) {
            return formatCommit(envCommit);
        }

        // 2. System property
        String sysProp = trimOrNull(System.getProperty("openclaw.git.commit"));
        if (sysProp != null) {
            return formatCommit(sysProp);
        }

        // 3. Resolve from .git/HEAD
        try {
            String startDir = cwd != null ? cwd : System.getProperty("user.dir", ".");
            Path headPath = resolveGitHead(Paths.get(startDir));
            if (headPath == null) {
                return null;
            }
            String head = Files.readString(headPath, StandardCharsets.UTF_8).trim();
            if (head.isEmpty()) {
                return null;
            }
            if (head.startsWith("ref:")) {
                String ref = head.replaceFirst("(?i)^ref:\\s*", "").trim();
                Path refPath = headPath.getParent().resolve(ref);
                try {
                    String refHash = Files.readString(refPath, StandardCharsets.UTF_8).trim();
                    return formatCommit(refHash);
                } catch (IOException e) {
                    // Try packed-refs
                    return null;
                }
            }
            return formatCommit(head);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Walk up directory tree to find .git/HEAD, handling submodules (gitdir: ...)
     */
    private static Path resolveGitHead(Path startDir) {
        Path current = startDir.toAbsolutePath().normalize();
        for (int i = 0; i < 12; i++) {
            Path gitPath = current.resolve(".git");
            try {
                if (Files.isDirectory(gitPath)) {
                    return gitPath.resolve("HEAD");
                }
                if (Files.isRegularFile(gitPath)) {
                    // Submodule or worktree — read gitdir pointer
                    String raw = Files.readString(gitPath, StandardCharsets.UTF_8);
                    var matcher = java.util.regex.Pattern.compile("(?i)gitdir:\\s*(.+)")
                            .matcher(raw);
                    if (matcher.find()) {
                        Path resolved = current.resolve(matcher.group(1).trim()).normalize();
                        return resolved.resolve("HEAD");
                    }
                }
            } catch (IOException e) {
                // ignore
            }
            Path parent = current.getParent();
            if (parent == null || parent.equals(current)) {
                break;
            }
            current = parent;
        }
        return null;
    }

    private static String formatCommit(String value) {
        if (value == null || value.isEmpty()) {
            return null;
        }
        return value.length() > 7 ? value.substring(0, 7) : value;
    }

    private static String trimOrNull(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }
}

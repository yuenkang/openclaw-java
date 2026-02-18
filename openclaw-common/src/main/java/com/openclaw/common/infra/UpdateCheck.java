package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Checks for updates from Git repositories and package registries.
 * Includes semver comparison, git ahead/behind detection, and install-kind
 * resolution.
 * <p>
 * Corresponds to TypeScript's infra/update-check.ts.
 */
public final class UpdateCheck {

    private UpdateCheck() {
    }

    private static final Logger log = LoggerFactory.getLogger(UpdateCheck.class);
    private static final long DEFAULT_TIMEOUT_MS = 5_000;
    private static final Pattern SEMVER_PATTERN = Pattern.compile(
            "^v?(\\d+)\\.(\\d+)\\.(\\d+)(?:-([a-zA-Z0-9.]+))?");

    // =========================================================================
    // Data model
    // =========================================================================

    public enum InstallKind {
        GIT, PACKAGE, UNKNOWN
    }

    /**
     * Git status information.
     */
    public record GitStatus(
            boolean isGitRepo,
            String branch,
            String tag,
            int ahead,
            int behind,
            boolean dirty) {
    }

    /**
     * Complete update-check result.
     */
    public record UpdateCheckResult(
            InstallKind installKind,
            String currentVersion,
            GitStatus gitStatus,
            String latestVersion,
            String latestTag) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Perform a full update check.
     *
     * @param root      project root (if git-based install)
     * @param version   current running version
     * @param timeoutMs timeout for subprocesses
     * @param fetchGit  whether to run git fetch
     * @return the update check result
     */
    public static UpdateCheckResult checkUpdateStatus(
            String root, String version, Long timeoutMs, boolean fetchGit) {
        long timeout = timeoutMs != null && timeoutMs > 0 ? timeoutMs : DEFAULT_TIMEOUT_MS;
        InstallKind installKind = resolveInstallKind(root);
        GitStatus gitStatus = null;

        if (installKind == InstallKind.GIT && root != null) {
            gitStatus = getGitStatus(root, timeout, fetchGit);
        }

        return new UpdateCheckResult(installKind, version, gitStatus, null, null);
    }

    /**
     * Determine how the application was installed.
     */
    public static InstallKind resolveInstallKind(String root) {
        if (root == null || root.isBlank()) {
            return InstallKind.UNKNOWN;
        }
        Path rootPath = Path.of(root);
        if (Files.isDirectory(rootPath.resolve(".git"))) {
            return InstallKind.GIT;
        }
        if (Files.exists(rootPath.resolve("package.json"))) {
            return InstallKind.PACKAGE;
        }
        // Java-specific: pom.xml indicates a source build
        if (Files.exists(rootPath.resolve("pom.xml"))) {
            return InstallKind.GIT;
        }
        return InstallKind.UNKNOWN;
    }

    /**
     * Compare two semver strings.
     *
     * @return negative if v1 &lt; v2, 0 if equal, positive if v1 &gt; v2, or null
     *         if
     *         either is unparseable
     */
    public static Integer compareSemver(String v1, String v2) {
        int[] a = parseSemver(v1);
        int[] b = parseSemver(v2);
        if (a == null || b == null) {
            return null;
        }
        for (int i = 0; i < 3; i++) {
            if (a[i] != b[i]) {
                return Integer.compare(a[i], b[i]);
            }
        }
        return 0;
    }

    /**
     * Parse a semver string into [major, minor, patch].
     *
     * @return the version components, or null if unparseable
     */
    public static int[] parseSemver(String version) {
        if (version == null || version.isBlank()) {
            return null;
        }
        Matcher matcher = SEMVER_PATTERN.matcher(version.trim());
        if (!matcher.find()) {
            return null;
        }
        try {
            return new int[] {
                    Integer.parseInt(matcher.group(1)),
                    Integer.parseInt(matcher.group(2)),
                    Integer.parseInt(matcher.group(3))
            };
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Extract the pre-release label from a semver string.
     *
     * @return the pre-release label, or null
     */
    public static String semverPreRelease(String version) {
        if (version == null) {
            return null;
        }
        Matcher matcher = SEMVER_PATTERN.matcher(version.trim());
        if (!matcher.find()) {
            return null;
        }
        return matcher.group(4); // may be null
    }

    // =========================================================================
    // Git operations
    // =========================================================================

    /**
     * Get the git status of a repository.
     */
    public static GitStatus getGitStatus(String root, long timeoutMs, boolean fetch) {
        if (root == null || !Files.isDirectory(Path.of(root, ".git"))) {
            return new GitStatus(false, null, null, 0, 0, false);
        }

        if (fetch) {
            runGit(root, timeoutMs, "fetch", "--quiet", "--prune");
        }

        String branch = runGitOutput(root, timeoutMs, "rev-parse", "--abbrev-ref", "HEAD");
        String tag = runGitOutput(root, timeoutMs, "describe", "--tags", "--exact-match", "HEAD");
        boolean dirty = isGitDirty(root, timeoutMs);
        int[] aheadBehind = getAheadBehind(root, branch, timeoutMs);

        return new GitStatus(
                true,
                branch != null && !branch.isBlank() ? branch.trim() : null,
                tag != null && !tag.isBlank() ? tag.trim() : null,
                aheadBehind[0],
                aheadBehind[1],
                dirty);
    }

    // =========================================================================
    // Internals
    // =========================================================================

    private static boolean isGitDirty(String root, long timeoutMs) {
        String output = runGitOutput(root, timeoutMs, "status", "--porcelain");
        return output != null && !output.isBlank();
    }

    private static int[] getAheadBehind(String root, String branch, long timeoutMs) {
        if (branch == null || branch.isBlank() || "HEAD".equals(branch)) {
            return new int[] { 0, 0 };
        }
        String output = runGitOutput(root, timeoutMs,
                "rev-list", "--left-right", "--count", branch + "...origin/" + branch);
        if (output == null || output.isBlank()) {
            return new int[] { 0, 0 };
        }
        String[] parts = output.trim().split("\\s+");
        if (parts.length < 2) {
            return new int[] { 0, 0 };
        }
        try {
            return new int[] { Integer.parseInt(parts[0]), Integer.parseInt(parts[1]) };
        } catch (NumberFormatException e) {
            return new int[] { 0, 0 };
        }
    }

    private static void runGit(String root, long timeoutMs, String... args) {
        try {
            String[] cmd = new String[args.length + 1];
            cmd[0] = "git";
            System.arraycopy(args, 0, cmd, 1, args.length);
            ProcessBuilder pb = new ProcessBuilder(cmd)
                    .directory(new java.io.File(root))
                    .redirectErrorStream(true);
            Process process = pb.start();
            try (var reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                while (reader.readLine() != null) {
                    // consume
                }
            }
            process.waitFor(timeoutMs, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            log.debug("git {} failed: {}", args.length > 0 ? args[0] : "", e.getMessage());
        }
    }

    private static String runGitOutput(String root, long timeoutMs, String... args) {
        try {
            String[] cmd = new String[args.length + 1];
            cmd[0] = "git";
            System.arraycopy(args, 0, cmd, 1, args.length);
            ProcessBuilder pb = new ProcessBuilder(cmd)
                    .directory(new java.io.File(root))
                    .redirectErrorStream(false);
            Process process = pb.start();
            StringBuilder sb = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (!sb.isEmpty()) {
                        sb.append('\n');
                    }
                    sb.append(line);
                }
            }
            boolean finished = process.waitFor(timeoutMs, TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return null;
            }
            return process.exitValue() == 0 ? sb.toString() : null;
        } catch (Exception e) {
            return null;
        }
    }
}

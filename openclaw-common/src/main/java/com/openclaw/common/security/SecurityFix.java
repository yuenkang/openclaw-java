package com.openclaw.common.security;

import com.openclaw.common.security.SecurityAuditTypes.*;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.*;

/**
 * Security fix engine — applies remediation for common security issues.
 * Translates TS security/fix.ts — chmod, config fixes, include path resolution.
 *
 */
public final class SecurityFix {

    private SecurityFix() {
    }

    // -----------------------------------------------------------------------
    // Types
    // -----------------------------------------------------------------------

    public sealed interface FixAction permits ChmodAction, ConfigAction {
        boolean ok();

        String description();
    }

    public record ChmodAction(
            String path,
            String mode,
            boolean ok,
            String skipped,
            String error) implements FixAction {
        @Override
        public String description() {
            return "chmod " + mode + " " + path;
        }
    }

    public record ConfigAction(
            String change,
            boolean ok,
            String error) implements FixAction {
        @Override
        public String description() {
            return change;
        }
    }

    public record FixResult(
            boolean ok,
            String stateDir,
            String configPath,
            boolean configWritten,
            List<String> changes,
            List<FixAction> actions,
            List<String> errors) {
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Apply security fixes for common issues.
     */
    public static FixResult fixSecurityFootguns() {
        return fixSecurityFootguns(null, null);
    }

    public static FixResult fixSecurityFootguns(String stateDir, String configPath) {
        String effectiveStateDir = stateDir != null ? stateDir
                : System.getProperty("user.home") + "/.openclaw";
        String effectiveConfigPath = configPath != null ? configPath
                : effectiveStateDir + "/config.json";

        List<String> changes = new ArrayList<>();
        List<FixAction> actions = new ArrayList<>();
        List<String> errors = new ArrayList<>();

        // Fix state directory permissions
        chmodPath(effectiveStateDir, "700", "dir", actions, changes, errors);

        // Fix config file permissions
        chmodPath(effectiveConfigPath, "600", "file", actions, changes, errors);

        // Fix credentials and agent state directories
        chmodCredentialsAndAgentState(effectiveStateDir, actions, changes, errors);

        // Fix log directory
        Path logDir = Path.of("/tmp/openclaw");
        if (Files.isDirectory(logDir)) {
            chmodPath(logDir.toString(), "700", "dir", actions, changes, errors);
        }

        boolean allOk = errors.isEmpty() && actions.stream().allMatch(FixAction::ok);
        return new FixResult(allOk, effectiveStateDir, effectiveConfigPath,
                false, changes, actions, errors);
    }

    // -----------------------------------------------------------------------
    // Include path resolution
    // -----------------------------------------------------------------------

    /**
     * Recursively collect all config include paths from $include directives.
     */
    public static List<String> collectIncludePathsRecursive(String configPath, String content) {
        Set<String> visited = new LinkedHashSet<>();
        collectIncludesWalk(configPath, content, visited, 0);
        return new ArrayList<>(visited);
    }

    private static void collectIncludesWalk(String basePath, String content,
            Set<String> visited, int depth) {
        if (depth > 10)
            return; // prevent infinite recursion
        // Simple $include pattern matching
        String[] lines = content.split("\\r?\\n");
        for (String line : lines) {
            String trimmed = line.trim();
            // Match patterns like "$include": "path" or $include: path
            if (trimmed.contains("$include")) {
                int quoteStart = trimmed.indexOf('"', trimmed.indexOf("$include") + 8);
                if (quoteStart >= 0) {
                    int quoteEnd = trimmed.indexOf('"', quoteStart + 1);
                    if (quoteEnd > quoteStart) {
                        String includePath = trimmed.substring(quoteStart + 1, quoteEnd);
                        String resolved = resolveIncludePath(basePath, includePath);
                        if (!visited.contains(resolved)) {
                            visited.add(resolved);
                            // Recursively scan included files
                            try {
                                String includeContent = Files.readString(Path.of(resolved));
                                collectIncludesWalk(resolved, includeContent, visited, depth + 1);
                            } catch (IOException ignored) {
                                // file doesn't exist or is unreadable
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Resolve an include path relative to a base config path.
     */
    public static String resolveIncludePath(String baseConfigPath, String includePath) {
        Path base = Path.of(baseConfigPath).getParent();
        if (base == null)
            base = Path.of(".");
        return base.resolve(includePath).normalize().toString();
    }

    // -----------------------------------------------------------------------
    // Internals
    // -----------------------------------------------------------------------

    private static void chmodPath(String pathStr, String mode, String require,
            List<FixAction> actions, List<String> changes,
            List<String> errors) {
        Path path = Path.of(pathStr);
        if (!Files.exists(path)) {
            actions.add(new ChmodAction(pathStr, mode, true,
                    "Path does not exist", null));
            return;
        }
        boolean isDir = Files.isDirectory(path);
        if ("dir".equals(require) && !isDir) {
            actions.add(new ChmodAction(pathStr, mode, true,
                    "Expected directory but found file", null));
            return;
        }
        if ("file".equals(require) && isDir) {
            actions.add(new ChmodAction(pathStr, mode, true,
                    "Expected file but found directory", null));
            return;
        }

        try {
            Set<PosixFilePermission> perms = modeToPermissions(mode);
            Files.setPosixFilePermissions(path, perms);
            actions.add(new ChmodAction(pathStr, mode, true, null, null));
            changes.add("chmod " + mode + " " + pathStr);
        } catch (UnsupportedOperationException e) {
            actions.add(new ChmodAction(pathStr, mode, true,
                    "Not a POSIX filesystem", null));
        } catch (IOException e) {
            String errMsg = "Failed to chmod " + pathStr + ": " + e.getMessage();
            actions.add(new ChmodAction(pathStr, mode, false, null, errMsg));
            errors.add(errMsg);
        }
    }

    private static void chmodCredentialsAndAgentState(String stateDir,
            List<FixAction> actions,
            List<String> changes,
            List<String> errors) {
        Path statePath = Path.of(stateDir);
        if (!Files.isDirectory(statePath))
            return;

        try (DirectoryStream<Path> stream = Files.newDirectoryStream(statePath)) {
            for (Path entry : stream) {
                String name = entry.getFileName().toString().toLowerCase();
                if (Files.isDirectory(entry)) {
                    // Secure agent state directories
                    if (name.equals("agents") || name.equals("sessions") || name.equals("credentials")) {
                        chmodPath(entry.toString(), "700", "dir", actions, changes, errors);
                    }
                } else if (Files.isRegularFile(entry)) {
                    // Secure credential files
                    if (name.contains("credential") || name.contains("token") ||
                            name.contains("secret") || name.endsWith(".key") || name.endsWith(".pem")) {
                        chmodPath(entry.toString(), "600", "file", actions, changes, errors);
                    }
                }
            }
        } catch (IOException ignored) {
            // skip unreadable directory
        }
    }

    private static Set<PosixFilePermission> modeToPermissions(String mode) {
        return switch (mode) {
            case "700" -> Set.of(
                    PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.OWNER_EXECUTE);
            case "600" -> Set.of(
                    PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE);
            case "755" -> Set.of(
                    PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.OWNER_EXECUTE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.GROUP_EXECUTE,
                    PosixFilePermission.OTHERS_READ,
                    PosixFilePermission.OTHERS_EXECUTE);
            case "644" -> Set.of(
                    PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.OTHERS_READ);
            default -> PosixFilePermissions.fromString("rwx------");
        };
    }

    /**
     * Format fix result as a human-readable summary.
     */
    public static String formatResult(FixResult result) {
        StringBuilder sb = new StringBuilder();
        sb.append("Security Fix ").append(result.ok() ? "✅ OK" : "❌ FAILED").append("\n");
        sb.append("State dir: ").append(result.stateDir()).append("\n");
        sb.append("Config: ").append(result.configPath()).append("\n");
        if (!result.changes().isEmpty()) {
            sb.append("\nChanges:\n");
            result.changes().forEach(c -> sb.append("  • ").append(c).append("\n"));
        }
        if (!result.errors().isEmpty()) {
            sb.append("\nErrors:\n");
            result.errors().forEach(e -> sb.append("  ❌ ").append(e).append("\n"));
        }
        return sb.toString();
    }
}

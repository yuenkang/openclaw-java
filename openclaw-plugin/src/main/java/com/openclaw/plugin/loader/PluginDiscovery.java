package com.openclaw.plugin.loader;

import com.openclaw.plugin.PluginTypes;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

/**
 * Plugin discovery — scans directories for plugin candidates.
 * Corresponds to TypeScript's plugins/discovery.ts.
 */
@Slf4j
public final class PluginDiscovery {

    private PluginDiscovery() {
    }

    private static final Set<String> EXTENSION_EXTS = Set.of(
            ".java", ".jar", ".ts", ".js", ".mts", ".cts", ".mjs", ".cjs");

    // =========================================================================
    // Discovery result types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginCandidate {
        private String idHint;
        private String source;
        private String rootDir;
        private PluginTypes.PluginOrigin origin;
        private String workspaceDir;
        private String packageName;
        private String packageVersion;
        private String packageDescription;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PluginDiscoveryResult {
        @Builder.Default
        private List<PluginCandidate> candidates = new ArrayList<>();
        @Builder.Default
        private List<PluginTypes.PluginDiagnostic> diagnostics = new ArrayList<>();
    }

    // =========================================================================
    // Discovery
    // =========================================================================

    /**
     * Discover OpenClaw plugins in default locations and extra paths.
     *
     * @param workspaceDir current workspace directory (optional)
     * @param extraPaths   additional plugin paths to scan
     * @return discovery result with candidates and diagnostics
     */
    public static PluginDiscoveryResult discoverPlugins(
            String workspaceDir, List<String> extraPaths) {

        List<PluginCandidate> candidates = new ArrayList<>();
        List<PluginTypes.PluginDiagnostic> diagnostics = new ArrayList<>();
        Set<String> seen = new HashSet<>();

        // 1. Scan workspace .openclaw/plugins/ if present
        if (workspaceDir != null) {
            Path wsPlugins = Path.of(workspaceDir, ".openclaw", "plugins");
            if (Files.isDirectory(wsPlugins)) {
                discoverInDirectory(wsPlugins, PluginTypes.PluginOrigin.WORKSPACE,
                        workspaceDir, candidates, diagnostics, seen);
            }
        }

        // 2. Scan user-level plugins dir
        String userHome = System.getProperty("user.home");
        Path userPlugins = Path.of(userHome, ".openclaw", "plugins");
        if (Files.isDirectory(userPlugins)) {
            discoverInDirectory(userPlugins, PluginTypes.PluginOrigin.INSTALLED,
                    workspaceDir, candidates, diagnostics, seen);
        }

        // 3. Scan bundled plugins directory (via PluginBundledDir)
        String bundledDir = PluginBundledDir.resolve();
        if (bundledDir != null) {
            Path bundledPath = Path.of(bundledDir);
            if (Files.isDirectory(bundledPath)) {
                discoverInDirectory(bundledPath, PluginTypes.PluginOrigin.BUNDLED,
                        workspaceDir, candidates, diagnostics, seen);
            }
        }

        // 4. Scan extra paths
        if (extraPaths != null) {
            for (String rawPath : extraPaths) {
                Path p = Path.of(rawPath).toAbsolutePath().normalize();
                if (!Files.exists(p)) {
                    diagnostics.add(PluginTypes.PluginDiagnostic.builder()
                            .level("warn")
                            .message("plugin path not found: " + rawPath)
                            .build());
                    continue;
                }
                if (Files.isDirectory(p)) {
                    discoverInDirectory(p, PluginTypes.PluginOrigin.LOCAL,
                            workspaceDir, candidates, diagnostics, seen);
                } else {
                    discoverFromPath(p, PluginTypes.PluginOrigin.LOCAL,
                            workspaceDir, candidates, diagnostics, seen);
                }
            }
        }

        return PluginDiscoveryResult.builder()
                .candidates(candidates)
                .diagnostics(diagnostics)
                .build();
    }

    // =========================================================================
    // Directory scanning
    // =========================================================================

    static void discoverInDirectory(
            Path dir, PluginTypes.PluginOrigin origin, String workspaceDir,
            List<PluginCandidate> candidates,
            List<PluginTypes.PluginDiagnostic> diagnostics,
            Set<String> seen) {

        try (Stream<Path> children = Files.list(dir)) {
            children.sorted().forEach(child -> {
                if (Files.isDirectory(child)) {
                    // Check for plugin manifest
                    Path manifest = child.resolve(PluginManifest.PLUGIN_MANIFEST_FILENAME);
                    if (Files.exists(manifest)) {
                        String idHint = child.getFileName().toString();
                        addCandidate(candidates, seen, idHint,
                                child.toString(), child.toString(),
                                origin, workspaceDir);
                    }
                } else if (isExtensionFile(child.toString())) {
                    String idHint = deriveIdHint(child.getFileName().toString());
                    addCandidate(candidates, seen, idHint,
                            child.toString(), dir.toString(),
                            origin, workspaceDir);
                }
            });
        } catch (IOException e) {
            diagnostics.add(PluginTypes.PluginDiagnostic.builder()
                    .level("warn")
                    .message("failed to scan plugin directory " + dir + ": " + e.getMessage())
                    .build());
        }
    }

    static void discoverFromPath(
            Path path, PluginTypes.PluginOrigin origin, String workspaceDir,
            List<PluginCandidate> candidates,
            List<PluginTypes.PluginDiagnostic> diagnostics,
            Set<String> seen) {

        if (!Files.exists(path)) {
            diagnostics.add(PluginTypes.PluginDiagnostic.builder()
                    .level("warn")
                    .message("plugin file not found: " + path)
                    .build());
            return;
        }

        String idHint = deriveIdHint(path.getFileName().toString());
        addCandidate(candidates, seen, idHint,
                path.toString(), path.getParent().toString(),
                origin, workspaceDir);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    static boolean isExtensionFile(String filePath) {
        return EXTENSION_EXTS.stream().anyMatch(filePath::endsWith);
    }

    static String deriveIdHint(String fileName) {
        // Strip extension
        int dot = fileName.lastIndexOf('.');
        return dot > 0 ? fileName.substring(0, dot) : fileName;
    }

    private static void addCandidate(
            List<PluginCandidate> candidates, Set<String> seen,
            String idHint, String source, String rootDir,
            PluginTypes.PluginOrigin origin, String workspaceDir) {

        String normalized = Path.of(source).toAbsolutePath().normalize().toString();
        if (seen.contains(normalized)) {
            return;
        }
        seen.add(normalized);

        candidates.add(PluginCandidate.builder()
                .idHint(idHint)
                .source(source)
                .rootDir(rootDir)
                .origin(origin)
                .workspaceDir(workspaceDir)
                .build());
    }
}

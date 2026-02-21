package com.openclaw.plugin.install;

import com.openclaw.plugin.loader.PluginManifest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.jar.*;

/**
 * Plugin installer — installs plugins from JAR files, directories, or Maven
 * coordinates (replacing TypeScript's npm-based install.ts).
 *
 * <p>
 * In the Java ecosystem, plugins are distributed as JAR files containing
 * an openclaw.plugin.json manifest. This class handles extraction, validation,
 * and installation to the extensions directory.
 * </p>
 */
@Slf4j
public final class PluginInstaller {

    private PluginInstaller() {
    }

    private static final String EXTENSIONS_DIR_NAME = "extensions";

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class InstallResult {
        private boolean ok;
        private String pluginId;
        private String targetDir;
        private String version;
        private String error;
        @Builder.Default
        private List<String> extensions = List.of();
    }

    // =========================================================================
    // Install from JAR
    // =========================================================================

    /**
     * Install a plugin from a JAR file.
     */
    public static InstallResult installFromJar(String jarPath, String extensionsDir) {
        Path jar = Path.of(jarPath);
        if (!Files.exists(jar)) {
            return InstallResult.builder().ok(false)
                    .error("JAR file not found: " + jarPath).build();
        }

        String targetExtDir = resolveExtensionsDir(extensionsDir);
        try {
            // Extract and validate manifest from JAR
            String manifestJson = extractManifestFromJar(jar);
            if (manifestJson == null) {
                return InstallResult.builder().ok(false)
                        .error("No openclaw.plugin.json found in JAR").build();
            }

            // Create temp dir, write manifest, validate via PluginManifest
            Path tempDir = Files.createTempDirectory("openclaw-plugin-");
            Files.writeString(tempDir.resolve(PluginManifest.PLUGIN_MANIFEST_FILENAME),
                    manifestJson);
            PluginManifest.ManifestLoadResult loadResult = PluginManifest.loadManifest(tempDir.toString());
            deleteRecursive(tempDir);

            if (loadResult instanceof PluginManifest.ManifestLoadResult.Failure f) {
                return InstallResult.builder().ok(false)
                        .error("Invalid manifest: " + f.error()).build();
            }

            PluginManifest.Manifest manifest = ((PluginManifest.ManifestLoadResult.Success) loadResult).manifest();
            String pluginId = manifest.getId();

            // Copy JAR to extensions dir
            Path pluginDir = Path.of(targetExtDir, safeDirName(pluginId));
            Files.createDirectories(pluginDir);
            Files.copy(jar, pluginDir.resolve(jar.getFileName()),
                    StandardCopyOption.REPLACE_EXISTING);
            // Write manifest alongside JAR
            Files.writeString(
                    pluginDir.resolve(PluginManifest.PLUGIN_MANIFEST_FILENAME),
                    manifestJson);

            log.info("Installed plugin {} v{} to {}",
                    pluginId, manifest.getVersion(), pluginDir);

            return InstallResult.builder()
                    .ok(true)
                    .pluginId(pluginId)
                    .targetDir(pluginDir.toString())
                    .version(manifest.getVersion())
                    .build();
        } catch (Exception e) {
            return InstallResult.builder().ok(false)
                    .error("Installation failed: " + e.getMessage()).build();
        }
    }

    /**
     * Install a plugin from a local directory (must contain openclaw.plugin.json).
     */
    public static InstallResult installFromDir(String dirPath, String extensionsDir) {
        Path dir = Path.of(dirPath);
        if (!Files.isDirectory(dir)) {
            return InstallResult.builder().ok(false)
                    .error("Not a directory: " + dirPath).build();
        }

        PluginManifest.ManifestLoadResult loadResult = PluginManifest.loadManifest(dirPath);
        if (loadResult instanceof PluginManifest.ManifestLoadResult.Failure f) {
            return InstallResult.builder().ok(false)
                    .error("Invalid manifest: " + f.error()).build();
        }

        PluginManifest.Manifest manifest = ((PluginManifest.ManifestLoadResult.Success) loadResult).manifest();

        String targetExtDir = resolveExtensionsDir(extensionsDir);
        Path pluginDir = Path.of(targetExtDir, safeDirName(manifest.getId()));

        try {
            copyDir(dir, pluginDir);
            log.info("Installed plugin {} from dir to {}", manifest.getId(), pluginDir);
            return InstallResult.builder()
                    .ok(true)
                    .pluginId(manifest.getId())
                    .targetDir(pluginDir.toString())
                    .version(manifest.getVersion())
                    .build();
        } catch (Exception e) {
            return InstallResult.builder().ok(false)
                    .error("Copy failed: " + e.getMessage()).build();
        }
    }

    // =========================================================================
    // Validation
    // =========================================================================

    /**
     * Validate a plugin id string.
     */
    public static String validatePluginId(String pluginId) {
        if (pluginId == null || pluginId.isBlank()) {
            return "plugin id is required";
        }
        String trimmed = pluginId.trim();
        if (trimmed.length() > 128) {
            return "plugin id too long (max 128)";
        }
        if (!trimmed.matches("^[a-zA-Z][a-zA-Z0-9._-]*$")) {
            return "plugin id contains invalid characters";
        }
        return null; // valid
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String resolveExtensionsDir(String override) {
        if (override != null && !override.isBlank())
            return override;
        return Path.of(System.getProperty("user.home"), ".openclaw", EXTENSIONS_DIR_NAME)
                .toString();
    }

    static String safeDirName(String input) {
        return input.replaceAll("[^a-zA-Z0-9._-]", "_");
    }

    private static String extractManifestFromJar(Path jar) throws IOException {
        try (JarFile jf = new JarFile(jar.toFile())) {
            JarEntry entry = jf.getJarEntry(PluginManifest.PLUGIN_MANIFEST_FILENAME);
            if (entry == null)
                return null;
            try (InputStream is = jf.getInputStream(entry)) {
                return new String(is.readAllBytes());
            }
        }
    }

    private static void copyDir(Path src, Path dest) throws IOException {
        Files.createDirectories(dest);
        try (var stream = Files.walk(src)) {
            stream.forEach(source -> {
                try {
                    Path target = dest.resolve(src.relativize(source));
                    if (Files.isDirectory(source)) {
                        Files.createDirectories(target);
                    } else {
                        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                    }
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            });
        }
    }

    private static void deleteRecursive(Path dir) {
        try (var stream = Files.walk(dir)) {
            stream.sorted(Comparator.reverseOrder())
                    .forEach(p -> {
                        try {
                            Files.deleteIfExists(p);
                        } catch (IOException ignored) {
                        }
                    });
        } catch (IOException ignored) {
        }
    }
}

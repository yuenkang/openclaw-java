package com.openclaw.plugin.loader;

import lombok.extern.slf4j.Slf4j;

import java.nio.file.*;

/**
 * Resolve the bundled plugins directory.
 * Corresponds to TypeScript's plugins/bundled-dir.ts.
 * In Java, bundled plugins ship as JARs on the classpath or in an
 * 'extensions/' directory adjacent to the application.
 */
@Slf4j
public final class PluginBundledDir {

    private PluginBundledDir() {
    }

    /** Env var override for bundled plugins directory. */
    private static final String ENV_KEY = "OPENCLAW_BUNDLED_PLUGINS_DIR";

    /**
     * Resolve the bundled plugins directory.
     * Search order:
     * 1. OPENCLAW_BUNDLED_PLUGINS_DIR env var
     * 2. extensions/ sibling of the application JAR
     * 3. extensions/ in current working directory
     */
    public static String resolve() {
        // 1. Env override
        String override = System.getenv(ENV_KEY);
        if (override != null && !override.isBlank()) {
            return override.trim();
        }

        // 2. Sibling of application JAR
        try {
            Path jarDir = Path.of(
                    PluginBundledDir.class.getProtectionDomain()
                            .getCodeSource().getLocation().toURI())
                    .getParent();
            Path sibling = jarDir.resolve("extensions");
            if (Files.isDirectory(sibling)) {
                return sibling.toString();
            }
        } catch (Exception e) {
            log.debug("Could not resolve JAR sibling extensions dir: {}", e.getMessage());
        }

        // 3. CWD/extensions
        Path cwd = Path.of(System.getProperty("user.dir"), "extensions");
        if (Files.isDirectory(cwd)) {
            return cwd.toString();
        }

        return null;
    }
}

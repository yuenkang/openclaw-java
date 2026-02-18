package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.TimeUnit;

/**
 * Checks for external binary availability via {@code which} / {@code where}.
 * <p>
 * Corresponds to TypeScript's infra/binaries.ts.
 */
public final class Binaries {

    private Binaries() {
    }

    private static final Logger log = LoggerFactory.getLogger(Binaries.class);
    private static final long TIMEOUT_MS = 5_000;

    /**
     * Ensure a required binary exists on PATH. Throws if missing.
     *
     * @param name the binary name (e.g. "ffmpeg", "docker")
     * @throws IllegalStateException if the binary is not found
     */
    public static void ensureBinary(String name) {
        if (!hasBinary(name)) {
            throw new IllegalStateException(
                    "Missing required binary: " + name + ". Please install it.");
        }
    }

    /**
     * Check if a binary exists on PATH.
     */
    public static boolean hasBinary(String name) {
        if (name == null || name.isBlank()) {
            return false;
        }
        try {
            String cmd = isWindows() ? "where" : "which";
            ProcessBuilder pb = new ProcessBuilder(cmd, name.trim())
                    .redirectErrorStream(true);
            Process process = pb.start();
            // Drain stdout
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                while (reader.readLine() != null) {
                    // consume
                }
            }
            boolean finished = process.waitFor(TIMEOUT_MS, TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return false;
            }
            return process.exitValue() == 0;
        } catch (IOException | InterruptedException e) {
            log.debug("Binary check for '{}' failed: {}", name, e.getMessage());
            return false;
        }
    }

    /**
     * Resolve the full path of a binary on PATH.
     *
     * @return the absolute path, or null if not found
     */
    public static String resolveBinaryPath(String name) {
        if (name == null || name.isBlank()) {
            return null;
        }
        try {
            String cmd = isWindows() ? "where" : "which";
            ProcessBuilder pb = new ProcessBuilder(cmd, name.trim())
                    .redirectErrorStream(false);
            Process process = pb.start();
            String firstLine = null;
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                firstLine = reader.readLine();
            }
            boolean finished = process.waitFor(TIMEOUT_MS, TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return null;
            }
            if (process.exitValue() == 0 && firstLine != null && !firstLine.isBlank()) {
                return firstLine.trim();
            }
            return null;
        } catch (IOException | InterruptedException e) {
            return null;
        }
    }

    private static boolean isWindows() {
        String os = System.getProperty("os.name", "").toLowerCase();
        return os.contains("win");
    }
}

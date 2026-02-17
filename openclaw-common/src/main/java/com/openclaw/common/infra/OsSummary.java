package com.openclaw.common.infra;

import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Locale;

/**
 * OS summary — gathers platform, arch, and release information.
 * Corresponds to TypeScript's {@code infra/os-summary.ts}.
 */
@Slf4j
public class OsSummary {

    @Data
    @Builder
    public static class Summary {
        private final String platform; // "mac", "linux", "windows"
        private final String arch; // "aarch64", "x86_64", etc.
        private final String release; // OS version string
        private final String label; // Human-readable label
    }

    public static Summary resolve() {
        String osName = System.getProperty("os.name", "unknown").toLowerCase(Locale.ROOT);
        String osVersion = System.getProperty("os.version", "unknown");
        String arch = System.getProperty("os.arch", "unknown");

        String platform;
        String label;

        if (osName.contains("mac") || osName.contains("darwin")) {
            platform = "darwin";
            String macVer = macosVersion();
            label = "macos " + (macVer != null ? macVer : osVersion) + " (" + arch + ")";
        } else if (osName.contains("win")) {
            platform = "windows";
            label = "windows " + osVersion + " (" + arch + ")";
        } else if (osName.contains("linux")) {
            platform = "linux";
            label = "linux " + osVersion + " (" + arch + ")";
        } else {
            platform = osName;
            label = osName + " " + osVersion + " (" + arch + ")";
        }

        return Summary.builder()
                .platform(platform)
                .arch(arch)
                .release(osVersion)
                .label(label)
                .build();
    }

    private static String macosVersion() {
        try {
            Process p = new ProcessBuilder("sw_vers", "-productVersion")
                    .redirectErrorStream(true)
                    .start();
            try (var reader = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
                String line = reader.readLine();
                p.waitFor();
                return line != null ? line.trim() : null;
            }
        } catch (Exception e) {
            log.debug("Failed to read macOS version: {}", e.getMessage());
            return null;
        }
    }
}

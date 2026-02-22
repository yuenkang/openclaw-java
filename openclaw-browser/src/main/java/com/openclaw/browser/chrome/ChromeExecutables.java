package com.openclaw.browser.chrome;

import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Cross-platform Chrome/Chromium executable detection.
 * Corresponds to TypeScript's chrome.executables.ts.
 *
 * <p>Currently implements macOS detection. Linux and Windows support
 * can be added later following the same patterns from the TS version.
 */
@Slf4j
public final class ChromeExecutables {

    private ChromeExecutables() {
    }

    /**
     * Browser executable info.
     */
    public record BrowserExecutable(String kind, String path) {
        /** Known kinds: chrome, brave, edge, chromium, canary, custom */
    }

    // ==================== macOS Bundle IDs ====================

    private static final Set<String> CHROMIUM_BUNDLE_IDS = Set.of(
            "com.google.Chrome",
            "com.google.Chrome.beta",
            "com.google.Chrome.canary",
            "com.google.Chrome.dev",
            "com.brave.Browser",
            "com.brave.Browser.beta",
            "com.brave.Browser.nightly",
            "com.microsoft.Edge",
            "com.microsoft.Edge.Beta",
            "com.microsoft.Edge.Dev",
            "com.microsoft.Edge.Canary",
            "org.chromium.Chromium",
            "com.vivaldi.Vivaldi",
            "com.operasoftware.Opera",
            "com.opera.OperaGX"
    );

    // ==================== Platform detection ====================

    /**
     * Detect the default Chromium-based browser for the current platform.
     */
    public static BrowserExecutable detectDefaultChromiumExecutable() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac")) {
            return detectDefaultChromiumExecutableMac();
        } else if (os.contains("linux")) {
            return detectDefaultChromiumExecutableLinux();
        } else if (os.contains("win")) {
            return detectDefaultChromiumExecutableWindows();
        }
        return null;
    }

    /**
     * Find any Chrome executable on the current platform.
     */
    public static BrowserExecutable findChromeExecutable() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac")) {
            return findChromeExecutableMac();
        } else if (os.contains("linux")) {
            return findChromeExecutableLinux();
        }
        return null;
    }

    /**
     * Resolve the browser executable, considering user configuration.
     */
    public static BrowserExecutable resolveBrowserExecutable(String configuredExe) {
        // If user explicitly configured an executable path, use it
        if (configuredExe != null && !configuredExe.isBlank()) {
            File f = new File(configuredExe);
            if (f.exists() && f.canExecute()) {
                return new BrowserExecutable(inferKindFromPath(configuredExe), configuredExe);
            }
            log.warn("Configured browser executable not found: {}", configuredExe);
        }

        // Try default browser first
        BrowserExecutable def = detectDefaultChromiumExecutable();
        if (def != null) return def;

        // Fallback to any Chrome
        return findChromeExecutable();
    }

    // ==================== macOS ====================

    static BrowserExecutable detectDefaultChromiumExecutableMac() {
        String bundleId = detectDefaultBrowserBundleIdMac();
        if (bundleId != null && CHROMIUM_BUNDLE_IDS.contains(bundleId)) {
            String appPath = getAppPathForBundleId(bundleId);
            if (appPath != null) {
                String exePath = appPath + "/Contents/MacOS/"
                        + getExecutableNameForBundleId(bundleId);
                if (Files.isExecutable(Path.of(exePath))) {
                    return new BrowserExecutable(inferKindFromBundleId(bundleId), exePath);
                }
            }
        }
        return findChromeExecutableMac();
    }

    static String detectDefaultBrowserBundleIdMac() {
        // Try to detect the default browser via defaults / plutil
        try {
            // macOS Sonoma+ uses ~/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist
            String home = System.getProperty("user.home");
            String plistPath = home + "/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist";
            if (Files.exists(Path.of(plistPath))) {
                ProcessBuilder pb = new ProcessBuilder("plutil", "-convert", "xml1",
                        "-o", "-", plistPath);
                pb.redirectErrorStream(true);
                Process p = pb.start();
                String output = new String(p.getInputStream().readAllBytes());
                p.waitFor();
                // Simple XML parsing — look for http scheme handler
                if (output.contains("LSHandlerURLScheme") && output.contains("LSHandlerRoleAll")) {
                    // Find https handler
                    int httpsIdx = output.indexOf(">https<");
                    if (httpsIdx > 0) {
                        int roleIdx = output.indexOf("LSHandlerRoleAll", httpsIdx);
                        if (roleIdx > 0) {
                            int stringStart = output.indexOf("<string>", roleIdx);
                            int stringEnd = output.indexOf("</string>", stringStart);
                            if (stringStart > 0 && stringEnd > stringStart) {
                                return output.substring(stringStart + 8, stringEnd).trim();
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.debug("Failed to detect default browser on macOS: {}", e.getMessage());
        }

        // Fallback: use defaults command
        try {
            ProcessBuilder pb = new ProcessBuilder("defaults", "read",
                    "com.apple.LaunchServices/com.apple.launchservices.secure",
                    "LSHandlers");
            pb.redirectErrorStream(true);
            Process p = pb.start();
            String output = new String(p.getInputStream().readAllBytes());
            p.waitFor();
            // Look for https handler in the defaults output
            if (output.contains("LSHandlerURLScheme = https")) {
                int idx = output.indexOf("LSHandlerURLScheme = https");
                int roleIdx = output.indexOf("LSHandlerRoleAll = ", idx);
                if (roleIdx > 0) {
                    int start = roleIdx + "LSHandlerRoleAll = ".length();
                    int end = output.indexOf(";", start);
                    if (end > start) {
                        String bundleId = output.substring(start, end).trim()
                                .replace("\"", "").replace("'", "");
                        if (!bundleId.isEmpty()) return bundleId;
                    }
                }
            }
        } catch (Exception e) {
            // ignore
        }

        return null;
    }

    static BrowserExecutable findChromeExecutableMac() {
        List<BrowserExecutable> candidates = new ArrayList<>();

        // Standard application paths
        Map<String, String> appPaths = new LinkedHashMap<>();
        appPaths.put("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "chrome");
        appPaths.put("/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary", "canary");
        appPaths.put("/Applications/Brave Browser.app/Contents/MacOS/Brave Browser", "brave");
        appPaths.put("/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge", "edge");
        appPaths.put("/Applications/Chromium.app/Contents/MacOS/Chromium", "chromium");

        // Also check ~/Applications
        String home = System.getProperty("user.home");
        appPaths.put(home + "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "chrome");
        appPaths.put(home + "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser", "brave");

        for (Map.Entry<String, String> e : appPaths.entrySet()) {
            if (Files.isExecutable(Path.of(e.getKey()))) {
                candidates.add(new BrowserExecutable(e.getValue(), e.getKey()));
            }
        }

        return candidates.isEmpty() ? null : candidates.get(0);
    }

    // ==================== Linux ====================

    static BrowserExecutable detectDefaultChromiumExecutableLinux() {
        return findChromeExecutableLinux();
    }

    static BrowserExecutable findChromeExecutableLinux() {
        String[] cmds = {
                "google-chrome-stable", "google-chrome", "chromium-browser", "chromium",
                "brave-browser", "microsoft-edge-stable", "microsoft-edge"
        };
        String[] kinds = {
                "chrome", "chrome", "chromium", "chromium",
                "brave", "edge", "edge"
        };

        for (int i = 0; i < cmds.length; i++) {
            String resolved = resolveLinuxPath(cmds[i]);
            if (resolved != null) {
                return new BrowserExecutable(kinds[i], resolved);
            }
        }
        return null;
    }

    static BrowserExecutable detectDefaultChromiumExecutableWindows() {
        // Stub — implement when needed
        return null;
    }

    // ==================== Utility ====================

    private static String resolveLinuxPath(String command) {
        try {
            ProcessBuilder pb = new ProcessBuilder("which", command);
            pb.redirectErrorStream(true);
            Process p = pb.start();
            String output = new String(p.getInputStream().readAllBytes()).trim();
            int code = p.waitFor();
            if (code == 0 && !output.isEmpty() && Files.isExecutable(Path.of(output))) {
                return output;
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }

    private static String getAppPathForBundleId(String bundleId) {
        try {
            ProcessBuilder pb = new ProcessBuilder("mdfind",
                    "kMDItemCFBundleIdentifier == '" + bundleId + "'");
            pb.redirectErrorStream(true);
            Process p = pb.start();
            String output = new String(p.getInputStream().readAllBytes()).trim();
            p.waitFor();
            if (!output.isEmpty()) {
                return output.lines().findFirst().orElse(null);
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }

    private static String getExecutableNameForBundleId(String bundleId) {
        return switch (bundleId) {
            case "com.google.Chrome", "com.google.Chrome.beta", "com.google.Chrome.dev" ->
                    "Google Chrome";
            case "com.google.Chrome.canary" -> "Google Chrome Canary";
            case "com.brave.Browser", "com.brave.Browser.beta", "com.brave.Browser.nightly" ->
                    "Brave Browser";
            case "com.microsoft.Edge", "com.microsoft.Edge.Beta",
                 "com.microsoft.Edge.Dev", "com.microsoft.Edge.Canary" -> "Microsoft Edge";
            case "org.chromium.Chromium" -> "Chromium";
            case "com.vivaldi.Vivaldi" -> "Vivaldi";
            case "com.operasoftware.Opera", "com.opera.OperaGX" -> "Opera";
            default -> "Chromium";
        };
    }

    private static String inferKindFromBundleId(String bundleId) {
        if (bundleId.contains("google.Chrome.canary")) return "canary";
        if (bundleId.contains("google.Chrome")) return "chrome";
        if (bundleId.contains("brave")) return "brave";
        if (bundleId.contains("Edge")) return "edge";
        if (bundleId.contains("chromium") || bundleId.contains("Chromium")) return "chromium";
        return "chrome";
    }

    private static String inferKindFromPath(String path) {
        String lower = path.toLowerCase();
        if (lower.contains("canary")) return "canary";
        if (lower.contains("brave")) return "brave";
        if (lower.contains("edge")) return "edge";
        if (lower.contains("chromium")) return "chromium";
        return "chrome";
    }
}

package com.openclaw.browser.chrome;

import com.openclaw.browser.BrowserConfig;
import com.openclaw.browser.BrowserConstants;
import com.openclaw.browser.cdp.CdpHelpers;
import com.openclaw.browser.cdp.CdpTypes;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Chrome process lifecycle manager — launch, stop, and readiness detection.
 * Corresponds to TypeScript's chrome.ts.
 */
@Slf4j
public final class ChromeManager {

    private ChromeManager() {
    }

    /**
     * Launch a Chrome browser instance for the given profile.
     *
     * @param resolved Resolved browser config
     * @param profile  Resolved profile (must be loopback)
     * @return RunningChrome instance
     */
    public static RunningChrome launchChrome(BrowserConfig.ResolvedBrowserConfig resolved,
                                              BrowserConfig.ResolvedBrowserProfile profile)
            throws IOException, InterruptedException {
        if (!profile.isCdpIsLoopback()) {
            throw new IllegalArgumentException(
                    "Profile \"" + profile.getName() + "\" is remote; cannot launch local Chrome.");
        }

        ChromeExecutables.BrowserExecutable exe = ChromeExecutables.resolveBrowserExecutable(
                null); // Auto-detect; user config can be added later
        if (exe == null) {
            throw new IOException(
                    "No supported browser found (Chrome/Brave/Edge/Chromium on macOS, Linux, or Windows).");
        }

        String userDataDir = resolveUserDataDir(profile.getName());
        Files.createDirectories(Path.of(userDataDir));

        // Check if profile needs decoration
        boolean needsDecorate = !ChromeProfileDecoration.isProfileDecorated(
                userDataDir, profile.getName(),
                (profile.getColor() != null ? profile.getColor() : BrowserConstants.DEFAULT_PROFILE_COLOR)
                        .toUpperCase());

        long startedAt = System.currentTimeMillis();
        Path localStatePath = Path.of(userDataDir, "Local State");
        Path preferencesPath = Path.of(userDataDir, "Default", "Preferences");
        boolean needsBootstrap = !Files.exists(localStatePath) || !Files.exists(preferencesPath);

        // Bootstrap run — create preference files if first time
        if (needsBootstrap) {
            Process bootstrap = spawnChrome(exe.path(), profile.getCdpPort(),
                    userDataDir, resolved.isHeadless(), resolved.isNoSandbox());
            long deadline = System.currentTimeMillis() + 10_000;
            while (System.currentTimeMillis() < deadline) {
                if (Files.exists(localStatePath) && Files.exists(preferencesPath)) break;
                Thread.sleep(100);
            }
            try {
                bootstrap.destroy();
            } catch (Exception e) {
                // ignore
            }
            long exitDeadline = System.currentTimeMillis() + 5_000;
            while (System.currentTimeMillis() < exitDeadline) {
                if (!bootstrap.isAlive()) break;
                Thread.sleep(50);
            }
            if (bootstrap.isAlive()) {
                bootstrap.destroyForcibly();
            }
        }

        // Decorate profile if needed
        if (needsDecorate) {
            try {
                ChromeProfileDecoration.decorateProfile(userDataDir, profile.getName(),
                        profile.getColor());
                log.info("🦞 openclaw browser profile decorated ({})", profile.getColor());
            } catch (Exception e) {
                log.warn("openclaw browser profile decoration failed: {}", e.getMessage());
            }
        }

        try {
            ChromeProfileDecoration.ensureCleanExit(userDataDir);
        } catch (Exception e) {
            log.warn("openclaw browser clean-exit prefs failed: {}", e.getMessage());
        }

        // Launch Chrome for real
        Process proc = spawnChrome(exe.path(), profile.getCdpPort(),
                userDataDir, resolved.isHeadless(), resolved.isNoSandbox());

        // Wait for CDP to become available
        String cdpUrl = "http://127.0.0.1:" + profile.getCdpPort();
        long readyDeadline = System.currentTimeMillis() + 15_000;
        boolean ready = false;
        while (System.currentTimeMillis() < readyDeadline) {
            if (isChromeReachable(cdpUrl, 500)) {
                ready = true;
                break;
            }
            Thread.sleep(200);
        }

        if (!ready) {
            try {
                proc.destroyForcibly();
            } catch (Exception e) {
                // ignore
            }
            throw new IOException(
                    "Failed to start Chrome CDP on port " + profile.getCdpPort()
                            + " for profile \"" + profile.getName() + "\".");
        }

        long pid = proc.pid();
        log.info("🦞 openclaw browser started ({}) profile \"{}\" on 127.0.0.1:{} (pid {})",
                exe.kind(), profile.getName(), profile.getCdpPort(), pid);

        return RunningChrome.builder()
                .pid(pid)
                .exe(exe)
                .userDataDir(userDataDir)
                .cdpPort(profile.getCdpPort())
                .startedAt(startedAt)
                .process(proc)
                .build();
    }

    /**
     * Stop a running Chrome instance gracefully.
     */
    public static void stopChrome(RunningChrome running, int timeoutMs) {
        Process proc = running.getProcess();
        if (proc == null || !proc.isAlive()) return;

        try {
            proc.destroy(); // SIGTERM on Unix
        } catch (Exception e) {
            // ignore
        }

        long start = System.currentTimeMillis();
        while (System.currentTimeMillis() - start < timeoutMs) {
            if (!proc.isAlive()) return;
            // Check if CDP is gone
            if (!isChromeReachable("http://127.0.0.1:" + running.getCdpPort(), 200)) {
                return;
            }
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }

        // Force kill if still alive
        try {
            proc.destroyForcibly();
        } catch (Exception e) {
            // ignore
        }
    }

    /**
     * Stop a running Chrome instance with default timeout.
     */
    public static void stopChrome(RunningChrome running) {
        stopChrome(running, 2500);
    }

    /**
     * Check if Chrome is reachable on the given CDP URL.
     */
    public static boolean isChromeReachable(String cdpUrl, int timeoutMs) {
        try {
            String versionUrl = CdpHelpers.appendCdpPath(cdpUrl, "/json/version");
            CdpTypes.ChromeVersion version = CdpHelpers.fetchJson(versionUrl, timeoutMs,
                    CdpTypes.ChromeVersion.class);
            return version != null;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Get Chrome's WebSocket debugger URL.
     */
    public static String getChromeWebSocketUrl(String cdpUrl, int timeoutMs) {
        try {
            String versionUrl = CdpHelpers.appendCdpPath(cdpUrl, "/json/version");
            CdpTypes.ChromeVersion version = CdpHelpers.fetchJson(versionUrl, timeoutMs,
                    CdpTypes.ChromeVersion.class);
            String wsUrl = version != null ? version.getWebSocketDebuggerUrl() : null;
            if (wsUrl == null || wsUrl.isBlank()) return null;
            return CdpHelpers.normalizeCdpWsUrl(wsUrl.trim(), cdpUrl);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Check if Chrome CDP is fully ready (HTTP + WebSocket handshake).
     */
    public static boolean isChromeCdpReady(String cdpUrl, int timeoutMs) {
        String wsUrl = getChromeWebSocketUrl(cdpUrl, timeoutMs);
        return wsUrl != null && !wsUrl.isBlank();
    }

    /**
     * Resolve the user-data directory path for a profile.
     */
    public static String resolveUserDataDir(String profileName) {
        String name = profileName != null ? profileName : BrowserConstants.DEFAULT_PROFILE_NAME;
        String configDir = System.getProperty("user.home") + "/.openclaw";
        return configDir + "/browser/" + name + "/user-data";
    }

    // ==================== Private ====================

    private static Process spawnChrome(String exePath, int cdpPort, String userDataDir,
                                       boolean headless, boolean noSandbox)
            throws IOException {
        List<String> args = new ArrayList<>();
        args.add(exePath);
        args.add("--remote-debugging-port=" + cdpPort);
        args.add("--user-data-dir=" + userDataDir);
        args.add("--no-first-run");
        args.add("--no-default-browser-check");
        args.add("--disable-sync");
        args.add("--disable-background-networking");
        args.add("--disable-component-update");
        args.add("--disable-features=Translate,MediaRouter");
        args.add("--disable-session-crashed-bubble");
        args.add("--hide-crash-restore-bubble");
        args.add("--password-store=basic");

        if (headless) {
            args.add("--headless=new");
            args.add("--disable-gpu");
        }
        if (noSandbox) {
            args.add("--no-sandbox");
            args.add("--disable-setuid-sandbox");
        }

        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("linux")) {
            args.add("--disable-dev-shm-usage");
        }

        // Always open a blank tab to ensure a target exists
        args.add("about:blank");

        ProcessBuilder pb = new ProcessBuilder(args);
        pb.redirectErrorStream(true);
        return pb.start();
    }
}

package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.time.Instant;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Gateway restart triggers — platform-specific restart using launchctl /
 * systemd
 * and SIGUSR1-like authorization windows for coordinated restarts.
 * <p>
 * Corresponds to TypeScript's infra/restart.ts.
 */
public final class Restart {

    private Restart() {
    }

    private static final Logger log = LoggerFactory.getLogger(Restart.class);

    /**
     * Authorization window duration for restart signals (30 seconds).
     */
    public static final long AUTHORIZATION_WINDOW_MS = 30_000;

    /**
     * Pending restart authorization timestamp. If non-null and within the window,
     * the next restart trigger should be honoured.
     */
    private static final AtomicReference<Instant> authorizedAt = new AtomicReference<>(null);

    // =========================================================================
    // Public API
    // =========================================================================

    public enum RestartMethod {
        LAUNCHCTL,
        SYSTEMD,
        EXIT_RESTART,
        UNKNOWN
    }

    /**
     * Result of a restart attempt.
     */
    public record RestartAttempt(boolean triggered, RestartMethod method, String message) {
    }

    /**
     * Authorize a pending restart. The authorization expires after
     * {@link #AUTHORIZATION_WINDOW_MS}.
     */
    public static void authorizeRestart() {
        authorizedAt.set(Instant.now());
        log.info("[openclaw] restart authorized (window: {}s)", AUTHORIZATION_WINDOW_MS / 1000);
    }

    /**
     * Check if a restart is authorized (and consume the authorization).
     */
    public static boolean consumeAuthorization() {
        Instant ts = authorizedAt.getAndSet(null);
        if (ts == null) {
            return false;
        }
        long elapsed = Instant.now().toEpochMilli() - ts.toEpochMilli();
        return elapsed <= AUTHORIZATION_WINDOW_MS;
    }

    /**
     * Check if a restart is currently authorized (without consuming).
     */
    public static boolean isAuthorized() {
        Instant ts = authorizedAt.get();
        if (ts == null) {
            return false;
        }
        long elapsed = Instant.now().toEpochMilli() - ts.toEpochMilli();
        return elapsed <= AUTHORIZATION_WINDOW_MS;
    }

    /**
     * Trigger a gateway restart using platform-specific methods.
     */
    public static RestartAttempt triggerRestart() {
        String os = System.getProperty("os.name", "").toLowerCase();

        if (os.contains("mac")) {
            return tryLaunchctl();
        }
        if (os.contains("linux")) {
            return trySystemd();
        }

        return new RestartAttempt(false, RestartMethod.UNKNOWN,
                "Unsupported platform for automatic restart: " + os);
    }

    /**
     * Trigger a restart by scheduling System.exit with a sentinel so that the
     * outer process manager can re-launch.
     */
    public static RestartAttempt triggerExitRestart() {
        if (!consumeAuthorization()) {
            return new RestartAttempt(false, RestartMethod.EXIT_RESTART,
                    "Restart not authorized. Call authorizeRestart() first.");
        }
        log.info("[openclaw] scheduling exit-restart (exit code 75)");
        // Exit with code 75 (EX_TEMPFAIL) — the wrapper script should restart
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
        }));
        System.exit(75);
        // unreachable
        return new RestartAttempt(true, RestartMethod.EXIT_RESTART, "exit-restart scheduled");
    }

    /**
     * Resolve the restart method for the current platform.
     */
    public static RestartMethod resolveRestartMethod() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac"))
            return RestartMethod.LAUNCHCTL;
        if (os.contains("linux"))
            return RestartMethod.SYSTEMD;
        return RestartMethod.UNKNOWN;
    }

    // =========================================================================
    // Platform-specific restart methods
    // =========================================================================

    private static RestartAttempt tryLaunchctl() {
        try {
            // Try to find the openclaw daemon label
            String serviceName = findLaunchctlService();
            if (serviceName == null) {
                return new RestartAttempt(false, RestartMethod.LAUNCHCTL,
                        "No openclaw launchctl service found");
            }
            ProcessBuilder pb = new ProcessBuilder(
                    "launchctl", "kickstart", "-k", "gui/" + getUid() + "/" + serviceName)
                    .redirectErrorStream(true);
            Process process = pb.start();
            boolean finished = process.waitFor(10, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return new RestartAttempt(false, RestartMethod.LAUNCHCTL, "launchctl timed out");
            }
            if (process.exitValue() == 0) {
                return new RestartAttempt(true, RestartMethod.LAUNCHCTL,
                        "restarted via launchctl: " + serviceName);
            }
            return new RestartAttempt(false, RestartMethod.LAUNCHCTL,
                    "launchctl exited with code " + process.exitValue());
        } catch (Exception e) {
            return new RestartAttempt(false, RestartMethod.LAUNCHCTL,
                    "launchctl failed: " + e.getMessage());
        }
    }

    private static RestartAttempt trySystemd() {
        try {
            ProcessBuilder pb = new ProcessBuilder(
                    "systemctl", "--user", "restart", "openclaw-gateway")
                    .redirectErrorStream(true);
            Process process = pb.start();
            boolean finished = process.waitFor(10, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return new RestartAttempt(false, RestartMethod.SYSTEMD, "systemctl timed out");
            }
            if (process.exitValue() == 0) {
                return new RestartAttempt(true, RestartMethod.SYSTEMD,
                        "restarted via systemd: openclaw-gateway");
            }
            return new RestartAttempt(false, RestartMethod.SYSTEMD,
                    "systemctl exited with code " + process.exitValue());
        } catch (Exception e) {
            return new RestartAttempt(false, RestartMethod.SYSTEMD,
                    "systemd failed: " + e.getMessage());
        }
    }

    private static String findLaunchctlService() {
        try {
            ProcessBuilder pb = new ProcessBuilder("launchctl", "list")
                    .redirectErrorStream(false);
            Process process = pb.start();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (line.contains("openclaw") && line.contains("gateway")) {
                        // Extract the service label (third column)
                        String[] parts = line.trim().split("\\s+");
                        if (parts.length >= 3) {
                            return parts[parts.length - 1];
                        }
                    }
                }
            }
            process.waitFor(5, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.debug("Failed to list launchctl services: {}", e.getMessage());
        }
        return null;
    }

    private static String getUid() {
        try {
            ProcessBuilder pb = new ProcessBuilder("id", "-u")
                    .redirectErrorStream(false);
            Process process = pb.start();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line = reader.readLine();
                process.waitFor(3, TimeUnit.SECONDS);
                return line != null ? line.trim() : "501";
            }
        } catch (Exception e) {
            return "501";
        }
    }

    /**
     * Reset state for testing.
     */
    public static void resetForTest() {
        authorizedAt.set(null);
    }
}

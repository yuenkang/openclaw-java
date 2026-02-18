package com.openclaw.common.infra;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;

/**
 * Runs an update check at gateway startup, throttled to once per day.
 * Persists state to {@code stateDir/update-check.json}.
 * <p>
 * Corresponds to TypeScript's infra/update-startup.ts.
 */
public final class UpdateStartup {

    private UpdateStartup() {
    }

    private static final Logger log = LoggerFactory.getLogger(UpdateStartup.class);
    private static final ObjectMapper mapper = new ObjectMapper();
    private static final String STATE_FILENAME = "update-check.json";
    private static final Duration CHECK_INTERVAL = Duration.ofHours(24);

    // =========================================================================
    // State model
    // =========================================================================

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record State(
            String lastCheckedAt,
            String lastNotifiedVersion,
            String lastNotifiedTag) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Run the startup update check (non-blocking, failures are swallowed).
     *
     * @param stateDir       openclaw state directory
     * @param currentVersion the current running version (e.g. "1.2.3")
     * @param projectRoot    project root for git-based installs
     * @param checkOnStart   whether update check is enabled in config
     * @param configChannel  user-configured update channel (may be null)
     */
    public static void runGatewayUpdateCheck(
            Path stateDir,
            String currentVersion,
            String projectRoot,
            boolean checkOnStart,
            UpdateChannels.Channel configChannel) {
        if (!checkOnStart) {
            return;
        }

        Path statePath = stateDir.resolve(STATE_FILENAME);

        try {
            State state = readState(statePath);

            // Throttle: skip if checked recently
            if (state.lastCheckedAt() != null) {
                Instant lastChecked = Instant.parse(state.lastCheckedAt());
                if (Duration.between(lastChecked, Instant.now()).compareTo(CHECK_INTERVAL) < 0) {
                    return;
                }
            }

            // Perform the check
            UpdateCheck.UpdateCheckResult result = UpdateCheck.checkUpdateStatus(
                    projectRoot, currentVersion, 2500L, false);

            State nextState = new State(
                    Instant.now().toString(),
                    state.lastNotifiedVersion(),
                    state.lastNotifiedTag());

            if (result.installKind() == UpdateCheck.InstallKind.GIT
                    && result.gitStatus() != null
                    && result.gitStatus().isGitRepo()) {
                UpdateCheck.GitStatus git = result.gitStatus();
                if (git.behind() > 0) {
                    log.info("[openclaw] update available: {} commits behind origin/{}",
                            git.behind(), git.branch());
                }
            }

            writeState(statePath, nextState);

        } catch (Exception e) {
            log.debug("update check failed: {}", e.getMessage());
        }
    }

    /**
     * Schedule an async update check (runs in a virtual thread).
     */
    public static void scheduleGatewayUpdateCheck(
            Path stateDir,
            String currentVersion,
            String projectRoot,
            boolean checkOnStart,
            UpdateChannels.Channel configChannel) {
        Thread t = new Thread(() -> {
            try {
                runGatewayUpdateCheck(stateDir, currentVersion, projectRoot,
                        checkOnStart, configChannel);
            } catch (Exception e) {
                // swallow
            }
        }, "update-check");
        t.setDaemon(true);
        t.start();
    }

    // =========================================================================
    // Internals
    // =========================================================================

    static State readState(Path statePath) {
        if (!Files.exists(statePath)) {
            return new State(null, null, null);
        }
        try {
            String raw = Files.readString(statePath);
            return mapper.readValue(raw, State.class);
        } catch (IOException e) {
            return new State(null, null, null);
        }
    }

    static void writeState(Path statePath, State state) throws IOException {
        Files.createDirectories(statePath.getParent());
        String json = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(state);
        Files.writeString(statePath, json);
    }
}

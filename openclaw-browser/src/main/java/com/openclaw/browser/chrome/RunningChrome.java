package com.openclaw.browser.chrome;

import lombok.Builder;
import lombok.Data;

/**
 * Represents a running Chrome browser instance.
 * Corresponds to TypeScript's RunningChrome type in chrome.ts.
 */
@Data
@Builder
public class RunningChrome {
    /** OS process ID of the Chrome browser. */
    private final long pid;
    /** Browser executable used to launch Chrome. */
    private final ChromeExecutables.BrowserExecutable exe;
    /** Path to the user-data-dir. */
    private final String userDataDir;
    /** CDP debugging port. */
    private final int cdpPort;
    /** Timestamp when Chrome was started (epoch millis). */
    private final long startedAt;
    /** The underlying OS process handle. */
    private final Process process;

    /**
     * Check if the Chrome process is still alive.
     */
    public boolean isAlive() {
        return process != null && process.isAlive();
    }

    /**
     * Get the CDP URL for this running instance.
     */
    public String getCdpUrl() {
        return "http://127.0.0.1:" + cdpPort;
    }
}

package com.openclaw.agent.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Subsystem logger for the embedded agent runner.
 * Mirrors {@code agents/pi-embedded-runner/logger.ts}.
 */
public final class RunnerLogger {

    private RunnerLogger() {
    }

    public static final Logger LOG = LoggerFactory.getLogger("agent.embedded");
}

package com.openclaw.gateway.cron;

import lombok.Builder;
import lombok.Data;

import java.util.function.Consumer;

/**
 * Cron service state types and initialization.
 * Corresponds to TypeScript's cron/service/state.ts.
 */
public final class CronState {

    private CronState() {
    }

    // =========================================================================
    // Event types
    // =========================================================================

    /**
     * Cron lifecycle event.
     */
    @Data
    @Builder
    public static class CronEvent {
        private String jobId;
        /** "added" | "updated" | "removed" | "started" | "finished" */
        private String action;
        private Long runAtMs;
        private Long durationMs;
        /** "ok" | "error" | "skipped" */
        private String status;
        private String error;
        private String summary;
        private Long nextRunAtMs;
    }

    // =========================================================================
    // Dependencies
    // =========================================================================

    /**
     * Dependencies for cron service initialization.
     */
    @Data
    @Builder
    public static class CronServiceDeps {
        private String storePath;
        private boolean cronEnabled;
        private String agentId;
        private Consumer<CronEvent> onEvent;
    }

    // =========================================================================
    // Result types
    // =========================================================================

    /** Result of running a cron job. */
    public sealed interface CronRunResult {
        boolean ok();
    }

    public record CronRanOk() implements CronRunResult {
        @Override
        public boolean ok() {
            return true;
        }
    }

    public record CronNotDue(String reason) implements CronRunResult {
        @Override
        public boolean ok() {
            return true;
        }
    }

    public record CronRunFailed(Exception error) implements CronRunResult {
        @Override
        public boolean ok() {
            return false;
        }
    }

    /** Result of removing a cron job. */
    public record CronRemoveResult(boolean ok, boolean removed) {
    }

    // =========================================================================
    // Enums
    // =========================================================================

    public enum CronRunMode {
        DUE, FORCE
    }

    public enum CronWakeMode {
        NOW, NEXT_HEARTBEAT
    }

    // =========================================================================
    // Status
    // =========================================================================

    /**
     * Cron status summary for diagnostics.
     */
    @Data
    @Builder
    public static class CronStatusSummary {
        private boolean enabled;
        private String storePath;
        private int jobs;
        private Long nextWakeAtMs;
    }
}

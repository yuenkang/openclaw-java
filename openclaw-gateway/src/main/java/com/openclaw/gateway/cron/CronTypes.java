package com.openclaw.gateway.cron;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Rich cron job type definitions.
 * Corresponds to TypeScript's cron/types.ts.
 *
 * <p>
 * These complement the simpler {@link CronJob} model with schedule variants,
 * typed payloads, delivery configuration, and create/patch DTOs.
 * </p>
 */
public final class CronTypes {

    private CronTypes() {
    }

    // =========================================================================
    // Schedule
    // =========================================================================

    public enum ScheduleKind {
        AT, EVERY, CRON
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronSchedule {
        private ScheduleKind kind;
        /** ISO-8601 timestamp for "at" schedules. */
        private String at;
        /** Interval in milliseconds for "every" schedules. */
        private Long everyMs;
        /** Anchor timestamp in ms for "every" schedules. */
        private Long anchorMs;
        /** Cron expression for "cron" schedules (e.g. "0 0 * * *"). */
        private String expr;
        /** IANA time zone for "cron" schedules. */
        private String tz;
    }

    // =========================================================================
    // Session/wake modes
    // =========================================================================

    public enum SessionTarget {
        MAIN, ISOLATED;

        public String key() {
            return name().toLowerCase();
        }
    }

    public enum WakeMode {
        NEXT_HEARTBEAT, NOW;

        public String key() {
            return name().toLowerCase().replace('_', '-');
        }

        public static WakeMode fromKey(String key) {
            if ("now".equalsIgnoreCase(key))
                return NOW;
            return NEXT_HEARTBEAT;
        }
    }

    // =========================================================================
    // Payload
    // =========================================================================

    public enum PayloadKind {
        SYSTEM_EVENT, AGENT_TURN;

        public String key() {
            return this == SYSTEM_EVENT ? "systemEvent" : "agentTurn";
        }

        public static PayloadKind fromKey(String key) {
            if ("agentTurn".equalsIgnoreCase(key))
                return AGENT_TURN;
            return SYSTEM_EVENT;
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronPayload {
        private PayloadKind kind;
        /** Text for systemEvent payloads, message for agentTurn. */
        private String text;
        private String message;
        private String model;
        private String thinking;
        private Integer timeoutSeconds;
        private Boolean allowUnsafeExternalContent;
    }

    // =========================================================================
    // Delivery
    // =========================================================================

    public enum DeliveryMode {
        NONE, ANNOUNCE;

        public String key() {
            return name().toLowerCase();
        }

        public static DeliveryMode fromKey(String key) {
            if ("announce".equalsIgnoreCase(key) || "deliver".equalsIgnoreCase(key))
                return ANNOUNCE;
            return NONE;
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronDelivery {
        private DeliveryMode mode;
        private String channel;
        private String to;
        private Boolean bestEffort;
    }

    // =========================================================================
    // Job state
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronJobState {
        private Long nextRunAtMs;
        private Long runningAtMs;
        private Long lastRunAtMs;
        /** "ok", "error", or "skipped" */
        private String lastStatus;
        private String lastError;
        private Long lastDurationMs;
    }

    // =========================================================================
    // Rich job model
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RichCronJob {
        private String id;
        private String agentId;
        private String name;
        private String description;
        private boolean enabled;
        private Boolean deleteAfterRun;
        private long createdAtMs;
        private long updatedAtMs;
        private CronSchedule schedule;
        private SessionTarget sessionTarget;
        private WakeMode wakeMode;
        private CronPayload payload;
        private CronDelivery delivery;
        private CronJobState state;
    }

    // =========================================================================
    // Create/Patch DTOs
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronJobCreate {
        private String agentId;
        private String name;
        private String description;
        private boolean enabled;
        private Boolean deleteAfterRun;
        private CronSchedule schedule;
        private SessionTarget sessionTarget;
        private WakeMode wakeMode;
        private CronPayload payload;
        private CronDelivery delivery;
        private Map<String, Object> stateOverrides;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronJobPatch {
        private String agentId;
        private String name;
        private String description;
        private Boolean enabled;
        private Boolean deleteAfterRun;
        private CronSchedule schedule;
        private SessionTarget sessionTarget;
        private WakeMode wakeMode;
        private CronPayload payload;
        private CronDelivery delivery;
        private Map<String, Object> stateOverrides;
    }

    // =========================================================================
    // Store format
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CronStoreFile {
        @Builder.Default
        private int version = 1;
        private java.util.List<RichCronJob> jobs;
    }
}

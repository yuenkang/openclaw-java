package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Cron job protocol schemas — schedule, payload, delivery, CRUD.
 * Mirrors {@code protocol/schema/cron.ts}.
 */
public final class CronSchemas {

    private CronSchemas() {
    }

    // --- Schedule variants ---

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronSchedule {
        private String kind; // "at" | "every" | "cron"
        private String at;
        private Long everyMs;
        private Long anchorMs;
        private String expr;
        private String tz;
    }

    // --- Payload variants ---

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronPayload {
        private String kind; // "systemEvent" | "agentTurn"
        private String text;
        private String message;
        private String model;
        private String thinking;
        private Integer timeoutSeconds;
    }

    // --- Delivery ---

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronDelivery {
        private String mode; // "none" | "announce"
        private String channel;
        private String to;
        private Boolean bestEffort;
    }

    // --- Job state ---

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronJobState {
        private Long nextRunAtMs;
        private Long runningAtMs;
        private Long lastRunAtMs;
        private String lastStatus; // "ok" | "error" | "skipped"
        private String lastError;
        private Long lastDurationMs;
    }

    // --- CronJob ---

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronJob {
        private String id;
        private String agentId;
        private String name;
        private String description;
        private boolean enabled;
        private Boolean deleteAfterRun;
        private long createdAtMs;
        private long updatedAtMs;
        private CronSchedule schedule;
        private String sessionTarget; // "main" | "isolated"
        private String wakeMode; // "next-heartbeat" | "now"
        private CronPayload payload;
        private CronDelivery delivery;
        private CronJobState state;
    }

    // --- CRUD Params ---

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronListParams {
        private Boolean includeDisabled;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronAddParams {
        private String name;
        private String agentId;
        private String description;
        private Boolean enabled;
        private Boolean deleteAfterRun;
        private CronSchedule schedule;
        private String sessionTarget;
        private String wakeMode;
        private CronPayload payload;
        private CronDelivery delivery;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronUpdateParams {
        private String id;
        private String jobId;
        private Map<String, Object> patch;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronRemoveParams {
        private String id;
        private String jobId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronRunParams {
        private String id;
        private String jobId;
        private String mode; // "due" | "force"
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronRunsParams {
        private String id;
        private String jobId;
        private Integer limit;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CronRunLogEntry {
        private long ts;
        private String jobId;
        private String action; // "finished"
        private String status; // "ok" | "error" | "skipped"
        private String error;
        private String summary;
        private Long runAtMs;
        private Long durationMs;
        private Long nextRunAtMs;
    }
}

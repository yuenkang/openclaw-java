package com.openclaw.gateway.cron;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

/**
 * Record of a cron job execution.
 * Corresponds to TypeScript's cron/run-log.ts.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CronRunLog {
    private String jobId;
    private String jobName;
    private Instant startedAt;
    private Instant finishedAt;
    private long durationMs;
    private boolean success;
    private String agentId;
    private String message;
    private String error;
}

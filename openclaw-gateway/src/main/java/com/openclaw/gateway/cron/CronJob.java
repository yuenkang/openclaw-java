package com.openclaw.gateway.cron;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

/**
 * Cron job definition.
 * Corresponds to TypeScript's cron/types.ts.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CronJob {
    private String id;
    private String name;
    private String schedule; // cron expression, e.g. "0 */6 * * *"
    private String agentId;
    private String message; // message to send to agent when triggered
    private String channelId;
    private String accountId;
    @Builder.Default
    private boolean enabled = true;

    private Instant lastRun;
    private Instant nextRun;
    private String lastRunStatus; // "success" | "error" | null
    private String lastRunError;
    private int runCount;
}

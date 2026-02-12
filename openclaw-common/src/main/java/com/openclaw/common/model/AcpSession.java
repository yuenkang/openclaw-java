package com.openclaw.common.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents an ACP session.
 * Corresponds to TypeScript's AcpSession (acp/types.ts).
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AcpSession {

    private String sessionId;
    private String sessionKey;
    private String cwd;
    private long createdAt;
    private long updatedAt;
    private String activeRunId;
    private volatile boolean cancelled;

    // Session metadata (aligned with TS SessionEntry)
    private String kind; // "direct" | "group" | "global"
    private String label;
    private String channel;
    private String agentId;

    // Model configuration
    private String model;
    private String modelProvider;
    private Integer contextTokens;
    private String thinkingLevel;

    // Token usage
    private long inputTokens;
    private long outputTokens;
    private long totalTokens;

    // File path for JSONL transcript
    private String sessionFile;

    /**
     * Mark this session's active run as cancelled.
     */
    public boolean cancel() {
        if (activeRunId == null) {
            return false;
        }
        this.cancelled = true;
        return true;
    }

    /**
     * Start a new run in this session.
     */
    public void startRun(String runId) {
        this.activeRunId = runId;
        this.cancelled = false;
    }

    /**
     * End the current run.
     */
    public void endRun() {
        this.activeRunId = null;
        this.cancelled = false;
    }
}

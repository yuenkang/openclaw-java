package com.openclaw.agent.runtime;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.List;

/**
 * Run parameters for the embedded runner.
 * Corresponds to TypeScript run/params.ts (RunEmbeddedPiAgentParams).
 *
 * <p>
 * This is a data holder for all configuration that flows into a single agent
 * run attempt.
 * </p>
 */
public record RunParams(
        String sessionId,
        String sessionKey,
        String messageChannel,
        String messageProvider,
        String agentAccountId,
        /** Delivery target for topic/thread routing. */
        String messageTo,
        /** Thread/topic identifier. */
        String messageThreadId,
        String groupId,
        String groupChannel,
        String groupSpace,
        /** Parent session key for subagent policy inheritance. */
        String spawnedBy,
        String senderId,
        String senderName,
        String senderUsername,
        boolean senderIsOwner,
        String currentChannelId,
        String currentThreadTs,
        /** Reply-to mode for auto-threading. */
        String replyToMode,
        boolean requireExplicitMessageTarget,
        boolean disableMessageTool,
        String sessionFile,
        String workspaceDir,
        String agentDir,
        String prompt,
        /** Optional client-provided tool definitions (OpenResponses hosted tools). */
        List<JsonNode> clientTools,
        /** Disable built-in tools for this run (LLM-only mode). */
        boolean disableTools,
        String provider,
        String model,
        String authProfileId,
        String authProfileIdSource,
        String thinkLevel,
        String verboseLevel,
        String reasoningLevel,
        String toolResultFormat,
        long timeoutMs,
        String runId,
        String lane,
        String extraSystemPrompt,
        List<String> ownerNumbers,
        boolean enforceFinalTag) {
    /**
     * Builder for ergonomic construction.
     */
    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private String sessionId;
        private String sessionKey;
        private String messageChannel;
        private String messageProvider;
        private String agentAccountId;
        private String messageTo;
        private String messageThreadId;
        private String groupId;
        private String groupChannel;
        private String groupSpace;
        private String spawnedBy;
        private String senderId;
        private String senderName;
        private String senderUsername;
        private boolean senderIsOwner;
        private String currentChannelId;
        private String currentThreadTs;
        private String replyToMode;
        private boolean requireExplicitMessageTarget;
        private boolean disableMessageTool;
        private String sessionFile;
        private String workspaceDir;
        private String agentDir;
        private String prompt;
        private List<JsonNode> clientTools;
        private boolean disableTools;
        private String provider;
        private String model;
        private String authProfileId;
        private String authProfileIdSource;
        private String thinkLevel;
        private String verboseLevel;
        private String reasoningLevel;
        private String toolResultFormat;
        private long timeoutMs;
        private String runId;
        private String lane;
        private String extraSystemPrompt;
        private List<String> ownerNumbers;
        private boolean enforceFinalTag;

        public Builder sessionId(String v) {
            this.sessionId = v;
            return this;
        }

        public Builder sessionKey(String v) {
            this.sessionKey = v;
            return this;
        }

        public Builder messageChannel(String v) {
            this.messageChannel = v;
            return this;
        }

        public Builder messageProvider(String v) {
            this.messageProvider = v;
            return this;
        }

        public Builder agentAccountId(String v) {
            this.agentAccountId = v;
            return this;
        }

        public Builder messageTo(String v) {
            this.messageTo = v;
            return this;
        }

        public Builder messageThreadId(String v) {
            this.messageThreadId = v;
            return this;
        }

        public Builder groupId(String v) {
            this.groupId = v;
            return this;
        }

        public Builder groupChannel(String v) {
            this.groupChannel = v;
            return this;
        }

        public Builder groupSpace(String v) {
            this.groupSpace = v;
            return this;
        }

        public Builder spawnedBy(String v) {
            this.spawnedBy = v;
            return this;
        }

        public Builder senderId(String v) {
            this.senderId = v;
            return this;
        }

        public Builder senderName(String v) {
            this.senderName = v;
            return this;
        }

        public Builder senderUsername(String v) {
            this.senderUsername = v;
            return this;
        }

        public Builder senderIsOwner(boolean v) {
            this.senderIsOwner = v;
            return this;
        }

        public Builder currentChannelId(String v) {
            this.currentChannelId = v;
            return this;
        }

        public Builder currentThreadTs(String v) {
            this.currentThreadTs = v;
            return this;
        }

        public Builder replyToMode(String v) {
            this.replyToMode = v;
            return this;
        }

        public Builder requireExplicitMessageTarget(boolean v) {
            this.requireExplicitMessageTarget = v;
            return this;
        }

        public Builder disableMessageTool(boolean v) {
            this.disableMessageTool = v;
            return this;
        }

        public Builder sessionFile(String v) {
            this.sessionFile = v;
            return this;
        }

        public Builder workspaceDir(String v) {
            this.workspaceDir = v;
            return this;
        }

        public Builder agentDir(String v) {
            this.agentDir = v;
            return this;
        }

        public Builder prompt(String v) {
            this.prompt = v;
            return this;
        }

        public Builder clientTools(List<JsonNode> v) {
            this.clientTools = v;
            return this;
        }

        public Builder disableTools(boolean v) {
            this.disableTools = v;
            return this;
        }

        public Builder provider(String v) {
            this.provider = v;
            return this;
        }

        public Builder model(String v) {
            this.model = v;
            return this;
        }

        public Builder authProfileId(String v) {
            this.authProfileId = v;
            return this;
        }

        public Builder authProfileIdSource(String v) {
            this.authProfileIdSource = v;
            return this;
        }

        public Builder thinkLevel(String v) {
            this.thinkLevel = v;
            return this;
        }

        public Builder verboseLevel(String v) {
            this.verboseLevel = v;
            return this;
        }

        public Builder reasoningLevel(String v) {
            this.reasoningLevel = v;
            return this;
        }

        public Builder toolResultFormat(String v) {
            this.toolResultFormat = v;
            return this;
        }

        public Builder timeoutMs(long v) {
            this.timeoutMs = v;
            return this;
        }

        public Builder runId(String v) {
            this.runId = v;
            return this;
        }

        public Builder lane(String v) {
            this.lane = v;
            return this;
        }

        public Builder extraSystemPrompt(String v) {
            this.extraSystemPrompt = v;
            return this;
        }

        public Builder ownerNumbers(List<String> v) {
            this.ownerNumbers = v;
            return this;
        }

        public Builder enforceFinalTag(boolean v) {
            this.enforceFinalTag = v;
            return this;
        }

        public RunParams build() {
            return new RunParams(
                    sessionId, sessionKey, messageChannel, messageProvider,
                    agentAccountId, messageTo, messageThreadId,
                    groupId, groupChannel, groupSpace, spawnedBy,
                    senderId, senderName, senderUsername, senderIsOwner,
                    currentChannelId, currentThreadTs, replyToMode,
                    requireExplicitMessageTarget, disableMessageTool,
                    sessionFile, workspaceDir, agentDir, prompt,
                    clientTools, disableTools, provider, model,
                    authProfileId, authProfileIdSource,
                    thinkLevel, verboseLevel, reasoningLevel, toolResultFormat,
                    timeoutMs, runId, lane, extraSystemPrompt,
                    ownerNumbers, enforceFinalTag);
        }
    }
}

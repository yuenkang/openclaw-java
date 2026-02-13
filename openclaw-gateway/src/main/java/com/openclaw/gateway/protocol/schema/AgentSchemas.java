package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Agent-related protocol schemas — agent params, events, identity,
 * send/poll/wake.
 * Mirrors {@code protocol/schema/agent.ts}.
 */
public final class AgentSchemas {

    private AgentSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AgentEvent {
        private String runId;
        private int seq;
        private String stream;
        private long ts;
        private Map<String, Object> data;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SendParams {
        private String to;
        private String message;
        private String mediaUrl;
        private List<String> mediaUrls;
        private Boolean gifPlayback;
        private String channel;
        private String accountId;
        private String sessionKey;
        private String idempotencyKey;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class PollParams {
        private String to;
        private String question;
        private List<String> options;
        private Integer maxSelections;
        private Integer durationHours;
        private String channel;
        private String accountId;
        private String idempotencyKey;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AgentParams {
        private String message;
        private String agentId;
        private String to;
        private String replyTo;
        private String sessionId;
        private String sessionKey;
        private String thinking;
        private Boolean deliver;
        private List<Object> attachments;
        private String channel;
        private String replyChannel;
        private String accountId;
        private String replyAccountId;
        private String threadId;
        private String groupId;
        private String groupChannel;
        private String groupSpace;
        private Integer timeout;
        private String lane;
        private String extraSystemPrompt;
        private String idempotencyKey;
        private String label;
        private String spawnedBy;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AgentIdentityParams {
        private String agentId;
        private String sessionKey;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AgentIdentityResult {
        private String agentId;
        private String name;
        private String avatar;
        private String emoji;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AgentWaitParams {
        private String runId;
        private Integer timeoutMs;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class WakeParams {
        private String mode; // "now" | "next-heartbeat"
        private String text;
    }
}

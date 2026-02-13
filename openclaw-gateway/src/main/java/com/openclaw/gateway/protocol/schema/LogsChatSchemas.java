package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Logs tail and WebSocket chat protocol schemas.
 * Mirrors {@code protocol/schema/logs-chat.ts}.
 */
public final class LogsChatSchemas {

    private LogsChatSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class LogsTailParams {
        private Integer cursor;
        private Integer limit;
        private Integer maxBytes;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class LogsTailResult {
        private String file;
        private int cursor;
        private int size;
        private List<String> lines;
        private Boolean truncated;
        private Boolean reset;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChatHistoryParams {
        private String sessionKey;
        private Integer limit;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChatSendParams {
        private String sessionKey;
        private String message;
        private String thinking;
        private Boolean deliver;
        private List<Object> attachments;
        private Integer timeoutMs;
        private String idempotencyKey;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChatAbortParams {
        private String sessionKey;
        private String runId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChatInjectParams {
        private String sessionKey;
        private String message;
        private String label;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChatEvent {
        private String runId;
        private String sessionKey;
        private int seq;
        private String state; // "delta" | "final" | "aborted" | "error"
        private Object message;
        private String errorMessage;
        private Object usage;
        private String stopReason;
    }
}

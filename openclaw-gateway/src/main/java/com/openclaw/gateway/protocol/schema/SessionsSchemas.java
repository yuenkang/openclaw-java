package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Session management protocol schemas.
 * Mirrors {@code protocol/schema/sessions.ts}.
 */
public final class SessionsSchemas {

    private SessionsSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsListParams {
        private Integer limit;
        private Integer activeMinutes;
        private Boolean includeGlobal;
        private Boolean includeUnknown;
        private Boolean includeDerivedTitles;
        private Boolean includeLastMessage;
        private String label;
        private String spawnedBy;
        private String agentId;
        private String search;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsPreviewParams {
        private java.util.List<String> keys;
        private Integer limit;
        private Integer maxChars;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsResolveParams {
        private String key;
        private String sessionId;
        private String label;
        private String agentId;
        private String spawnedBy;
        private Boolean includeGlobal;
        private Boolean includeUnknown;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsPatchParams {
        private String key;
        private String label;
        private String thinkingLevel;
        private String verboseLevel;
        private String reasoningLevel;
        private String responseUsage; // "off" | "tokens" | "full" | "on" | null
        private String elevatedLevel;
        private String execHost;
        private String execSecurity;
        private String execAsk;
        private String execNode;
        private String model;
        private String spawnedBy;
        private String sendPolicy; // "allow" | "deny" | null
        private String groupActivation; // "mention" | "always" | null
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SessionsResetParams {
        private String key;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsDeleteParams {
        private String key;
        private Boolean deleteTranscript;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsCompactParams {
        private String key;
        private Integer maxLines;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionsUsageParams {
        private String key;
        private String startDate;
        private String endDate;
        private Integer limit;
        private Boolean includeContextWeight;
    }
}

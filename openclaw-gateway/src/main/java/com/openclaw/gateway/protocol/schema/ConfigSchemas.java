package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Config and update-run protocol schemas.
 * Mirrors {@code protocol/schema/config.ts}.
 */
public final class ConfigSchemas {

    private ConfigSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ConfigSetParams {
        private String raw;
        private String baseHash;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ConfigApplyParams {
        private String raw;
        private String baseHash;
        private String sessionKey;
        private String note;
        private Integer restartDelayMs;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ConfigPatchParams {
        private String raw;
        private String baseHash;
        private String sessionKey;
        private String note;
        private Integer restartDelayMs;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ConfigUiHint {
        private String label;
        private String help;
        private String group;
        private Integer order;
        private Boolean advanced;
        private Boolean sensitive;
        private String placeholder;
        private Object itemTemplate;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ConfigSchemaResponse {
        private Object schema;
        private java.util.Map<String, ConfigUiHint> uiHints;
        private String version;
        private String generatedAt;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class UpdateRunParams {
        private String sessionKey;
        private String note;
        private Integer restartDelayMs;
        private Integer timeoutMs;
    }
}

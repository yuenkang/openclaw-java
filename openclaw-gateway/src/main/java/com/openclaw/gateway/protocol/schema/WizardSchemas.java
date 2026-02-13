package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Wizard flow protocol schemas — start/next/cancel/status, steps, results.
 * Mirrors {@code protocol/schema/wizard.ts}.
 */
public final class WizardSchemas {

    private WizardSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardStartParams {
        private String mode; // "local" | "remote"
        private String workspace;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardAnswer {
        private String stepId;
        private Object value;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardNextParams {
        private String sessionId;
        private WizardAnswer answer;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class WizardCancelParams {
        private String sessionId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class WizardStatusParams {
        private String sessionId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardStepOption {
        private Object value;
        private String label;
        private String hint;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardStep {
        private String id;
        private String type; // "note" | "select" | "text" | "confirm" | "multiselect" | "progress" |
                             // "action"
        private String title;
        private String message;
        private List<WizardStepOption> options;
        private Object initialValue;
        private String placeholder;
        private Boolean sensitive;
        private String executor; // "gateway" | "client"
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardNextResult {
        private boolean done;
        private WizardStep step;
        private String status; // "running" | "done" | "cancelled" | "error"
        private String error;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardStartResult {
        private String sessionId;
        private boolean done;
        private WizardStep step;
        private String status;
        private String error;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WizardStatusResult {
        private String status;
        private String error;
    }
}

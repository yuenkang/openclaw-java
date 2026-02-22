package com.openclaw.browser.cdp;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * CDP protocol types.
 * Corresponds to TypeScript types in cdp.ts.
 */
public final class CdpTypes {

    private CdpTypes() {
    }

    // ==================== Eval result types ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CdpRemoteObject {
        private String type;
        private String subtype;
        private Object value;
        private String description;
        private String unserializableValue;
        private Object preview;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CdpExceptionDetails {
        private String text;
        private Integer lineNumber;
        private Integer columnNumber;
        private CdpRemoteObject exception;
        private Object stackTrace;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CdpEvalResult {
        private CdpRemoteObject result;
        private CdpExceptionDetails exceptionDetails;
    }

    // ==================== Aria snapshot types ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AriaSnapshotNode {
        private String ref;
        private String role;
        private String name;
        private String value;
        private String description;
        private Integer backendDOMNodeId;
        private int depth;
    }

    /** Raw accessibility node from Accessibility.getFullAXTree. */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class RawAXNode {
        private String nodeId;
        private AXValue role;
        private AXValue name;
        private AXValue value;
        private AXValue description;
        private List<String> childIds;
        private Integer backendDOMNodeId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class AXValue {
        private Object value;

        public String getStringValue() {
            if (value == null) return "";
            if (value instanceof String s) return s;
            return String.valueOf(value);
        }
    }

    // ==================== DOM snapshot types ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class DomSnapshotNode {
        private String ref;
        private String parentRef;
        private int depth;
        private String tag;
        private String id;
        private String className;
        private String role;
        private String name;
        private String text;
        private String href;
        private String type;
        private String value;
    }

    // ==================== Query match ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class QueryMatch {
        private int index;
        private String tag;
        private String id;
        private String className;
        private String text;
        private String value;
        private String href;
        private String outerHTML;
    }

    // ==================== Chrome version ====================

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ChromeVersion {
        private String webSocketDebuggerUrl;
        private String Browser;
        private String userAgent;
    }

    // ==================== Screenshot ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ScreenshotResult {
        private byte[] data;
        private String format;
    }
}

package com.openclaw.agent.tools.builtin.browser;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Browser control DTO types.
 * Corresponds to TypeScript's browser/client.ts types.
 */
public final class BrowserTypes {

    private BrowserTypes() {
    }

    // ===== Status =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class BrowserStatus {
        private boolean enabled;
        private String profile;
        private boolean running;
        private Boolean cdpReady;
        private Boolean cdpHttp;
        private Integer pid;
        private int cdpPort;
        private String cdpUrl;
        private String chosenBrowser;
        private String detectedBrowser;
        private String detectedExecutablePath;
        private String detectError;
        private String userDataDir;
        private String color;
        private boolean headless;
        private Boolean noSandbox;
        private String executablePath;
        private boolean attachOnly;
    }

    // ===== Profile =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ProfileStatus {
        private String name;
        private int cdpPort;
        private String cdpUrl;
        private String color;
        private boolean running;
        private int tabCount;
        private boolean isDefault;
        private boolean isRemote;
    }

    // ===== Tab =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class BrowserTab {
        private String targetId;
        private String title;
        private String url;
        private String wsUrl;
        private String type;
    }

    // ===== Snapshot =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SnapshotResult {
        private boolean ok;
        /** "aria" | "ai" */
        private String format;
        private String targetId;
        private String url;
        /** For aria format: list of ARIA nodes */
        private List<SnapshotAriaNode> nodes;
        /** For ai format: page snapshot text */
        private String snapshot;
        private Boolean truncated;
        private Integer lines;
        private Integer chars;
        private Integer refs;
        @JsonProperty("interactive")
        private Integer interactiveCount;
        private Boolean labels;
        private Integer labelsCount;
        private Integer labelsSkipped;
        private String imagePath;
        /** "png" | "jpeg" */
        private String imageType;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SnapshotAriaNode {
        private String ref;
        private String role;
        private String name;
        private String value;
        private String description;
        private Integer backendDOMNodeId;
        private int depth;
    }

    // ===== Act =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class BrowserActRequest {
        /** click|type|press|hover|drag|select|fill|resize|wait|evaluate|close */
        private String kind;
        private String targetId;
        private String ref;
        // click
        private Boolean doubleClick;
        private String button;
        private List<String> modifiers;
        // type
        private String text;
        private Boolean submit;
        private Boolean slowly;
        // press
        private String key;
        // drag
        private String startRef;
        private String endRef;
        // select
        private List<String> values;
        // fill
        private List<Map<String, Object>> fields;
        // resize
        private Integer width;
        private Integer height;
        // wait
        private Integer timeMs;
        private String textGone;
        // evaluate
        private String fn;
    }

    // ===== Screenshot =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ScreenshotResult {
        private boolean ok;
        private String path;
        private String targetId;
        /** "png" | "jpeg" */
        private String type;
    }

    // ===== Navigate =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class NavigateResult {
        private boolean ok;
        private String targetId;
        private String url;
    }

    // ===== Console =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ConsoleMessage {
        private String type;
        private String text;
        private String timestamp;
        private String url;
        private Integer lineNumber;
        private Integer columnNumber;
    }

    // ===== PDF =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class PdfResult {
        private boolean ok;
        private String path;
    }

    // ===== Profile management =====

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ResetProfileResult {
        private boolean ok;
        private boolean moved;
        private String from;
        private String to;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CreateProfileResult {
        private boolean ok;
        private String profile;
        private int cdpPort;
        private String cdpUrl;
        private String color;
        private boolean isRemote;
    }

    // ===== Constants =====

    public static final int DEFAULT_AI_SNAPSHOT_MAX_CHARS = 80_000;
    public static final int DEFAULT_AI_SNAPSHOT_EFFICIENT_MAX_CHARS = 10_000;
    public static final int DEFAULT_AI_SNAPSHOT_EFFICIENT_DEPTH = 6;
    public static final String DEFAULT_BROWSER_COLOR = "#FF4500";
    public static final String DEFAULT_PROFILE_NAME = "chrome";
}

package com.openclaw.browser.relay;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Extension Relay protocol types.
 * Must be wire-compatible with TypeScript Chrome Extension messages in extension-relay.ts.
 *
 * <p>Protocol overview:
 * <ul>
 *   <li>Extension → Relay: {@link ExtensionResponseMessage}, {@link ExtensionForwardEventMessage}, {@link ExtensionPongMessage}</li>
 *   <li>Relay → Extension: {@link ExtensionForwardCommandMessage}, {@link ExtensionPingMessage}</li>
 *   <li>CDP Client → Relay (via /cdp): standard CDP JSON-RPC commands</li>
 *   <li>Relay → CDP Client: standard CDP JSON-RPC responses + events</li>
 * </ul>
 */
public final class ExtensionRelayTypes {

    private ExtensionRelayTypes() {
    }

    public static final String RELAY_AUTH_HEADER = "x-openclaw-relay-token";

    // ==================== Relay → Extension ====================

    /** Command forwarded to the Chrome Extension. */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExtensionForwardCommandMessage {
        private int id;
        private String method; // always "forwardCDPCommand"
        private ForwardParams params;

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        @JsonInclude(JsonInclude.Include.NON_NULL)
        public static class ForwardParams {
            private String method;    // CDP method e.g. "Runtime.evaluate"
            private Object params;    // CDP params
            private String sessionId; // optional CDP session
        }

        public static ExtensionForwardCommandMessage create(int id, String cdpMethod,
                                                             Object cdpParams, String sessionId) {
            return ExtensionForwardCommandMessage.builder()
                    .id(id)
                    .method("forwardCDPCommand")
                    .params(ForwardParams.builder()
                            .method(cdpMethod)
                            .params(cdpParams)
                            .sessionId(sessionId)
                            .build())
                    .build();
        }
    }

    /** Ping sent to extension for keepalive. */
    @Data
    @NoArgsConstructor
    public static class ExtensionPingMessage {
        private final String method = "ping";
    }

    // ==================== Extension → Relay ====================

    /** Response from extension for a forwarded CDP command. */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExtensionResponseMessage {
        private int id;
        private Object result;
        private String error;
    }

    /** CDP event forwarded from extension. */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExtensionForwardEventMessage {
        private String method; // always "forwardCDPEvent"
        private EventParams params;

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        @JsonIgnoreProperties(ignoreUnknown = true)
        @JsonInclude(JsonInclude.Include.NON_NULL)
        public static class EventParams {
            private String method;    // CDP event method
            private Object params;    // CDP event params
            private String sessionId;
        }
    }

    /** Pong response from extension. */
    @Data
    @NoArgsConstructor
    public static class ExtensionPongMessage {
        private final String method = "pong";
    }

    // ==================== CDP types ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CdpCommand {
        private int id;
        private String method;
        private Object params;
        private String sessionId;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CdpResponse {
        private int id;
        private Object result;
        private CdpError error;
        private String sessionId;

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class CdpError {
            private String message;
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class CdpEvent {
        private String method;
        private Object params;
        private String sessionId;
    }

    // ==================== Target tracking ====================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class TargetInfo {
        private String targetId;
        private String type;
        private String title;
        private String url;
        private Boolean attached;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ConnectedTarget {
        private String sessionId;
        private String targetId;
        private TargetInfo targetInfo;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class AttachedToTargetEvent {
        private String sessionId;
        private TargetInfo targetInfo;
        private Boolean waitingForDebugger;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DetachedFromTargetEvent {
        private String sessionId;
        private String targetId;
    }
}

package com.openclaw.gateway.protocol;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Gateway WebSocket protocol types.
 * Aligns with TypeScript protocol/schema/frames.ts.
 *
 * <p>
 * The gateway protocol uses three frame types:
 * <ul>
 * <li>{@code req} – client→server request</li>
 * <li>{@code res} – server→client response</li>
 * <li>{@code event} – server→client push event</li>
 * </ul>
 */
public final class ProtocolTypes {

    private ProtocolTypes() {
    }

    /** Current protocol version. Must match TypeScript PROTOCOL_VERSION (= 3). */
    public static final int PROTOCOL_VERSION = 3;

    /** Maximum WebSocket payload bytes. */
    public static final int MAX_PAYLOAD_BYTES = 10 * 1024 * 1024; // 10 MB
    /** Maximum buffered bytes per connection. */
    public static final int MAX_BUFFERED_BYTES = 20 * 1024 * 1024; // 20 MB
    /** Tick heartbeat interval in ms. */
    public static final int TICK_INTERVAL_MS = 30_000;
    /** Handshake timeout in ms. */
    public static final int HANDSHAKE_TIMEOUT_MS = 10_000;

    // ── Error Codes ──────────────────────────────────────────────

    public static final class ErrorCodes {
        public static final String INVALID_REQUEST = "INVALID_REQUEST";
        public static final String UNAVAILABLE = "UNAVAILABLE";
        public static final String NOT_PAIRED = "NOT_PAIRED";
        public static final String NOT_FOUND = "NOT_FOUND";
        public static final String INTERNAL = "INTERNAL";

        private ErrorCodes() {
        }
    }

    // ── Error Shape ──────────────────────────────────────────────

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ErrorShape {
        private String code;
        private String message;
        private Object details;
        private Boolean retryable;
        private Integer retryAfterMs;

        public static ErrorShape of(String code, String message) {
            return new ErrorShape(code, message, null, null, null);
        }

        public static ErrorShape of(String code, String message, Object details) {
            return new ErrorShape(code, message, details, null, null);
        }
    }

    // ── Request Frame ────────────────────────────────────────────

    /** Client → Server request frame: {type:"req", id, method, params?} */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class RequestFrame {
        private String type; // "req"
        private String id;
        private String method;
        private Object params;

        public boolean isValid() {
            return "req".equals(type) && id != null && !id.isEmpty()
                    && method != null && !method.isEmpty();
        }
    }

    // ── Response Frame ───────────────────────────────────────────

    /** Server → Client response frame: {type:"res", id, ok, payload?, error?} */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ResponseFrame {
        private String type = "res";
        private String id;
        private boolean ok;
        private Object payload;
        private ErrorShape error;

        public static ResponseFrame success(String id, Object payload) {
            return new ResponseFrame("res", id, true, payload, null);
        }

        public static ResponseFrame failure(String id, ErrorShape error) {
            return new ResponseFrame("res", id, false, null, error);
        }

        public static ResponseFrame failure(String id, String code, String message) {
            return failure(id, ErrorShape.of(code, message));
        }
    }

    // ── Event Frame ──────────────────────────────────────────────

    /** Server → Client push event: {type:"event", event, payload?, seq?} */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class EventFrame {
        private String type = "event";
        private String event;
        private Object payload;
        private Integer seq;

        public static EventFrame of(String event, Object payload) {
            return new EventFrame("event", event, payload, null);
        }
    }

    // ── Connect Params ───────────────────────────────────────────

    /** Client info sent in the connect request. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ClientInfo {
        private String id;
        private String displayName;
        private String version;
        private String platform;
        private String deviceFamily;
        private String modelIdentifier;
        private String mode;
        private String instanceId;
    }

    /** Auth credentials in connect params. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ConnectAuth {
        private String token;
        private String password;
    }

    /** Parameters for the "connect" handshake request. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ConnectParams {
        private int minProtocol;
        private int maxProtocol;
        private ClientInfo client;
        private List<String> caps;
        private List<String> commands;
        private Map<String, Boolean> permissions;
        private String pathEnv;
        private String role; // "operator" or "node"
        private List<String> scopes;
        private ConnectAuth auth;
        private String locale;
        private String userAgent;
    }

    // ── Hello-OK ─────────────────────────────────────────────────

    /** Server info in the hello-ok response. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ServerInfo {
        private String version;
        private String commit;
        private String host;
        private String connId;
    }

    /** Features advertised by the server. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Features {
        private List<String> methods;
        private List<String> events;
    }

    /** Connection policy limits. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Policy {
        private int maxPayload;
        private int maxBufferedBytes;
        private int tickIntervalMs;

        public static Policy defaults() {
            return new Policy(MAX_PAYLOAD_BYTES, MAX_BUFFERED_BYTES, TICK_INTERVAL_MS);
        }
    }

    /** State version counters. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StateVersion {
        private int presence;
        private int health;
    }

    /** Snapshot of gateway state sent in hello-ok. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class Snapshot {
        private List<Object> presence;
        private Object health;
        private StateVersion stateVersion;
        private long uptimeMs;
        private String configPath;
        private String stateDir;
    }

    /** The hello-ok payload returned after successful connect handshake. */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class HelloOk {
        private String type = "hello-ok";
        private int protocol;
        private ServerInfo server;
        private Features features;
        private Snapshot snapshot;
        private String canvasHostUrl;
        private Policy policy;
    }

    // ── Handshake State ──────────────────────────────────────────

    public enum HandshakeState {
        PENDING, CONNECTED, FAILED
    }
}

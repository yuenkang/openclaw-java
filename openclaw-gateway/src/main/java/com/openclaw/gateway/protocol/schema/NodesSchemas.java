package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Node pairing, invocation, and event protocol schemas.
 * Mirrors {@code protocol/schema/nodes.ts}.
 */
public final class NodesSchemas {

    private NodesSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class NodePairRequestParams {
        private String nodeId;
        private String displayName;
        private String platform;
        private String version;
        private String coreVersion;
        private String uiVersion;
        private String deviceFamily;
        private String modelIdentifier;
        private List<String> caps;
        private List<String> commands;
        private String remoteIp;
        private Boolean silent;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class NodePairApproveParams {
        private String requestId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class NodePairRejectParams {
        private String requestId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class NodePairVerifyParams {
        private String nodeId;
        private String token;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class NodeRenameParams {
        private String nodeId;
        private String displayName;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class NodeDescribeParams {
        private String nodeId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class NodeInvokeParams {
        private String nodeId;
        private String command;
        private Object params;
        private Integer timeoutMs;
        private String idempotencyKey;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class NodeInvokeError {
        private String code;
        private String message;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class NodeInvokeResultParams {
        private String id;
        private String nodeId;
        private boolean ok;
        private Object payload;
        private String payloadJSON;
        private NodeInvokeError error;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class NodeEventParams {
        private String event;
        private Object payload;
        private String payloadJSON;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class NodeInvokeRequestEvent {
        private String id;
        private String nodeId;
        private String command;
        private String paramsJSON;
        private Integer timeoutMs;
        private String idempotencyKey;
    }
}

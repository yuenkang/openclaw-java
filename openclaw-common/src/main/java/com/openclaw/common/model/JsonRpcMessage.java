package com.openclaw.common.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * JSON-RPC message types for Gateway communication.
 * Corresponds to TypeScript's Gateway WebSocket protocol.
 */
public class JsonRpcMessage {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Request {
        private String jsonrpc;
        private String method;
        private Object params;
        private String id;

        public static Request create(String method, Object params, String id) {
            return Request.builder()
                    .jsonrpc("2.0")
                    .method(method)
                    .params(params)
                    .id(id)
                    .build();
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Response {
        private String jsonrpc;
        private Object result;
        private RpcError error;
        private String id;

        public static Response success(String id, Object result) {
            return Response.builder()
                    .jsonrpc("2.0")
                    .result(result)
                    .id(id)
                    .build();
        }

        public static Response error(String id, int code, String message) {
            return Response.builder()
                    .jsonrpc("2.0")
                    .error(new RpcError(code, message, null))
                    .id(id)
                    .build();
        }
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Notification {
        private String jsonrpc;
        private String method;
        private Object params;

        public static Notification create(String method, Object params) {
            return Notification.builder()
                    .jsonrpc("2.0")
                    .method(method)
                    .params(params)
                    .build();
        }
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RpcError {
        private int code;
        private String message;
        private Object data;
    }

    // Standard error codes
    public static final int PARSE_ERROR = -32700;
    public static final int INVALID_REQUEST = -32600;
    public static final int METHOD_NOT_FOUND = -32601;
    public static final int INVALID_PARAMS = -32602;
    public static final int INTERNAL_ERROR = -32603;
}

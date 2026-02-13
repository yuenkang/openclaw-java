package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Device pairing protocol schemas.
 * Mirrors {@code protocol/schema/devices.ts}.
 */
public final class DevicesSchemas {

    private DevicesSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DevicePairApproveParams {
        private String requestId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DevicePairRejectParams {
        private String requestId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class DeviceTokenRotateParams {
        private String deviceId;
        private String role;
        private List<String> scopes;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DeviceTokenRevokeParams {
        private String deviceId;
        private String role;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class DevicePairRequestedEvent {
        private String requestId;
        private String deviceId;
        private String publicKey;
        private String displayName;
        private String platform;
        private String clientId;
        private String clientMode;
        private String role;
        private List<String> roles;
        private List<String> scopes;
        private String remoteIp;
        private Boolean silent;
        private Boolean isRepair;
        private long ts;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DevicePairResolvedEvent {
        private String requestId;
        private String deviceId;
        private String decision;
        private long ts;
    }
}

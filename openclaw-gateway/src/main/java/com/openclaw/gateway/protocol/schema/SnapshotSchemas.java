package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Snapshot and presence protocol schemas.
 * Mirrors {@code protocol/schema/snapshot.ts}.
 * Extends the basic Snapshot type already in ProtocolTypes with additional
 * nested types (PresenceEntry, SessionDefaults, HealthSnapshot).
 */
public final class SnapshotSchemas {

    private SnapshotSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class PresenceEntry {
        private String host;
        private String ip;
        private String version;
        private String platform;
        private String deviceFamily;
        private String modelIdentifier;
        private String mode;
        private Integer lastInputSeconds;
        private String reason;
        private List<String> tags;
        private String text;
        private long ts;
        private String deviceId;
        private List<String> roles;
        private List<String> scopes;
        private String instanceId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SessionDefaults {
        private String defaultAgentId;
        private String mainKey;
        private String mainSessionKey;
        private String scope;
    }
}

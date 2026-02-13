package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Exec-approval protocol schemas.
 * Mirrors {@code protocol/schema/exec-approvals.ts}.
 */
public final class ExecApprovalsSchemas {

    private ExecApprovalsSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AllowlistEntry {
        private String id;
        private String pattern;
        private Long lastUsedAt;
        private String lastUsedCommand;
        private String lastResolvedPath;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsDefaults {
        private String security;
        private String ask;
        private String askFallback;
        private Boolean autoAllowSkills;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsAgent {
        private String security;
        private String ask;
        private String askFallback;
        private Boolean autoAllowSkills;
        private List<AllowlistEntry> allowlist;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class SocketConfig {
        private String path;
        private String token;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsFile {
        private int version; // always 1
        private SocketConfig socket;
        private ExecApprovalsDefaults defaults;
        private Map<String, ExecApprovalsAgent> agents;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsSnapshot {
        private String path;
        private boolean exists;
        private String hash;
        private ExecApprovalsFile file;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsSetParams {
        private ExecApprovalsFile file;
        private String baseHash;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ExecApprovalsNodeGetParams {
        private String nodeId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsNodeSetParams {
        private String nodeId;
        private ExecApprovalsFile file;
        private String baseHash;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalRequestParams {
        private String id;
        private String command;
        private String cwd;
        private String host;
        private String security;
        private String ask;
        private String agentId;
        private String resolvedPath;
        private String sessionKey;
        private Integer timeoutMs;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ExecApprovalResolveParams {
        private String id;
        private String decision;
    }
}

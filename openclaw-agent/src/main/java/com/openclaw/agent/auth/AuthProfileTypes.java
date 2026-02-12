package com.openclaw.agent.auth;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Auth profile type definitions.
 * Corresponds to TypeScript auth-profiles/types.ts.
 */
public final class AuthProfileTypes {

    private AuthProfileTypes() {
    }

    // ── Credential types ──────────────────────────────────────────────

    public enum CredentialType {
        api_key, token, oauth
    }

    /**
     * Base credential interface.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder(toBuilder = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AuthProfileCredential {
        private CredentialType type;
        private String provider;
        /** api_key only */
        private String key;
        /** token only */
        private String token;
        /** oauth/token: expiry epoch ms */
        private Long expires;
        /** Email associated with this credential */
        private String email;
        /** OAuth access token */
        private String access;
        /** OAuth refresh token */
        private String refresh;
        /** Optional OAuth fields */
        private String enterpriseUrl;
        private String projectId;
        private String accountId;
        private String clientId;
        /** Optional provider-specific metadata (e.g. account IDs, gateway IDs). */
        private Map<String, String> metadata;
    }

    // ── Failure reason ────────────────────────────────────────────────

    public enum AuthProfileFailureReason {
        auth, format, rate_limit, billing, timeout, unknown
    }

    // ── Per-profile usage statistics ──────────────────────────────────

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder(toBuilder = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ProfileUsageStats {
        private Long lastUsed;
        private Long cooldownUntil;
        private Long disabledUntil;
        private AuthProfileFailureReason disabledReason;
        private Integer errorCount;
        private Map<AuthProfileFailureReason, Integer> failureCounts;
        private Long lastFailureAt;
    }

    // ── Store shape ──────────────────────────────────────────────────

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder(toBuilder = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AuthProfileStoreData {
        private int version;
        private Map<String, AuthProfileCredential> profiles;
        /** Per-agent preferred profile order overrides. */
        private Map<String, List<String>> order;
        private Map<String, String> lastGood;
        /** Usage statistics per profile for round-robin rotation. */
        private Map<String, ProfileUsageStats> usageStats;
    }

    // ── Repair result ────────────────────────────────────────────────

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class AuthProfileIdRepairResult {
        private Object config; // OpenClawConfig
        private List<String> changes;
        private boolean migrated;
        private String fromProfileId;
        private String toProfileId;
    }
}

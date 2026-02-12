package com.openclaw.agent.models;

import java.util.*;

/**
 * Auth profile health assessment service.
 * Evaluates the health status of authentication profiles (OAuth, token, API
 * key)
 * and aggregates results by provider.
 * Corresponds to TypeScript's auth-health.ts.
 */
public class AuthHealthService {

    // =========================================================================
    // Constants
    // =========================================================================

    /** Default warning threshold: 24 hours before token expiration. */
    public static final long DEFAULT_OAUTH_WARN_MS = 24 * 60 * 60 * 1000L;

    // =========================================================================
    // Types
    // =========================================================================

    public enum HealthStatus {
        OK, EXPIRING, EXPIRED, MISSING, STATIC;

        public String key() {
            return name().toLowerCase();
        }
    }

    public enum CredentialType {
        OAUTH("oauth"), TOKEN("token"), API_KEY("api_key");

        private final String label;

        CredentialType(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }
    }

    /**
     * Health assessment for a single auth profile.
     */
    public record AuthProfileHealth(
            String profileId,
            String provider,
            CredentialType type,
            HealthStatus status,
            Long expiresAt,
            Long remainingMs,
            String source,
            String label) {
    }

    /**
     * Health assessment aggregated by provider.
     */
    public record AuthProviderHealth(
            String provider,
            HealthStatus status,
            Long expiresAt,
            Long remainingMs,
            List<AuthProfileHealth> profiles) {
    }

    /**
     * Complete health summary across all authentication profiles and providers.
     */
    public record AuthHealthSummary(
            long now,
            long warnAfterMs,
            List<AuthProfileHealth> profiles,
            List<AuthProviderHealth> providers) {
    }

    /**
     * Input credential data for health assessment.
     */
    public record AuthCredential(
            String profileId,
            String provider,
            CredentialType type,
            Long expiresAt,
            String refresh, // refresh token for OAuth
            String label) {
    }

    // =========================================================================
    // Health Summary Builder
    // =========================================================================

    /**
     * Build a complete health summary from a list of credentials.
     *
     * @param credentials    list of auth credentials to evaluate
     * @param warnAfterMs    warning threshold in ms (null = default 24h)
     * @param providerFilter optional list of providers to include
     * @return health summary
     */
    public static AuthHealthSummary buildHealthSummary(
            List<AuthCredential> credentials,
            Long warnAfterMs,
            List<String> providerFilter) {

        long now = System.currentTimeMillis();
        long warn = warnAfterMs != null ? warnAfterMs : DEFAULT_OAUTH_WARN_MS;

        Set<String> filterSet = null;
        if (providerFilter != null && !providerFilter.isEmpty()) {
            filterSet = new HashSet<>();
            for (String p : providerFilter) {
                String trimmed = p.trim();
                if (!trimmed.isEmpty())
                    filterSet.add(trimmed);
            }
        }

        // Build profile health
        List<AuthProfileHealth> profiles = new ArrayList<>();
        for (AuthCredential cred : credentials) {
            if (filterSet != null && !filterSet.contains(cred.provider()))
                continue;
            profiles.add(buildProfileHealth(cred, now, warn));
        }
        profiles.sort(Comparator.comparing(AuthProfileHealth::provider)
                .thenComparing(AuthProfileHealth::profileId));

        // Aggregate to providers
        Map<String, List<AuthProfileHealth>> byProvider = new LinkedHashMap<>();
        for (AuthProfileHealth profile : profiles) {
            byProvider.computeIfAbsent(profile.provider(), k -> new ArrayList<>()).add(profile);
        }

        // Add missing providers from filter
        if (filterSet != null) {
            for (String provider : filterSet) {
                byProvider.putIfAbsent(provider, new ArrayList<>());
            }
        }

        List<AuthProviderHealth> providers = new ArrayList<>();
        for (Map.Entry<String, List<AuthProfileHealth>> entry : byProvider.entrySet()) {
            providers.add(buildProviderHealth(entry.getKey(), entry.getValue(), now));
        }
        providers.sort(Comparator.comparing(AuthProviderHealth::provider));

        return new AuthHealthSummary(now, warn, profiles, providers);
    }

    /**
     * Convenience overload without filters.
     */
    public static AuthHealthSummary buildHealthSummary(List<AuthCredential> credentials) {
        return buildHealthSummary(credentials, null, null);
    }

    // =========================================================================
    // Profile health evaluation
    // =========================================================================

    private static AuthProfileHealth buildProfileHealth(
            AuthCredential cred, long now, long warnAfterMs) {

        String source = "store";

        // API key — always static
        if (cred.type() == CredentialType.API_KEY) {
            return new AuthProfileHealth(cred.profileId(), cred.provider(),
                    CredentialType.API_KEY, HealthStatus.STATIC,
                    null, null, source, cred.label());
        }

        // Token — check expiry
        if (cred.type() == CredentialType.TOKEN) {
            Long expiresAt = cred.expiresAt();
            if (expiresAt == null || expiresAt <= 0) {
                return new AuthProfileHealth(cred.profileId(), cred.provider(),
                        CredentialType.TOKEN, HealthStatus.STATIC,
                        null, null, source, cred.label());
            }
            OAuthStatusResult osr = resolveOAuthStatus(expiresAt, now, warnAfterMs);
            return new AuthProfileHealth(cred.profileId(), cred.provider(),
                    CredentialType.TOKEN, osr.status, expiresAt, osr.remainingMs,
                    source, cred.label());
        }

        // OAuth — check expiry but account for refresh token
        boolean hasRefreshToken = cred.refresh() != null && !cred.refresh().isEmpty();
        OAuthStatusResult osr = resolveOAuthStatus(cred.expiresAt(), now, warnAfterMs);

        // OAuth with valid refresh token auto-renews → don't warn about access token
        // expiration
        HealthStatus status = osr.status;
        if (hasRefreshToken && (status == HealthStatus.EXPIRED || status == HealthStatus.EXPIRING)) {
            status = HealthStatus.OK;
        }

        return new AuthProfileHealth(cred.profileId(), cred.provider(),
                CredentialType.OAUTH, status, cred.expiresAt(), osr.remainingMs,
                source, cred.label());
    }

    private record OAuthStatusResult(HealthStatus status, Long remainingMs) {
    }

    private static OAuthStatusResult resolveOAuthStatus(
            Long expiresAt, long now, long warnAfterMs) {
        if (expiresAt == null || expiresAt <= 0) {
            return new OAuthStatusResult(HealthStatus.MISSING, null);
        }
        long remainingMs = expiresAt - now;
        if (remainingMs <= 0) {
            return new OAuthStatusResult(HealthStatus.EXPIRED, remainingMs);
        }
        if (remainingMs <= warnAfterMs) {
            return new OAuthStatusResult(HealthStatus.EXPIRING, remainingMs);
        }
        return new OAuthStatusResult(HealthStatus.OK, remainingMs);
    }

    // =========================================================================
    // Provider health aggregation
    // =========================================================================

    private static AuthProviderHealth buildProviderHealth(
            String provider, List<AuthProfileHealth> profiles, long now) {

        if (profiles.isEmpty()) {
            return new AuthProviderHealth(provider, HealthStatus.MISSING, null, null, profiles);
        }

        List<AuthProfileHealth> expirable = profiles.stream()
                .filter(p -> p.type() == CredentialType.OAUTH || p.type() == CredentialType.TOKEN)
                .toList();

        if (expirable.isEmpty()) {
            boolean hasApiKey = profiles.stream().anyMatch(p -> p.type() == CredentialType.API_KEY);
            HealthStatus status = hasApiKey ? HealthStatus.STATIC : HealthStatus.MISSING;
            return new AuthProviderHealth(provider, status, null, null, profiles);
        }

        // Find earliest expiry
        Long earliestExpiry = expirable.stream()
                .map(AuthProfileHealth::expiresAt)
                .filter(Objects::nonNull)
                .min(Comparator.naturalOrder())
                .orElse(null);
        Long remainingMs = earliestExpiry != null ? earliestExpiry - now : null;

        // Determine aggregate status
        Set<HealthStatus> statuses = new HashSet<>();
        expirable.forEach(p -> statuses.add(p.status()));

        HealthStatus status;
        if (statuses.contains(HealthStatus.EXPIRED) || statuses.contains(HealthStatus.MISSING)) {
            status = HealthStatus.EXPIRED;
        } else if (statuses.contains(HealthStatus.EXPIRING)) {
            status = HealthStatus.EXPIRING;
        } else {
            status = HealthStatus.OK;
        }

        return new AuthProviderHealth(provider, status, earliestExpiry, remainingMs, profiles);
    }

    // =========================================================================
    // Formatting
    // =========================================================================

    /**
     * Format remaining milliseconds as a short human-readable string.
     * E.g., "5m", "3h", "2d".
     */
    public static String formatRemainingShort(Long remainingMs) {
        if (remainingMs == null)
            return "unknown";
        if (remainingMs <= 0)
            return "0m";

        long minutes = Math.max(1, Math.round(remainingMs / 60_000.0));
        if (minutes < 60)
            return minutes + "m";

        long hours = Math.round(minutes / 60.0);
        if (hours < 48)
            return hours + "h";

        long days = Math.round(hours / 24.0);
        return days + "d";
    }
}

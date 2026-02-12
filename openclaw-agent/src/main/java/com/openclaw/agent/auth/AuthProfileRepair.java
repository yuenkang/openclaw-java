package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Auth profile repair — suggest and migrate legacy OAuth profile IDs.
 * Corresponds to TypeScript auth-profiles/repair.ts.
 */
public final class AuthProfileRepair {

    private AuthProfileRepair() {
    }

    /**
     * Suggest a better OAuth profile ID when the legacy ":default" suffix is in
     * use.
     */
    public static String suggestOAuthProfileIdForLegacyDefault(
            OpenClawConfig cfg, AuthProfileStoreData store,
            String provider, String legacyProfileId) {

        String providerKey = AuthProfileOrder.normalizeProviderId(provider);
        String suffix = getProfileSuffix(legacyProfileId);
        if (!"default".equals(suffix))
            return null;

        // Find all OAuth profiles for this provider
        List<String> oauthProfiles = AuthProfileProfiles.listProfilesForProvider(store, providerKey)
                .stream()
                .filter(id -> {
                    AuthProfileCredential c = store.getProfiles().get(id);
                    return c != null && c.getType() == CredentialType.oauth;
                })
                .collect(Collectors.toList());

        if (oauthProfiles.isEmpty())
            return null;

        // Try matching by lastGood
        Map<String, String> lastGood = store.getLastGood();
        if (lastGood != null) {
            String good = lastGood.getOrDefault(providerKey, lastGood.get(provider));
            if (good != null && oauthProfiles.contains(good))
                return good;
        }

        // Filter out the legacy profile itself
        List<String> nonLegacy = oauthProfiles.stream()
                .filter(id -> !id.equals(legacyProfileId))
                .collect(Collectors.toList());
        if (nonLegacy.size() == 1)
            return nonLegacy.get(0);

        // Try to find one with an email-like suffix
        List<String> emailLike = nonLegacy.stream()
                .filter(id -> isEmailLike(getProfileSuffix(id)))
                .collect(Collectors.toList());
        if (emailLike.size() == 1)
            return emailLike.get(0);

        return null;
    }

    /**
     * Repair an OAuth profile ID mismatch by migrating config from legacy to
     * suggested.
     */
    public static AuthProfileIdRepairResult repairOAuthProfileIdMismatch(
            OpenClawConfig cfg, AuthProfileStoreData store,
            String provider, String legacyProfileId) {

        String fromId = legacyProfileId != null ? legacyProfileId
                : AuthProfileOrder.normalizeProviderId(provider) + ":default";

        String toId = suggestOAuthProfileIdForLegacyDefault(cfg, store, provider, fromId);
        if (toId == null || toId.equals(fromId)) {
            return AuthProfileIdRepairResult.builder()
                    .config(cfg).changes(List.of()).migrated(false).build();
        }

        List<String> changes = List.of(
                "Auth: migrate " + fromId + " → " + toId + " (OAuth profile id)");

        return AuthProfileIdRepairResult.builder()
                .config(cfg)
                .changes(changes)
                .migrated(true)
                .fromProfileId(fromId)
                .toProfileId(toId)
                .build();
    }

    // ── Helpers ──────────────────────────────────────────────────────

    private static String getProfileSuffix(String profileId) {
        int idx = profileId.indexOf(':');
        return idx < 0 ? "" : profileId.substring(idx + 1);
    }

    private static boolean isEmailLike(String value) {
        String trimmed = value.trim();
        return !trimmed.isEmpty() && trimmed.contains("@") && trimmed.contains(".");
    }
}

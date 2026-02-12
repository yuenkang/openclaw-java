package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Auth profile diagnostics — hint formatting for errors.
 * Corresponds to TypeScript auth-profiles/doctor.ts.
 */
public final class AuthProfileDoctor {

    private AuthProfileDoctor() {
    }

    /**
     * Format a diagnostic hint for auth failures (currently Anthropic-focused).
     *
     * @return Hint string, or empty if no hint applicable
     */
    public static String formatAuthDoctorHint(
            OpenClawConfig cfg, AuthProfileStoreData store,
            String provider, String profileId) {

        String providerKey = AuthProfileOrder.normalizeProviderId(provider);
        if (!"anthropic".equals(providerKey))
            return "";

        String legacyId = profileId != null ? profileId : "anthropic:default";
        String suggested = AuthProfileRepair.suggestOAuthProfileIdForLegacyDefault(
                cfg, store, providerKey, legacyId);
        if (suggested == null || suggested.equals(legacyId))
            return "";

        String storeOauthProfiles = AuthProfileProfiles.listProfilesForProvider(store, providerKey)
                .stream()
                .filter(id -> {
                    AuthProfileCredential c = store.getProfiles().get(id);
                    return c != null && c.getType() == CredentialType.oauth;
                })
                .collect(Collectors.joining(", "));

        return String.join("\n", List.of(
                "Doctor hint (for GitHub issue):",
                "- provider: " + providerKey,
                "- config: " + legacyId,
                "- auth store oauth profiles: " +
                        (storeOauthProfiles.isEmpty() ? "(none)" : storeOauthProfiles),
                "- suggested profile: " + suggested,
                "Fix: run \"openclaw doctor --yes\""));
    }
}

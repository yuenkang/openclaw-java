package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;

/**
 * Auth profile display formatting.
 * Corresponds to TypeScript auth-profiles/display.ts.
 */
public final class AuthProfileDisplay {

    private AuthProfileDisplay() {
    }

    /**
     * Resolve a human-readable display label for a profile.
     * Appends email if available.
     *
     * @return "profileId (email)" or just "profileId"
     */
    public static String resolveDisplayLabel(
            OpenClawConfig cfg, AuthProfileStoreData store, String profileId) {

        AuthProfileCredential cred = store.getProfiles().get(profileId);
        // Try config email first, then credential email
        String email = null;
        // TODO: read cfg.auth.profiles[profileId].email when config supports it
        if (email == null && cred != null && cred.getEmail() != null
                && !cred.getEmail().isBlank()) {
            email = cred.getEmail().trim();
        }

        return email != null ? profileId + " (" + email + ")" : profileId;
    }
}

package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import lombok.extern.slf4j.Slf4j;

import static com.openclaw.agent.auth.AuthProfileConstants.*;

/**
 * External CLI credential synchronization (Qwen, MiniMax).
 * Corresponds to TypeScript auth-profiles/external-cli-sync.ts.
 */
@Slf4j
public final class AuthProfileExternalSync {

    private AuthProfileExternalSync() {
    }

    /**
     * Sync OAuth credentials from external CLI tools into the store.
     *
     * <p>
     * Currently supports:
     * <ul>
     * <li>Qwen Code CLI → qwen-portal:qwen-cli</li>
     * <li>MiniMax CLI → minimax-portal:minimax-cli</li>
     * </ul>
     *
     * @return true if any credentials were updated
     */
    public static boolean syncExternalCliCredentials(AuthProfileStoreData store) {
        boolean mutated = false;
        long now = System.currentTimeMillis();

        // Sync Qwen Code CLI
        if (shouldSync(store, QWEN_CLI_PROFILE_ID, "qwen-portal", now)) {
            AuthProfileCredential qwenCreds = readExternalCliCredentials("qwen-portal");
            if (qwenCreds != null && shouldUpdate(store, QWEN_CLI_PROFILE_ID,
                    "qwen-portal", qwenCreds, now)) {
                store.getProfiles().put(QWEN_CLI_PROFILE_ID, qwenCreds);
                mutated = true;
                log.info("synced qwen credentials from qwen cli: {}", QWEN_CLI_PROFILE_ID);
            }
        }

        // Sync MiniMax Portal CLI
        if (shouldSync(store, MINIMAX_CLI_PROFILE_ID, "minimax-portal", now)) {
            AuthProfileCredential minimaxCreds = readExternalCliCredentials("minimax-portal");
            if (minimaxCreds != null && shouldUpdate(store, MINIMAX_CLI_PROFILE_ID,
                    "minimax-portal", minimaxCreds, now)) {
                store.getProfiles().put(MINIMAX_CLI_PROFILE_ID, minimaxCreds);
                mutated = true;
                log.info("synced minimax credentials from minimax cli: {}",
                        MINIMAX_CLI_PROFILE_ID);
            }
        }

        return mutated;
    }

    // ── Helpers ──────────────────────────────────────────────────────

    private static boolean shouldSync(
            AuthProfileStoreData store, String profileId, String provider, long now) {
        AuthProfileCredential existing = store.getProfiles().get(profileId);
        if (existing == null)
            return true;
        if (!provider.equals(existing.getProvider()))
            return true;
        return !isExternalProfileFresh(existing, now);
    }

    private static boolean shouldUpdate(
            AuthProfileStoreData store, String profileId, String provider,
            AuthProfileCredential newCreds, long now) {
        AuthProfileCredential existing = store.getProfiles().get(profileId);
        if (existing == null || existing.getType() != CredentialType.oauth)
            return true;
        if (!provider.equals(existing.getProvider()))
            return true;
        if (existing.getExpires() != null && existing.getExpires() <= now)
            return true;
        if (newCreds.getExpires() != null && existing.getExpires() != null
                && newCreds.getExpires() > existing.getExpires())
            return true;
        return false;
    }

    private static boolean isExternalProfileFresh(AuthProfileCredential cred, long now) {
        if (cred.getType() != CredentialType.oauth && cred.getType() != CredentialType.token)
            return false;
        String provider = cred.getProvider();
        if (!"qwen-portal".equals(provider) && !"minimax-portal".equals(provider))
            return false;
        if (cred.getExpires() == null)
            return true;
        return cred.getExpires() > now + EXTERNAL_CLI_NEAR_EXPIRY_MS;
    }

    /**
     * Read OAuth credentials from an external CLI tool.
     * TODO: implement actual credential file reading for qwen/minimax CLIs.
     */
    private static AuthProfileCredential readExternalCliCredentials(String provider) {
        log.debug("readExternalCliCredentials: stub for provider={}", provider);
        return null;
    }
}

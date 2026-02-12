package com.openclaw.agent.auth;

import com.openclaw.agent.auth.AuthProfileTypes.*;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * OAuth credential resolution and refresh.
 * Corresponds to TypeScript auth-profiles/oauth.ts.
 */
@Slf4j
public final class AuthProfileOAuth {

    private AuthProfileOAuth() {
    }

    /**
     * Resolved API key result.
     */
    public record ResolvedApiKey(String apiKey, String provider, String email) {
    }

    /**
     * Build an OAuth "API key" from credentials.
     * Some providers need a JSON payload (e.g. Google with projectId).
     */
    public static String buildOAuthApiKey(String provider, AuthProfileCredential cred) {
        boolean needsProjectId = "google-gemini-cli".equals(provider)
                || "google-antigravity".equals(provider);
        if (needsProjectId && cred.getProjectId() != null) {
            return "{\"token\":\"" + cred.getAccess() + "\",\"projectId\":\"" + cred.getProjectId() + "\"}";
        }
        return cred.getAccess();
    }

    /**
     * Resolve an API key for a profile, handling all credential types.
     *
     * @param cfg       OpenClaw config
     * @param store     Auth profile store
     * @param profileId Profile to resolve
     * @param agentDir  Agent directory (null for main)
     * @return Resolved API key or null if profile is invalid/expired
     */
    public static ResolvedApiKey resolveApiKeyForProfile(
            OpenClawConfig cfg, AuthProfileStoreData store,
            String profileId, String agentDir) {

        AuthProfileCredential cred = store.getProfiles().get(profileId);
        if (cred == null)
            return null;

        long now = System.currentTimeMillis();

        switch (cred.getType()) {
            case api_key -> {
                String key = cred.getKey();
                if (key == null || key.isBlank())
                    return null;
                return new ResolvedApiKey(key.trim(), cred.getProvider(), cred.getEmail());
            }
            case token -> {
                String token = cred.getToken();
                if (token == null || token.isBlank())
                    return null;
                if (cred.getExpires() != null && cred.getExpires() > 0 && now >= cred.getExpires()) {
                    return null;
                }
                return new ResolvedApiKey(token.trim(), cred.getProvider(), cred.getEmail());
            }
            case oauth -> {
                // Check if access token is still valid
                if (cred.getAccess() != null && !cred.getAccess().isBlank()
                        && cred.getExpires() != null && now < cred.getExpires()) {
                    String apiKey = buildOAuthApiKey(cred.getProvider(), cred);
                    return new ResolvedApiKey(apiKey, cred.getProvider(), cred.getEmail());
                }

                // Try refresh
                ResolvedApiKey refreshed = tryRefreshOAuth(store, profileId, cred, agentDir);
                if (refreshed != null)
                    return refreshed;

                // Try main agent fallback for sub-agents
                if (agentDir != null) {
                    ResolvedApiKey mainFallback = tryMainAgentFallback(
                            cfg, store, profileId, cred, agentDir);
                    if (mainFallback != null)
                        return mainFallback;
                }

                String accessStr = cred.getAccess();
                if (accessStr != null && !accessStr.isBlank()) {
                    // Return potentially expired token as last resort
                    return new ResolvedApiKey(
                            buildOAuthApiKey(cred.getProvider(), cred),
                            cred.getProvider(), cred.getEmail());
                }
                return null;
            }
            default -> {
                return null;
            }
        }
    }

    /**
     * Try to refresh an OAuth token.
     * TODO: Integrate with actual OAuth providers (pi-ai SDK, Qwen, Chutes).
     */
    private static ResolvedApiKey tryRefreshOAuth(
            AuthProfileStoreData store, String profileId,
            AuthProfileCredential cred, String agentDir) {

        // Reload store to check if another process already refreshed
        AuthProfileStoreData fresh = AuthProfileStoreManager.ensureAuthProfileStore(agentDir);
        AuthProfileCredential freshCred = fresh.getProfiles().get(profileId);
        if (freshCred != null && freshCred.getType() == CredentialType.oauth
                && freshCred.getExpires() != null
                && System.currentTimeMillis() < freshCred.getExpires()
                && freshCred.getAccess() != null && !freshCred.getAccess().isBlank()) {
            // Another thread/process already refreshed
            store.getProfiles().put(profileId, freshCred);
            return new ResolvedApiKey(
                    buildOAuthApiKey(freshCred.getProvider(), freshCred),
                    freshCred.getProvider(), freshCred.getEmail());
        }

        // TODO: actual refresh via provider SDK
        log.debug("OAuth refresh not yet implemented for profile {}", profileId);
        return null;
    }

    /**
     * Try to use main agent's credentials as fallback for sub-agent.
     */
    private static ResolvedApiKey tryMainAgentFallback(
            OpenClawConfig cfg, AuthProfileStoreData store,
            String profileId, AuthProfileCredential cred, String agentDir) {

        try {
            AuthProfileStoreData mainStore = AuthProfileStoreManager.ensureAuthProfileStore(null);
            AuthProfileCredential mainCred = mainStore.getProfiles().get(profileId);
            if (mainCred != null && mainCred.getType() == CredentialType.oauth
                    && mainCred.getExpires() != null
                    && System.currentTimeMillis() < mainCred.getExpires()) {
                // Inherit from main
                store.getProfiles().put(profileId, mainCred);
                AuthProfileStoreManager.saveAuthProfileStore(store, agentDir);
                log.info("Inherited fresh OAuth credentials from main agent: {}", profileId);
                return new ResolvedApiKey(
                        buildOAuthApiKey(mainCred.getProvider(), mainCred),
                        mainCred.getProvider(), mainCred.getEmail());
            }
        } catch (Exception e) {
            log.debug("Main agent fallback failed for {}: {}", profileId, e.getMessage());
        }
        return null;
    }
}

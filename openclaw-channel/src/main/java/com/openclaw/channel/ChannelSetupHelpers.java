package com.openclaw.channel;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Setup helpers for channel account name management in config.
 * Corresponds to TypeScript's channels/plugins/setup-helpers.ts.
 */
public final class ChannelSetupHelpers {

    private ChannelSetupHelpers() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    /**
     * Apply an account name to a channel config section.
     * Decides whether to store at top level or under accounts/ based on existing
     * structure.
     *
     * @param channels          the "channels" section (mutable)
     * @param channelKey        channel section key (e.g. "telegram")
     * @param accountId         account ID
     * @param name              the display name to set
     * @param alwaysUseAccounts if true, always store under accounts/
     */
    @SuppressWarnings("unchecked")
    public static void applyAccountName(Map<String, Object> channels,
            String channelKey,
            String accountId,
            String name,
            boolean alwaysUseAccounts) {
        String trimmed = name != null ? name.trim() : "";
        if (trimmed.isEmpty()) {
            return;
        }
        String effectiveAccountId = (accountId != null && !accountId.isBlank())
                ? accountId
                : DEFAULT_ACCOUNT_ID;

        Map<String, Object> base = channels.get(channelKey) instanceof Map
                ? (Map<String, Object>) channels.get(channelKey)
                : new LinkedHashMap<>();
        channels.put(channelKey, base);

        boolean useAccounts = shouldStoreInAccounts(base, effectiveAccountId, alwaysUseAccounts);

        if (!useAccounts && DEFAULT_ACCOUNT_ID.equals(effectiveAccountId)) {
            base.put("name", trimmed);
            return;
        }

        // Store under accounts/ sub-map
        Map<String, Object> accounts = base.get("accounts") instanceof Map
                ? (Map<String, Object>) base.get("accounts")
                : new LinkedHashMap<>();
        base.put("accounts", accounts);

        Map<String, Object> accountEntry = accounts.get(effectiveAccountId) instanceof Map
                ? (Map<String, Object>) accounts.get(effectiveAccountId)
                : new LinkedHashMap<>();
        accounts.put(effectiveAccountId, accountEntry);
        accountEntry.put("name", trimmed);

        // If moving default to accounts, remove top-level name
        if (DEFAULT_ACCOUNT_ID.equals(effectiveAccountId)) {
            base.remove("name");
        }
    }

    /**
     * Migrate a base-level name to the default account entry.
     */
    @SuppressWarnings("unchecked")
    public static void migrateBaseNameToDefaultAccount(Map<String, Object> channels,
            String channelKey) {
        Object sectionObj = channels.get(channelKey);
        if (!(sectionObj instanceof Map)) {
            return;
        }
        Map<String, Object> base = (Map<String, Object>) sectionObj;
        Object nameObj = base.get("name");
        if (!(nameObj instanceof String) || ((String) nameObj).trim().isEmpty()) {
            return;
        }
        String baseName = ((String) nameObj).trim();

        Map<String, Object> accounts = base.get("accounts") instanceof Map
                ? (Map<String, Object>) base.get("accounts")
                : new LinkedHashMap<>();
        base.put("accounts", accounts);

        Map<String, Object> defaultAccount = accounts.get(DEFAULT_ACCOUNT_ID) instanceof Map
                ? (Map<String, Object>) accounts.get(DEFAULT_ACCOUNT_ID)
                : new LinkedHashMap<>();
        accounts.put(DEFAULT_ACCOUNT_ID, defaultAccount);

        if (!defaultAccount.containsKey("name")) {
            defaultAccount.put("name", baseName);
        }
        base.remove("name");
    }

    // =========================================================================
    // Internal
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static boolean shouldStoreInAccounts(Map<String, Object> base,
            String accountId,
            boolean alwaysUseAccounts) {
        if (alwaysUseAccounts) {
            return true;
        }
        if (!DEFAULT_ACCOUNT_ID.equals(accountId)) {
            return true;
        }
        Map<String, Object> accounts = base.get("accounts") instanceof Map
                ? (Map<String, Object>) base.get("accounts")
                : null;
        return accounts != null && !accounts.isEmpty();
    }
}

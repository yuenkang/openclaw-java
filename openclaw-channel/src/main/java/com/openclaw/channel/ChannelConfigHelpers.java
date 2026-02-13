package com.openclaw.channel;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Config section helpers for channel account enable/disable/delete.
 * Corresponds to TypeScript's channels/plugins/config-helpers.ts.
 */
public final class ChannelConfigHelpers {

    private ChannelConfigHelpers() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    /**
     * Set the enabled flag for a channel account in a config map.
     *
     * @param channels      the "channels" section of the config (mutable copy)
     * @param sectionKey    the channel section key (e.g. "telegram", "discord")
     * @param accountId     account ID
     * @param enabled       whether to enable
     * @param allowTopLevel if true, allow setting enabled at top level for default
     *                      account
     */
    @SuppressWarnings("unchecked")
    public static void setAccountEnabled(Map<String, Object> channels,
            String sectionKey,
            String accountId,
            boolean enabled,
            boolean allowTopLevel) {
        String accountKey = (accountId == null || accountId.isEmpty()) ? DEFAULT_ACCOUNT_ID : accountId;
        Map<String, Object> base = getOrCreateSection(channels, sectionKey);
        Map<String, Object> accounts = (Map<String, Object>) base.get("accounts");
        boolean hasAccounts = accounts != null && !accounts.isEmpty();

        if (allowTopLevel && DEFAULT_ACCOUNT_ID.equals(accountKey) && !hasAccounts) {
            base.put("enabled", enabled);
            return;
        }

        if (accounts == null) {
            accounts = new LinkedHashMap<>();
            base.put("accounts", accounts);
        }
        Map<String, Object> accountEntry = (Map<String, Object>) accounts.computeIfAbsent(
                accountKey, k -> new LinkedHashMap<>());
        accountEntry.put("enabled", enabled);
    }

    /**
     * Delete a channel account from a config section.
     *
     * @param channels        the "channels" section of the config (mutable copy)
     * @param sectionKey      the channel section key
     * @param accountId       account ID to remove
     * @param clearBaseFields fields to clear from the base section when removing
     *                        default account
     */
    @SuppressWarnings("unchecked")
    public static void deleteAccount(Map<String, Object> channels,
            String sectionKey,
            String accountId,
            String... clearBaseFields) {
        String accountKey = (accountId == null || accountId.isEmpty()) ? DEFAULT_ACCOUNT_ID : accountId;
        Map<String, Object> base = (Map<String, Object>) channels.get(sectionKey);
        if (base == null) {
            return;
        }

        Map<String, Object> accounts = (Map<String, Object>) base.get("accounts");

        if (!DEFAULT_ACCOUNT_ID.equals(accountKey)) {
            if (accounts != null) {
                accounts.remove(accountKey);
                if (accounts.isEmpty()) {
                    base.remove("accounts");
                }
            }
            return;
        }

        // Removing default account
        if (accounts != null && !accounts.isEmpty()) {
            accounts.remove(accountKey);
            for (String field : clearBaseFields) {
                base.remove(field);
            }
            if (accounts.isEmpty()) {
                base.remove("accounts");
            }
            return;
        }

        // No other accounts: remove the entire section
        channels.remove(sectionKey);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getOrCreateSection(Map<String, Object> channels,
            String sectionKey) {
        return (Map<String, Object>) channels.computeIfAbsent(
                sectionKey, k -> new LinkedHashMap<>());
    }
}

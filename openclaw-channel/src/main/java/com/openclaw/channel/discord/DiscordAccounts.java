package com.openclaw.channel.discord;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Discord account resolution and enumeration.
 * Corresponds to TypeScript's discord/accounts.ts.
 */
public final class DiscordAccounts {

    private DiscordAccounts() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    /**
     * A resolved Discord account with merged config, token, and enabled state.
     */
    public record ResolvedAccount(
            String accountId,
            boolean enabled,
            String name,
            String token,
            DiscordToken.Source tokenSource,
            Map<String, Object> config) {
    }

    /**
     * List all configured Discord account IDs.
     */
    @SuppressWarnings("unchecked")
    public static List<String> listAccountIds(Map<String, Object> channelsConfig) {
        Map<String, Object> discordCfg = DiscordToken.getMap(channelsConfig, "discord");
        if (discordCfg == null)
            return List.of(DEFAULT_ACCOUNT_ID);

        Map<String, Object> accounts = DiscordToken.getMap(discordCfg, "accounts");
        if (accounts == null || accounts.isEmpty())
            return List.of(DEFAULT_ACCOUNT_ID);

        List<String> ids = new ArrayList<>(accounts.keySet().stream().filter(k -> !k.isBlank()).toList());
        Collections.sort(ids);
        return ids.isEmpty() ? List.of(DEFAULT_ACCOUNT_ID) : ids;
    }

    /**
     * Resolve the default account ID.
     */
    public static String resolveDefaultAccountId(Map<String, Object> channelsConfig) {
        List<String> ids = listAccountIds(channelsConfig);
        if (ids.contains(DEFAULT_ACCOUNT_ID))
            return DEFAULT_ACCOUNT_ID;
        return ids.isEmpty() ? DEFAULT_ACCOUNT_ID : ids.get(0);
    }

    /**
     * Merge base Discord config with account overlay.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> mergeAccountConfig(Map<String, Object> channelsConfig,
            String accountId) {
        Map<String, Object> discordCfg = DiscordToken.getMap(channelsConfig, "discord");
        if (discordCfg == null)
            return Map.of();

        java.util.LinkedHashMap<String, Object> merged = new java.util.LinkedHashMap<>(discordCfg);
        merged.remove("accounts");

        Map<String, Object> accounts = DiscordToken.getMap(discordCfg, "accounts");
        if (accounts != null) {
            Map<String, Object> accountCfg = DiscordToken.getMap(accounts, accountId);
            if (accountCfg != null) {
                merged.putAll(accountCfg);
            }
        }
        return merged;
    }

    /**
     * Resolve a Discord account by ID, merging config and resolving token.
     */
    public static ResolvedAccount resolve(Map<String, Object> channelsConfig, String accountId) {
        String effectiveId = DiscordToken.normalizeAccountId(accountId);
        Map<String, Object> merged = mergeAccountConfig(channelsConfig, effectiveId);

        Map<String, Object> discordCfg = DiscordToken.getMap(channelsConfig, "discord");
        boolean baseEnabled = discordCfg == null || !Boolean.FALSE.equals(discordCfg.get("enabled"));
        boolean accountEnabled = !Boolean.FALSE.equals(merged.get("enabled"));
        boolean enabled = baseEnabled && accountEnabled;

        DiscordToken.Resolution tokenRes = DiscordToken.resolve(channelsConfig, effectiveId);

        String name = null;
        Object nameObj = merged.get("name");
        if (nameObj instanceof String s && !s.isBlank()) {
            name = s.trim();
        }

        return new ResolvedAccount(effectiveId, enabled, name, tokenRes.token(), tokenRes.source(), merged);
    }

    /**
     * List all enabled Discord accounts.
     */
    public static List<ResolvedAccount> listEnabled(Map<String, Object> channelsConfig) {
        return listAccountIds(channelsConfig).stream()
                .map(id -> resolve(channelsConfig, id))
                .filter(ResolvedAccount::enabled)
                .toList();
    }
}

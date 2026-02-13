package com.openclaw.channel.telegram;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Telegram account resolution and enumeration.
 * Corresponds to TypeScript's telegram/accounts.ts.
 */
public final class TelegramAccounts {

    private TelegramAccounts() {
    }

    public static final String DEFAULT_ACCOUNT_ID = "default";

    /**
     * A resolved Telegram account with merged config, token, and enabled state.
     */
    public record ResolvedAccount(
            String accountId,
            boolean enabled,
            String name,
            String token,
            TelegramToken.Source tokenSource,
            Map<String, Object> config) {
    }

    /**
     * List all configured Telegram account IDs (normalized).
     */
    @SuppressWarnings("unchecked")
    public static List<String> listAccountIds(Map<String, Object> channelsConfig) {
        Map<String, Object> telegramCfg = TelegramToken.getMap(channelsConfig, "telegram");
        if (telegramCfg == null)
            return List.of(DEFAULT_ACCOUNT_ID);

        Map<String, Object> accounts = TelegramToken.getMap(telegramCfg, "accounts");
        if (accounts == null || accounts.isEmpty())
            return List.of(DEFAULT_ACCOUNT_ID);

        Set<String> ids = new LinkedHashSet<>();
        for (String key : accounts.keySet()) {
            if (key != null && !key.isBlank()) {
                ids.add(TelegramToken.normalizeAccountId(key));
            }
        }
        List<String> sorted = new ArrayList<>(ids);
        Collections.sort(sorted);
        return sorted.isEmpty() ? List.of(DEFAULT_ACCOUNT_ID) : sorted;
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
     * Merge base Telegram config with account overlay.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> mergeAccountConfig(Map<String, Object> channelsConfig,
            String accountId) {
        Map<String, Object> telegramCfg = TelegramToken.getMap(channelsConfig, "telegram");
        if (telegramCfg == null)
            return Map.of();

        java.util.LinkedHashMap<String, Object> merged = new java.util.LinkedHashMap<>(telegramCfg);
        merged.remove("accounts");

        // Resolve account config with normalized key fallback
        Map<String, Object> accounts = TelegramToken.getMap(telegramCfg, "accounts");
        if (accounts != null) {
            Map<String, Object> accountCfg = findAccountConfig(accounts, accountId);
            if (accountCfg != null) {
                merged.putAll(accountCfg);
            }
        }
        return merged;
    }

    /**
     * Resolve a Telegram account by ID, merging config and resolving token.
     */
    public static ResolvedAccount resolve(Map<String, Object> channelsConfig, String accountId) {
        String effectiveId = TelegramToken.normalizeAccountId(accountId);
        Map<String, Object> merged = mergeAccountConfig(channelsConfig, effectiveId);

        Map<String, Object> telegramCfg = TelegramToken.getMap(channelsConfig, "telegram");
        boolean baseEnabled = telegramCfg == null || !Boolean.FALSE.equals(telegramCfg.get("enabled"));
        boolean accountEnabled = !Boolean.FALSE.equals(merged.get("enabled"));
        boolean enabled = baseEnabled && accountEnabled;

        TelegramToken.Resolution tokenRes = TelegramToken.resolve(channelsConfig, effectiveId);

        String name = null;
        Object nameObj = merged.get("name");
        if (nameObj instanceof String s && !s.isBlank()) {
            name = s.trim();
        }

        return new ResolvedAccount(effectiveId, enabled, name, tokenRes.token(), tokenRes.source(), merged);
    }

    /**
     * Resolve a Telegram account with implicit fallback:
     * if no explicit accountId and default has no token, try the first configured
     * account.
     */
    public static ResolvedAccount resolveWithFallback(Map<String, Object> channelsConfig,
            String accountId) {
        boolean hasExplicit = accountId != null && !accountId.isBlank();
        ResolvedAccount primary = resolve(channelsConfig, accountId);

        if (hasExplicit)
            return primary;
        if (primary.tokenSource() != TelegramToken.Source.NONE)
            return primary;

        String fallbackId = resolveDefaultAccountId(channelsConfig);
        if (fallbackId.equals(primary.accountId()))
            return primary;

        ResolvedAccount fallback = resolve(channelsConfig, fallbackId);
        return fallback.tokenSource() == TelegramToken.Source.NONE ? primary : fallback;
    }

    /**
     * List all enabled Telegram accounts.
     */
    public static List<ResolvedAccount> listEnabled(Map<String, Object> channelsConfig) {
        return listAccountIds(channelsConfig).stream()
                .map(id -> resolve(channelsConfig, id))
                .filter(ResolvedAccount::enabled)
                .toList();
    }

    // =========================================================================
    // Internal
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static Map<String, Object> findAccountConfig(Map<String, Object> accounts, String accountId) {
        // Direct match
        Object direct = accounts.get(accountId);
        if (direct instanceof Map)
            return (Map<String, Object>) direct;

        // Normalized fallback
        for (Map.Entry<String, Object> entry : accounts.entrySet()) {
            if (TelegramToken.normalizeAccountId(entry.getKey()).equals(accountId)
                    && entry.getValue() instanceof Map) {
                return (Map<String, Object>) entry.getValue();
            }
        }
        return null;
    }
}

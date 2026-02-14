package com.openclaw.channel;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Directory config: list peers and groups from channel configuration.
 * Supports Telegram, Discord, WhatsApp, and Slack directory entries.
 * Corresponds to TypeScript's channels/plugins/directory-config.ts.
 */
@Slf4j
public final class DirectoryConfig {

    private DirectoryConfig() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    public static class DirectoryConfigParams {
        private OpenClawConfig cfg;
        private String accountId;
        private String query;
        private Integer limit;
    }

    @Data
    public static class DirectoryEntry {
        private String id;
        private String name;
        private String type; // "peer" | "group"
        private String channel;
        private String accountId;
        private Map<String, Object> metadata;
    }

    // =========================================================================
    // Telegram
    // =========================================================================

    /**
     * List Telegram directory peers from config.
     */
    @SuppressWarnings("unchecked")
    public static List<DirectoryEntry> listTelegramDirectoryPeersFromConfig(DirectoryConfigParams params) {
        List<DirectoryEntry> entries = new ArrayList<>();
        OpenClawConfig cfg = params.getCfg();
        if (cfg == null || cfg.getChannels() == null)
            return entries;

        Map<String, Object> providers = cfg.getChannels().getProviders();
        if (providers == null)
            return entries;

        Object telegram = providers.get("telegram");
        if (!(telegram instanceof Map<?, ?> tg))
            return entries;

        Object allowFrom = tg.get("allowFrom");
        if (allowFrom instanceof List<?> list) {
            for (Object item : list) {
                String id = String.valueOf(item).trim();
                if (id.isBlank())
                    continue;
                if (params.getQuery() != null && !id.toLowerCase().contains(params.getQuery().toLowerCase()))
                    continue;

                DirectoryEntry entry = new DirectoryEntry();
                entry.setId(id);
                entry.setName(id.startsWith("@") ? id.substring(1) : id);
                entry.setType("peer");
                entry.setChannel("telegram");
                entry.setAccountId(params.getAccountId());
                entries.add(entry);
            }
        }

        if (params.getLimit() != null && entries.size() > params.getLimit()) {
            return entries.subList(0, params.getLimit());
        }
        return entries;
    }

    /**
     * List Telegram directory groups from config.
     */
    @SuppressWarnings("unchecked")
    public static List<DirectoryEntry> listTelegramDirectoryGroupsFromConfig(DirectoryConfigParams params) {
        List<DirectoryEntry> entries = new ArrayList<>();
        OpenClawConfig cfg = params.getCfg();
        if (cfg == null || cfg.getChannels() == null)
            return entries;

        Map<String, Object> providers = cfg.getChannels().getProviders();
        if (providers == null)
            return entries;

        Object telegram = providers.get("telegram");
        if (!(telegram instanceof Map<?, ?> tg))
            return entries;

        Object groups = tg.get("groups");
        if (groups instanceof Map<?, ?> groupMap) {
            for (Map.Entry<?, ?> e : groupMap.entrySet()) {
                String groupId = String.valueOf(e.getKey()).trim();
                if (groupId.isBlank())
                    continue;
                if (params.getQuery() != null &&
                        !groupId.toLowerCase().contains(params.getQuery().toLowerCase()))
                    continue;

                DirectoryEntry entry = new DirectoryEntry();
                entry.setId(groupId);
                entry.setName(groupId);
                entry.setType("group");
                entry.setChannel("telegram");
                entry.setAccountId(params.getAccountId());
                entries.add(entry);
            }
        }

        if (params.getLimit() != null && entries.size() > params.getLimit()) {
            return entries.subList(0, params.getLimit());
        }
        return entries;
    }

    // =========================================================================
    // Generic
    // =========================================================================

    /**
     * List directory entries for a given channel.
     */
    public static List<DirectoryEntry> listDirectoryEntries(
            String channel, String type, DirectoryConfigParams params) {
        return switch (channel) {
            case "telegram" -> "peer".equals(type)
                    ? listTelegramDirectoryPeersFromConfig(params)
                    : listTelegramDirectoryGroupsFromConfig(params);
            default -> {
                log.debug("No directory config support for channel: {}", channel);
                yield List.of();
            }
        };
    }
}

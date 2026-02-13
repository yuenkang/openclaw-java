package com.openclaw.common.config;

import java.util.Map;

/**
 * Markdown table mode resolution for channels.
 * Corresponds to TypeScript's markdown-tables.ts.
 */
public final class MarkdownTableModeResolver {

    private MarkdownTableModeResolver() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    /** Valid modes: "off", "bullets", "code" */
    private static final Map<String, String> DEFAULT_TABLE_MODES = Map.of(
            "signal", "bullets",
            "whatsapp", "bullets");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Check whether a value is a valid MarkdownTableMode.
     */
    public static boolean isMarkdownTableMode(Object value) {
        return "off".equals(value) || "bullets".equals(value) || "code".equals(value);
    }

    /**
     * Resolve the markdown table mode for a channel/account.
     *
     * @param cfg       the OpenClaw config (partial)
     * @param channel   channel identifier (e.g. "signal", "whatsapp")
     * @param accountId optional account ID for per-account resolution
     * @return the resolved mode: "off", "bullets", or "code"
     */
    @SuppressWarnings("unchecked")
    public static String resolveMarkdownTableMode(OpenClawConfig cfg, String channel,
            String accountId) {
        String normalizedChannel = normalizeChannelId(channel);
        String defaultMode = normalizedChannel != null
                ? DEFAULT_TABLE_MODES.getOrDefault(normalizedChannel, "code")
                : "code";

        if (normalizedChannel == null || cfg == null) {
            return defaultMode;
        }

        // Try to find the channel section in config
        var channelsConfig = cfg.getChannels();
        Map<String, Object> section = null;
        if (channelsConfig != null && channelsConfig.getProviders() != null) {
            Object raw = channelsConfig.getProviders().get(normalizedChannel);
            if (raw instanceof Map) {
                section = (Map<String, Object>) raw;
            }
        }
        if (section == null) {
            return defaultMode;
        }

        String resolved = resolveMarkdownModeFromSection(section, accountId);
        return resolved != null ? resolved : defaultMode;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static String resolveMarkdownModeFromSection(Map<String, Object> section,
            String accountId) {
        if (section == null) {
            return null;
        }

        String normalizedAccountId = normalizeAccountId(accountId);

        // Check per-account setting first
        Object accountsRaw = section.get("accounts");
        if (accountsRaw instanceof Map<?, ?> accounts) {
            // Direct lookup
            Object directEntry = accounts.get(normalizedAccountId);
            String directMode = extractTableMode(directEntry);
            if (directMode != null) {
                return directMode;
            }
            // Case-insensitive lookup
            for (var entry : ((Map<String, Object>) accounts).entrySet()) {
                if (entry.getKey().equalsIgnoreCase(normalizedAccountId)) {
                    String matchMode = extractTableMode(entry.getValue());
                    if (matchMode != null) {
                        return matchMode;
                    }
                    break;
                }
            }
        }

        // Section-level setting
        return extractTableMode(section);
    }

    @SuppressWarnings("unchecked")
    private static String extractTableMode(Object entry) {
        if (!(entry instanceof Map<?, ?> map)) {
            return null;
        }
        // Check entry.markdown.tables
        Object markdownRaw = map.get("markdown");
        if (markdownRaw instanceof Map<?, ?> markdown) {
            Object tables = markdown.get("tables");
            if (isMarkdownTableMode(tables)) {
                return (String) tables;
            }
        }
        // Check entry.tables (legacy)
        Object tables = map.get("tables");
        if (isMarkdownTableMode(tables)) {
            return (String) tables;
        }
        return null;
    }

    private static String normalizeChannelId(String channel) {
        if (channel == null || channel.isBlank()) {
            return null;
        }
        return channel.trim().toLowerCase();
    }

    private static String normalizeAccountId(String accountId) {
        if (accountId == null || accountId.isBlank()) {
            return "";
        }
        return accountId.trim().toLowerCase();
    }
}

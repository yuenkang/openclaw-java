package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Collects status issues from all configured channel plugins.
 * <p>
 * Corresponds to TypeScript's infra/channels-status-issues.ts.
 */
public final class ChannelStatusIssues {

    private ChannelStatusIssues() {
    }

    private static final Logger log = LoggerFactory.getLogger(ChannelStatusIssues.class);

    // =========================================================================
    // Data model
    // =========================================================================

    public enum Severity {
        ERROR, WARNING, INFO
    }

    /**
     * A single issue found during channel status inspection.
     */
    public record Issue(
            String channelId,
            String accountLabel,
            Severity severity,
            String message) {
    }

    /**
     * Collector interface that channels can implement to report issues.
     */
    @FunctionalInterface
    public interface IssueCollector {
        /**
         * Inspect the channel's accounts and return any issues.
         *
         * @param channelId      the channel identifier (e.g. "telegram")
         * @param accountConfigs raw account configuration list
         * @return list of issues, may be empty
         */
        List<Issue> collect(String channelId, List<Map<String, Object>> accountConfigs);
    }

    // =========================================================================
    // Registry
    // =========================================================================

    private static final List<IssueCollector> collectors = Collections.synchronizedList(new ArrayList<>());

    /**
     * Register a channel-specific issue collector.
     */
    public static void registerCollector(IssueCollector collector) {
        if (collector != null) {
            collectors.add(collector);
        }
    }

    /**
     * Collect issues from all registered collectors.
     *
     * @param channelAccounts map from channelId to list of account configs
     * @return all collected issues
     */
    @SuppressWarnings("unchecked")
    public static List<Issue> collectAll(Map<String, Object> channelAccounts) {
        if (channelAccounts == null || channelAccounts.isEmpty()) {
            return List.of();
        }

        List<Issue> issues = new ArrayList<>();
        for (IssueCollector collector : collectors) {
            for (Map.Entry<String, Object> entry : channelAccounts.entrySet()) {
                String channelId = entry.getKey();
                Object raw = entry.getValue();
                if (!(raw instanceof List<?>)) {
                    continue;
                }
                try {
                    List<Map<String, Object>> accounts = (List<Map<String, Object>>) raw;
                    List<Issue> result = collector.collect(channelId, accounts);
                    if (result != null) {
                        issues.addAll(result);
                    }
                } catch (ClassCastException e) {
                    log.debug("Invalid account config format for channel {}", channelId);
                }
            }
        }
        return issues;
    }

    /**
     * Reset registered collectors (for testing).
     */
    public static void resetForTest() {
        collectors.clear();
    }
}

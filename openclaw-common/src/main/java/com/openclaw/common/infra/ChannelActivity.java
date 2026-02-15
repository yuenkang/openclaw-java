package com.openclaw.common.infra;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Channel activity tracker — records inbound/outbound timestamps per
 * channel+account.
 * Used for heartbeat scheduling and channel readiness checks.
 * Corresponds to TypeScript's infra/channel-activity.ts.
 */
public final class ChannelActivity {

    private ChannelActivity() {
    }

    public enum Direction {
        INBOUND, OUTBOUND
    }

    /**
     * Activity entry for a channel+account pair.
     */
    public static class Entry {
        private volatile Long inboundAt;
        private volatile Long outboundAt;

        public Long getInboundAt() {
            return inboundAt;
        }

        public Long getOutboundAt() {
            return outboundAt;
        }
    }

    private static final Map<String, Entry> activity = new ConcurrentHashMap<>();

    private static String keyFor(String channel, String accountId) {
        return channel + ":" + (accountId != null && !accountId.isBlank() ? accountId.trim() : "default");
    }

    /**
     * Record channel activity (inbound or outbound).
     *
     * @param channel   channel identifier (e.g. "telegram", "discord")
     * @param accountId account identifier (nullable, defaults to "default")
     * @param direction inbound or outbound
     */
    public static void record(String channel, String accountId, Direction direction) {
        record(channel, accountId, direction, System.currentTimeMillis());
    }

    public static void record(String channel, String accountId, Direction direction, long at) {
        Entry entry = activity.computeIfAbsent(keyFor(channel, accountId), k -> new Entry());
        if (direction == Direction.INBOUND) {
            entry.inboundAt = at;
        } else {
            entry.outboundAt = at;
        }
    }

    /**
     * Get activity entry for a channel+account pair.
     *
     * @return the entry, or a new empty entry if no activity recorded
     */
    public static Entry get(String channel, String accountId) {
        Entry entry = activity.get(keyFor(channel, accountId));
        return entry != null ? entry : new Entry();
    }

    /**
     * Clear all activity (for testing).
     */
    public static void resetForTest() {
        activity.clear();
    }
}

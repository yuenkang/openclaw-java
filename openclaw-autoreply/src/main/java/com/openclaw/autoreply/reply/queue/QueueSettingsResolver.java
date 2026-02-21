package com.openclaw.autoreply.reply.queue;

import java.util.Map;

/**
 * Resolves effective queue settings from config, session, inline overrides.
 * Mirrors {@code auto-reply/reply/queue/settings.ts}.
 */
public final class QueueSettingsResolver {

    private QueueSettingsResolver() {
    }

    /** Default queue mode when no override is found. */
    public static String defaultQueueModeForChannel(String channel) {
        return QueueTypes.MODE_COLLECT;
    }

    /**
     * Resolve per-channel debounce from a {@code debounceMsByChannel} map.
     */
    public static Integer resolveChannelDebounce(Map<String, Object> byChannel, String channelKey) {
        if (channelKey == null || channelKey.isBlank() || byChannel == null)
            return null;
        Object value = byChannel.get(channelKey);
        if (value instanceof Number n) {
            double d = n.doubleValue();
            if (Double.isFinite(d))
                return (int) Math.max(0, d);
        }
        return null;
    }

    /**
     * Resolve queue settings from config + session + inline overrides.
     *
     * @param cfgQueue      the {@code messages.queue} config section (may be null)
     * @param channelKey    normalized channel key (may be null)
     * @param sessionEntry  session entry (may contain queueMode, queueDebounceMs,
     *                      etc.)
     * @param inlineMode    inline-directive queue mode (may be null)
     * @param inlineOptions inline-directive partial settings (may be null)
     * @return resolved settings
     */
    @SuppressWarnings("unchecked")
    public static QueueTypes.QueueSettings resolveQueueSettings(
            Map<String, Object> cfgQueue,
            String channelKey,
            Map<String, Object> sessionEntry,
            String inlineMode,
            QueueTypes.QueueSettings inlineOptions) {

        String normalizedChannel = channelKey != null ? channelKey.trim().toLowerCase() : null;

        // Resolve mode cascade
        String providerModeRaw = null;
        if (normalizedChannel != null && cfgQueue != null) {
            Object byChannel = cfgQueue.get("byChannel");
            if (byChannel instanceof Map<?, ?> map) {
                Object raw = map.get(normalizedChannel);
                if (raw instanceof String s)
                    providerModeRaw = s;
            }
        }

        String sessionMode = sessionEntry != null
                ? QueueNormalize.normalizeQueueMode((String) sessionEntry.get("queueMode"))
                : null;
        String cfgMode = cfgQueue != null
                ? QueueNormalize.normalizeQueueMode((String) cfgQueue.get("mode"))
                : null;

        String resolvedMode = inlineMode != null ? inlineMode
                : sessionMode != null ? sessionMode
                        : QueueNormalize.normalizeQueueMode(providerModeRaw) != null
                                ? QueueNormalize.normalizeQueueMode(providerModeRaw)
                                : cfgMode != null ? cfgMode
                                        : defaultQueueModeForChannel(normalizedChannel);

        // Debounce cascade
        Integer inlineDebounce = inlineOptions != null ? inlineOptions.debounceMs() : null;
        Integer sessionDebounce = sessionEntry != null && sessionEntry.get("queueDebounceMs") instanceof Number n
                ? n.intValue()
                : null;
        Object byChannelMap = cfgQueue != null ? cfgQueue.get("debounceMsByChannel") : null;
        Integer channelDebounce = byChannelMap instanceof Map<?, ?>
                ? resolveChannelDebounce((Map<String, Object>) byChannelMap, normalizedChannel)
                : null;
        Integer cfgDebounce = cfgQueue != null && cfgQueue.get("debounceMs") instanceof Number n
                ? n.intValue()
                : null;

        int debounceMs = first(inlineDebounce, sessionDebounce, channelDebounce, cfgDebounce,
                QueueState.DEFAULT_QUEUE_DEBOUNCE_MS);

        // Cap cascade
        Integer inlineCap = inlineOptions != null ? inlineOptions.cap() : null;
        Integer sessionCap = sessionEntry != null && sessionEntry.get("queueCap") instanceof Number n
                ? n.intValue()
                : null;
        Integer cfgCap = cfgQueue != null && cfgQueue.get("cap") instanceof Number n
                ? n.intValue()
                : null;

        int cap = firstPositive(inlineCap, sessionCap, cfgCap, QueueState.DEFAULT_QUEUE_CAP);

        // Drop policy cascade
        String inlineDrop = inlineOptions != null ? inlineOptions.dropPolicy() : null;
        String sessionDrop = sessionEntry != null
                ? QueueNormalize.normalizeQueueDropPolicy((String) sessionEntry.get("queueDrop"))
                : null;
        String cfgDrop = cfgQueue != null
                ? QueueNormalize.normalizeQueueDropPolicy((String) cfgQueue.get("drop"))
                : null;

        String drop = inlineDrop != null ? inlineDrop
                : sessionDrop != null ? sessionDrop
                        : cfgDrop != null ? cfgDrop
                                : QueueState.DEFAULT_QUEUE_DROP;

        return new QueueTypes.QueueSettings(resolvedMode, Math.max(0, debounceMs), cap, drop);
    }

    private static int first(Integer... values) {
        for (Integer v : values) {
            if (v != null)
                return v;
        }
        return 0;
    }

    private static int firstPositive(Integer... values) {
        for (Integer v : values) {
            if (v != null && v > 0)
                return v;
        }
        return QueueState.DEFAULT_QUEUE_CAP;
    }
}

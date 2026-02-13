package com.openclaw.gateway.cron;

import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Cron job input normalization — coerces raw user input into well-typed
 * create/patch objects.
 * Corresponds to TypeScript's cron/normalize.ts.
 */
public final class CronNormalize {

    private CronNormalize() {
    }

    /**
     * Normalize a raw schedule map (auto-detect kind, coerce at/atMs).
     */
    public static Map<String, Object> coerceSchedule(Map<String, Object> schedule) {
        Map<String, Object> next = new LinkedHashMap<>(schedule);
        String kind = schedule.get("kind") instanceof String k ? k : null;

        Object atMsRaw = schedule.get("atMs");
        Object atRaw = schedule.get("at");
        String atString = atRaw instanceof String s ? s.trim() : "";

        Long parsedAtMs = null;
        if (atMsRaw instanceof Number n) {
            parsedAtMs = n.longValue();
        } else if (atMsRaw instanceof String s) {
            parsedAtMs = CronParse.parseAbsoluteTimeMs(s);
        } else if (!atString.isEmpty()) {
            parsedAtMs = CronParse.parseAbsoluteTimeMs(atString);
        }

        // Auto-detect kind
        if (kind == null) {
            if (schedule.containsKey("atMs") || schedule.get("at") instanceof String) {
                next.put("kind", "at");
            } else if (schedule.get("everyMs") instanceof Number) {
                next.put("kind", "every");
            } else if (schedule.get("expr") instanceof String) {
                next.put("kind", "cron");
            }
        }

        // Normalize at field
        if (!atString.isEmpty()) {
            next.put("at", parsedAtMs != null ? Instant.ofEpochMilli(parsedAtMs).toString() : atString);
        } else if (parsedAtMs != null) {
            next.put("at", Instant.ofEpochMilli(parsedAtMs).toString());
        }
        next.remove("atMs");

        return next;
    }

    /**
     * Normalize delivery config (mode aliases, channel/to trimming).
     */
    public static Map<String, Object> coerceDelivery(Map<String, Object> delivery) {
        Map<String, Object> next = new LinkedHashMap<>(delivery);

        if (delivery.get("mode") instanceof String mode) {
            String normalized = mode.trim().toLowerCase();
            next.put("mode", "deliver".equals(normalized) ? "announce" : normalized);
        }
        if (delivery.get("channel") instanceof String ch) {
            String trimmed = ch.trim().toLowerCase();
            if (trimmed.isEmpty())
                next.remove("channel");
            else
                next.put("channel", trimmed);
        }
        if (delivery.get("to") instanceof String to) {
            String trimmed = to.trim();
            if (trimmed.isEmpty())
                next.remove("to");
            else
                next.put("to", trimmed);
        }

        return next;
    }

    /**
     * Normalize a cron job create map — coerce schedule/payload/delivery + apply
     * defaults.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> normalizeCronJobCreate(Map<String, Object> raw) {
        if (raw == null)
            return null;
        Map<String, Object> next = unwrapJob(raw);

        // Coerce schedule
        if (next.get("schedule") instanceof Map<?, ?> sched) {
            next.put("schedule", coerceSchedule((Map<String, Object>) sched));
        }

        // Coerce delivery
        if (next.get("delivery") instanceof Map<?, ?> del) {
            next.put("delivery", coerceDelivery((Map<String, Object>) del));
        }

        // Apply defaults
        if (!next.containsKey("wakeMode")) {
            next.put("wakeMode", "next-heartbeat");
        }
        if (!(next.get("enabled") instanceof Boolean)) {
            next.put("enabled", true);
        }

        return next;
    }

    /**
     * Normalize a cron job patch map — coerce but do NOT apply defaults.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> normalizeCronJobPatch(Map<String, Object> raw) {
        if (raw == null)
            return null;
        Map<String, Object> next = unwrapJob(raw);

        if (next.get("schedule") instanceof Map<?, ?> sched) {
            next.put("schedule", coerceSchedule((Map<String, Object>) sched));
        }
        if (next.get("delivery") instanceof Map<?, ?> del) {
            next.put("delivery", coerceDelivery((Map<String, Object>) del));
        }

        return next;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> unwrapJob(Map<String, Object> raw) {
        if (raw.get("data") instanceof Map<?, ?> data) {
            return new LinkedHashMap<>((Map<String, Object>) data);
        }
        if (raw.get("job") instanceof Map<?, ?> job) {
            return new LinkedHashMap<>((Map<String, Object>) job);
        }
        return new LinkedHashMap<>(raw);
    }
}

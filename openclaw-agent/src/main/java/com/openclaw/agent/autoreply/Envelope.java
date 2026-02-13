package com.openclaw.agent.autoreply;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * Agent envelope formatting — build envelope headers for inbound
 * messages with channel, sender, timestamp, elapsed time, and
 * timezone resolution.
 * Mirrors {@code auto-reply/envelope.ts}.
 */
public final class Envelope {

    private Envelope() {
    }

    /** Envelope format options. */
    public record EnvelopeFormatOptions(
            String timezone,
            boolean includeTimestamp,
            boolean includeElapsed,
            String userTimezone) {

        public static EnvelopeFormatOptions defaults() {
            return new EnvelopeFormatOptions("local", true, true, null);
        }
    }

    /** Parameters for formatAgentEnvelope. */
    public record AgentEnvelopeParams(
            String channel,
            String from,
            Long timestamp,
            String host,
            String ip,
            String body,
            Long previousTimestamp,
            EnvelopeFormatOptions envelope) {
    }

    public static EnvelopeFormatOptions resolveEnvelopeFormatOptions(Map<String, Object> cfg) {
        if (cfg == null)
            return EnvelopeFormatOptions.defaults();
        @SuppressWarnings("unchecked")
        Map<String, Object> agents = cfg.get("agents") instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : null;
        @SuppressWarnings("unchecked")
        Map<String, Object> defaults = agents != null && agents.get("defaults") instanceof Map<?, ?> m
                ? (Map<String, Object>) m
                : null;
        if (defaults == null)
            return EnvelopeFormatOptions.defaults();

        String tz = defaults.get("envelopeTimezone") instanceof String s ? s : "local";
        boolean includeTs = !"off".equals(defaults.get("envelopeTimestamp"));
        boolean includeElapsed = !"off".equals(defaults.get("envelopeElapsed"));
        String userTz = defaults.get("userTimezone") instanceof String s ? s : null;
        return new EnvelopeFormatOptions(tz, includeTs, includeElapsed, userTz);
    }

    public static String formatAgentEnvelope(AgentEnvelopeParams params) {
        String channel = params.channel() != null && !params.channel().isBlank()
                ? params.channel().trim()
                : "Channel";
        List<String> parts = new ArrayList<>();
        parts.add(channel);

        EnvelopeFormatOptions opts = params.envelope() != null
                ? params.envelope()
                : EnvelopeFormatOptions.defaults();

        String elapsed = null;
        if (opts.includeElapsed() && params.timestamp() != null && params.previousTimestamp() != null) {
            elapsed = formatElapsedTime(params.timestamp(), params.previousTimestamp());
        }

        String from = params.from() != null ? params.from().trim() : "";
        if (!from.isEmpty()) {
            parts.add(elapsed != null ? from + " +" + elapsed : from);
        } else if (elapsed != null) {
            parts.add("+" + elapsed);
        }

        if (params.host() != null && !params.host().isBlank())
            parts.add(params.host().trim());
        if (params.ip() != null && !params.ip().isBlank())
            parts.add(params.ip().trim());

        String ts = formatTimestamp(params.timestamp(), opts);
        if (ts != null)
            parts.add(ts);

        String header = "[" + String.join(" ", parts) + "]";
        return header + " " + params.body();
    }

    public static String formatInboundEnvelope(
            String channel, String from, String body,
            Long timestamp, String chatType, String senderLabel,
            Long previousTimestamp, EnvelopeFormatOptions envelope) {
        boolean isDirect = chatType == null || chatType.isEmpty() || "direct".equals(chatType);
        String resolvedSender = senderLabel != null && !senderLabel.isBlank()
                ? senderLabel.trim()
                : "";
        String envelopeBody = !isDirect && !resolvedSender.isEmpty()
                ? resolvedSender + ": " + body
                : body;
        return formatAgentEnvelope(new AgentEnvelopeParams(
                channel, from, timestamp, null, null, envelopeBody,
                previousTimestamp, envelope));
    }

    public static String formatInboundFromLabel(
            boolean isGroup, String groupLabel, String groupId,
            String directLabel, String directId, String groupFallback) {
        if (isGroup) {
            String label = (groupLabel != null && !groupLabel.isBlank())
                    ? groupLabel.trim()
                    : (groupFallback != null ? groupFallback : "Group");
            String id = groupId != null ? groupId.trim() : null;
            return id != null && !id.isEmpty() ? label + " id:" + id : label;
        }
        String dl = directLabel.trim();
        String di = directId != null ? directId.trim() : null;
        if (di == null || di.isEmpty() || di.equals(dl))
            return dl;
        return dl + " id:" + di;
    }

    public static String formatThreadStarterEnvelope(
            String channel, String author, Long timestamp,
            String body, EnvelopeFormatOptions envelope) {
        return formatAgentEnvelope(new AgentEnvelopeParams(
                channel, author, timestamp, null, null, body, null, envelope));
    }

    // --- private ---

    private static String formatTimestamp(Long ts, EnvelopeFormatOptions opts) {
        if (ts == null || !opts.includeTimestamp())
            return null;
        Instant instant = Instant.ofEpochMilli(ts);
        String tz = opts.timezone() != null ? opts.timezone().trim() : "local";
        String lowered = tz.toLowerCase();
        if ("utc".equals(lowered) || "gmt".equals(lowered)) {
            return DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm'Z'")
                    .withZone(ZoneOffset.UTC).format(instant);
        }
        if ("local".equals(lowered) || "host".equals(lowered)) {
            return DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm z")
                    .withZone(ZoneId.systemDefault()).format(instant);
        }
        try {
            ZoneId zone = ZoneId.of(tz);
            return DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm z")
                    .withZone(zone).format(instant);
        } catch (Exception e) {
            return DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm'Z'")
                    .withZone(ZoneOffset.UTC).format(instant);
        }
    }

    private static String formatElapsedTime(long currentMs, long previousMs) {
        long elapsedMs = currentMs - previousMs;
        if (elapsedMs < 0)
            return null;
        long seconds = elapsedMs / 1000;
        if (seconds < 60)
            return seconds + "s";
        long minutes = seconds / 60;
        if (minutes < 60)
            return minutes + "m";
        long hours = minutes / 60;
        if (hours < 24)
            return hours + "h";
        long days = hours / 24;
        return days + "d";
    }
}

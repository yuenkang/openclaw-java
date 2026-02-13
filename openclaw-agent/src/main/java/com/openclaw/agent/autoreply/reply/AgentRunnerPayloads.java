package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Build final reply payloads — sanitize heartbeat tokens, apply reply
 * threading, parse reply directives, filter messaging-tool duplicates,
 * and drop payloads already sent via block streaming.
 * Mirrors {@code auto-reply/reply/agent-runner-payloads.ts}.
 */
public final class AgentRunnerPayloads {

    private static final Logger log = LoggerFactory.getLogger(AgentRunnerPayloads.class);
    private static final Pattern HEARTBEAT_TOKEN_RE = Pattern.compile("HEARTBEAT_OK");

    private AgentRunnerPayloads() {
    }

    /** Result of buildReplyPayloads. */
    public record BuildResult(
            List<Map<String, Object>> replyPayloads,
            boolean didLogHeartbeatStrip) {
    }

    /**
     * Build reply payloads from raw agent output.
     *
     * @param payloads               raw payloads from agent run
     * @param isHeartbeat            whether this is a heartbeat run
     * @param didLogHeartbeatStrip   whether heartbeat strip was already logged
     * @param blockStreamingEnabled  whether block streaming is active
     * @param blockReplyPipeline     block reply pipeline (nullable)
     * @param directlySentBlockKeys  keys sent directly during tool flush
     * @param replyToMode            reply-to mode
     * @param currentMessageId       current inbound message id
     * @param messageProvider        message provider name
     * @param messagingToolSentTexts texts sent by messaging tool
     * @param originatingTo          originating to field
     * @param accountId              agent account id
     * @return sanitized, filtered reply payloads
     */
    @SuppressWarnings("unchecked")
    public static BuildResult buildReplyPayloads(
            List<Map<String, Object>> payloads,
            boolean isHeartbeat,
            boolean didLogHeartbeatStrip,
            boolean blockStreamingEnabled,
            Object blockReplyPipeline,
            Set<String> directlySentBlockKeys,
            String replyToMode,
            String currentMessageId,
            String messageProvider,
            List<String> messagingToolSentTexts,
            String originatingTo,
            String accountId) {

        boolean didLog = didLogHeartbeatStrip;

        // 1. Sanitize heartbeat tokens
        List<Map<String, Object>> sanitized;
        if (isHeartbeat) {
            sanitized = new ArrayList<>(payloads);
        } else {
            sanitized = new ArrayList<>();
            for (Map<String, Object> payload : payloads) {
                String text = (String) payload.get("text");

                // Format Bun fetch socket errors
                if (Boolean.TRUE.equals(payload.get("isError"))
                        && text != null && AgentRunnerUtils.isBunFetchSocketError(text)) {
                    text = AgentRunnerUtils.formatBunFetchSocketError(text);
                }

                if (text == null || !text.contains("HEARTBEAT_OK")) {
                    Map<String, Object> copy = new LinkedHashMap<>(payload);
                    copy.put("text", text);
                    sanitized.add(copy);
                    continue;
                }
                // Strip heartbeat token
                String stripped = HEARTBEAT_TOKEN_RE.matcher(text).replaceAll("").trim();
                if (!didLog) {
                    didLog = true;
                    log.debug("Stripped stray HEARTBEAT_OK token from reply");
                }
                boolean hasMedia = payload.get("mediaUrl") != null;
                if (!hasMedia) {
                    List<String> urls = (List<String>) payload.get("mediaUrls");
                    hasMedia = urls != null && !urls.isEmpty();
                }
                if (stripped.isEmpty() && !hasMedia)
                    continue;
                Map<String, Object> copy = new LinkedHashMap<>(payload);
                copy.put("text", stripped);
                sanitized.add(copy);
            }
        }

        // 2. Apply reply threading (inline — typed method expects ReplyPayload, using
        // Map here)
        List<Map<String, Object>> threaded = new ArrayList<>(sanitized); // simplified: skip reply threading for now

        // 3. Parse reply directives and filter
        List<Map<String, Object>> tagged = new ArrayList<>();
        for (Map<String, Object> payload : threaded) {
            String text = payload.get("text") != null ? payload.get("text").toString() : "";
            ReplyDirectiveTypes.ReplyDirectiveParseResult parsed = ReplyDirectives.parseReplyDirectives(text,
                    currentMessageId, null);

            Map<String, Object> copy = new LinkedHashMap<>(payload);
            if (parsed.text() != null && !parsed.text().isEmpty()) {
                copy.put("text", parsed.text());
            } else {
                copy.remove("text");
            }
            if (parsed.mediaUrl() != null && !copy.containsKey("mediaUrl")) {
                copy.put("mediaUrl", parsed.mediaUrl());
            }
            if (parsed.replyToId() != null && !copy.containsKey("replyToId")) {
                copy.put("replyToId", parsed.replyToId());
            }
            if (parsed.replyToCurrent() && !copy.containsKey("replyToCurrent")) {
                copy.put("replyToCurrent", true);
            }

            // Check if renderable (inline — typed method expects ReplyPayload)
            if (isRenderableMap(copy)) {
                tagged.add(copy);
            }
        }

        // 4. Filter messaging tool duplicates (inline)
        List<String> sentTexts = messagingToolSentTexts != null ? messagingToolSentTexts : List.of();
        List<Map<String, Object>> deduped;
        if (sentTexts.isEmpty()) {
            deduped = tagged;
        } else {
            deduped = new ArrayList<>();
            for (Map<String, Object> p : tagged) {
                String pText = p.get("text") != null ? p.get("text").toString() : "";
                boolean isDup = sentTexts.stream().anyMatch(
                        s -> s != null && !s.isEmpty() && pText.contains(s));
                if (!isDup)
                    deduped.add(p);
            }
        }

        // 5. Filter already-sent block payloads
        List<Map<String, Object>> filtered;
        if (blockStreamingEnabled && blockReplyPipeline != null) {
            // Full pipeline integration deferred — pass through
            filtered = deduped;
        } else if (directlySentBlockKeys != null && !directlySentBlockKeys.isEmpty()) {
            filtered = deduped.stream()
                    .filter(p -> {
                        String key = p.get("text") != null ? p.get("text").toString() : "";
                        return !directlySentBlockKeys.contains(key);
                    })
                    .toList();
        } else {
            filtered = deduped;
        }

        // 6. Suppress if messaging tool already replied to same target
        boolean suppress = ReplyPayloads.shouldSuppressMessagingToolReplies(
                messageProvider, List.of(), originatingTo, accountId);
        List<Map<String, Object>> result = suppress ? List.of() : filtered;

        return new BuildResult(result, didLog);
    }

    /** Check if a Map-based payload has renderable content. */
    private static boolean isRenderableMap(Map<String, Object> payload) {
        String text = payload.get("text") != null ? payload.get("text").toString() : null;
        if (text != null && !text.isEmpty())
            return true;
        if (payload.get("mediaUrl") != null)
            return true;
        Object urls = payload.get("mediaUrls");
        if (urls instanceof List<?> list && !list.isEmpty())
            return true;
        if (Boolean.TRUE.equals(payload.get("audioAsVoice")))
            return true;
        Object cd = payload.get("channelData");
        return cd instanceof Map<?, ?> map && !map.isEmpty();
    }
}

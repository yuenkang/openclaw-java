package com.openclaw.agent.runtime;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Tool-call ID sanitisation for provider compatibility.
 * <p>
 * Providers like Gemini and Mistral restrict tool-call IDs to
 * alphanumeric characters (and Mistral limits length to 9).
 * This class provides sanitisation + transcript-wide de-duplication
 * to avoid collisions introduced by stripping special characters.
 * <p>
 * Mirrors {@code agents/tool-call-id.ts}.
 */
public final class ToolCallIdSanitizer {

    private ToolCallIdSanitizer() {
    }

    public enum Mode {
        STRICT, STRICT9
    }

    private static final int STRICT9_LEN = 9;
    private static final int MAX_LEN = 40;
    private static final String ALNUM = "[^a-zA-Z0-9]";

    // --- Public API ---

    /** Sanitise a single ID (no dedup). */
    public static String sanitize(String id, Mode mode) {
        if (id == null || id.isEmpty()) {
            return mode == Mode.STRICT9 ? "defaultid" : "defaulttoolid";
        }
        String alphanumeric = id.replaceAll(ALNUM, "");

        if (mode == Mode.STRICT9) {
            if (alphanumeric.length() >= STRICT9_LEN) {
                return alphanumeric.substring(0, STRICT9_LEN);
            }
            if (!alphanumeric.isEmpty()) {
                return shortHash(alphanumeric, STRICT9_LEN);
            }
            return shortHash("sanitized", STRICT9_LEN);
        }

        return alphanumeric.isEmpty() ? "sanitizedtoolid" : alphanumeric;
    }

    /** Check whether an ID already satisfies provider constraints. */
    public static boolean isValid(String id, Mode mode) {
        if (id == null || id.isEmpty())
            return false;
        if (mode == Mode.STRICT9) {
            return id.matches("[a-zA-Z0-9]{9}");
        }
        return id.matches("[a-zA-Z0-9]+");
    }

    /**
     * Sanitise all tool-call IDs across a message transcript.
     * <p>
     * Each message is represented as a mutable {@link Map}. The method
     * rewrites {@code id}, {@code toolCallId}, and {@code toolUseId}
     * fields in-place, using a shared mapping to keep references
     * consistent between assistant {@code functionCall/toolUse/toolCall}
     * blocks and the corresponding {@code toolResult} messages.
     *
     * @return {@code true} if any IDs were rewritten.
     */
    public static boolean sanitizeTranscript(
            List<Map<String, Object>> messages, Mode mode) {
        if (messages == null || messages.isEmpty())
            return false;

        Map<String, String> mapping = new HashMap<>();
        Set<String> used = new HashSet<>();
        boolean changed = false;

        for (Map<String, Object> msg : messages) {
            if (msg == null)
                continue;
            String role = asString(msg.get("role"));

            if ("assistant".equals(role)) {
                changed |= rewriteAssistant(msg, mapping, used, mode);
            } else if ("toolResult".equals(role)) {
                changed |= rewriteToolResult(msg, mapping, used, mode);
            }
        }
        return changed;
    }

    // --- Rewriting helpers ---

    @SuppressWarnings("unchecked")
    private static boolean rewriteAssistant(
            Map<String, Object> msg,
            Map<String, String> mapping, Set<String> used, Mode mode) {
        Object content = msg.get("content");
        if (!(content instanceof List<?> list))
            return false;
        boolean changed = false;

        for (Object item : list) {
            if (!(item instanceof Map<?, ?> raw))
                continue;
            Map<String, Object> block = (Map<String, Object>) raw;
            String type = asString(block.get("type"));
            if (!"functionCall".equals(type) && !"toolUse".equals(type)
                    && !"toolCall".equals(type)) {
                continue;
            }
            String id = asString(block.get("id"));
            if (id == null || id.isEmpty())
                continue;
            String nextId = resolve(id, mapping, used, mode);
            if (!nextId.equals(id)) {
                block.put("id", nextId);
                changed = true;
            }
        }
        return changed;
    }

    private static boolean rewriteToolResult(
            Map<String, Object> msg,
            Map<String, String> mapping, Set<String> used, Mode mode) {
        boolean changed = false;

        String toolCallId = asString(msg.get("toolCallId"));
        if (toolCallId != null && !toolCallId.isEmpty()) {
            String next = resolve(toolCallId, mapping, used, mode);
            if (!next.equals(toolCallId)) {
                msg.put("toolCallId", next);
                changed = true;
            }
        }
        String toolUseId = asString(msg.get("toolUseId"));
        if (toolUseId != null && !toolUseId.isEmpty()) {
            String next = resolve(toolUseId, mapping, used, mode);
            if (!next.equals(toolUseId)) {
                msg.put("toolUseId", next);
                changed = true;
            }
        }
        return changed;
    }

    private static String resolve(String id, Map<String, String> mapping,
            Set<String> used, Mode mode) {
        String existing = mapping.get(id);
        if (existing != null)
            return existing;
        String next = makeUniqueId(id, used, mode);
        mapping.put(id, next);
        used.add(next);
        return next;
    }

    // --- Unique ID generation ---

    private static String makeUniqueId(String id, Set<String> used, Mode mode) {
        if (mode == Mode.STRICT9) {
            String base = sanitize(id, mode);
            if (base.length() >= STRICT9_LEN) {
                String candidate = base.substring(0, STRICT9_LEN);
                if (!used.contains(candidate))
                    return candidate;
            }
            for (int i = 0; i < 1000; i++) {
                String hashed = shortHash(id + ":" + i, STRICT9_LEN);
                if (!used.contains(hashed))
                    return hashed;
            }
            return shortHash(id + ":" + System.currentTimeMillis(), STRICT9_LEN);
        }

        // STRICT mode
        String base = sanitize(id, mode);
        if (base.length() > MAX_LEN)
            base = base.substring(0, MAX_LEN);
        if (!used.contains(base))
            return base;

        String hash = shortHash(id, 8);
        int maxBaseLen = MAX_LEN - hash.length();
        String clippedBase = base.length() > maxBaseLen
                ? base.substring(0, maxBaseLen)
                : base;
        String candidate = clippedBase + hash;
        if (!used.contains(candidate))
            return candidate;

        for (int i = 2; i < 1000; i++) {
            String suffix = "x" + i;
            String next = candidate.substring(
                    0, Math.min(candidate.length(), MAX_LEN - suffix.length())) + suffix;
            if (!used.contains(next))
                return next;
        }

        String ts = "t" + System.currentTimeMillis();
        return candidate.substring(
                0, Math.min(candidate.length(), MAX_LEN - ts.length())) + ts;
    }

    // --- Utilities ---

    private static String shortHash(String text, int length) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] digest = md.digest(text.getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (byte b : digest) {
                sb.append(String.format("%02x", b));
                if (sb.length() >= length)
                    break;
            }
            return sb.substring(0, Math.min(sb.length(), length));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    private static String asString(Object v) {
        return v instanceof String s ? s : null;
    }
}

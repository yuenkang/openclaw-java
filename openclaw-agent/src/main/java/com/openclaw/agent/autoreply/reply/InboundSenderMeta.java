package com.openclaw.agent.autoreply.reply;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Inbound sender metadata — format body text with a [from: …] line for
 * group/channel messages when sender is not already mentioned.
 * Mirrors {@code auto-reply/reply/inbound-sender-meta.ts}.
 */
public final class InboundSenderMeta {

    private InboundSenderMeta() {
    }

    /**
     * Append a {@code [from: senderLabel]} line to the body when the message
     * is a group/channel message and does not already contain sender metadata.
     */
    public static String formatInboundBodyWithSenderMeta(String body, Map<String, Object> ctx) {
        if (body == null || body.trim().isEmpty())
            return body;

        Object chatType = ctx.get("ChatType");
        String ct = chatType instanceof String s ? s.toLowerCase() : null;
        if (ct == null || "direct".equals(ct))
            return body;

        if (hasSenderMetaLine(body, ctx))
            return body;

        String senderLabel = resolveSenderLabel(ctx);
        if (senderLabel == null || senderLabel.isEmpty())
            return body;

        return body + "\n[from: " + senderLabel + "]";
    }

    private static boolean hasSenderMetaLine(String body, Map<String, Object> ctx) {
        if (Pattern.compile("(^|\\n)\\[from:", Pattern.CASE_INSENSITIVE).matcher(body).find()) {
            return true;
        }
        List<String> candidates = listSenderLabelCandidates(ctx);
        if (candidates.isEmpty())
            return false;

        return candidates.stream().anyMatch(candidate -> {
            String escaped = Pattern.quote(candidate);
            Pattern p = Pattern.compile("(^|\\n|]\\s*)" + escaped + ":\\s", Pattern.CASE_INSENSITIVE);
            return p.matcher(body).find();
        });
    }

    private static String resolveSenderLabel(Map<String, Object> ctx) {
        List<String> candidates = listSenderLabelCandidates(ctx);
        return candidates.isEmpty() ? null : candidates.get(0);
    }

    static List<String> listSenderLabelCandidates(Map<String, Object> ctx) {
        List<String> candidates = new ArrayList<>();
        addIfPresent(candidates, ctx, "SenderName");
        addIfPresent(candidates, ctx, "SenderUsername");
        addIfPresent(candidates, ctx, "SenderTag");
        addIfPresent(candidates, ctx, "SenderE164");
        addIfPresent(candidates, ctx, "SenderId");
        return candidates;
    }

    private static void addIfPresent(List<String> list, Map<String, Object> ctx, String key) {
        Object val = ctx.get(key);
        if (val instanceof String s && !s.trim().isEmpty()) {
            list.add(s.trim());
        }
    }
}

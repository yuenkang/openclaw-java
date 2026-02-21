package com.openclaw.autoreply.reply;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Finalize inbound message context — normalize text fields, resolve
 * chat types, conversation labels, and sender meta lines.
 * Mirrors {@code auto-reply/reply/inbound-context.ts}.
 */
public final class InboundContext {

    private InboundContext() {
    }

    /** Options for finalization. */
    public record FinalizeOptions(
            boolean forceBodyForAgent,
            boolean forceBodyForCommands,
            boolean forceChatType,
            boolean forceConversationLabel) {

        public FinalizeOptions() {
            this(false, false, false, false);
        }
    }

    /**
     * Normalize a text field — delegates to
     * InboundText.normalizeInboundTextNewlines.
     */
    private static String normalizeTextField(Object value) {
        if (!(value instanceof String s))
            return null;
        return InboundText.normalizeInboundTextNewlines(s);
    }

    /**
     * Finalize inbound context — normalize body text fields,
     * resolve chat type, conversation label, and append sender meta.
     *
     * @param ctx  mutable context map
     * @param opts finalization options
     */
    @SuppressWarnings("unchecked")
    public static void finalizeInboundContext(Map<String, Object> ctx, FinalizeOptions opts) {
        // Normalize Body
        Object rawBody = ctx.get("Body");
        String body = rawBody instanceof String s ? s : "";
        ctx.put("Body", InboundText.normalizeInboundTextNewlines(body));

        // Normalize optional text fields
        ctx.put("RawBody", normalizeTextField(ctx.get("RawBody")));
        ctx.put("CommandBody", normalizeTextField(ctx.get("CommandBody")));
        ctx.put("Transcript", normalizeTextField(ctx.get("Transcript")));
        ctx.put("ThreadStarterBody", normalizeTextField(ctx.get("ThreadStarterBody")));

        // Normalize UntrustedContext array
        Object uc = ctx.get("UntrustedContext");
        if (uc instanceof List<?> list) {
            List<String> normalized = new ArrayList<>();
            for (Object entry : list) {
                if (entry instanceof String s) {
                    String n = InboundText.normalizeInboundTextNewlines(s);
                    if (n != null && !n.isEmpty())
                        normalized.add(n);
                }
            }
            ctx.put("UntrustedContext", normalized);
        }

        // Chat type normalization (simplified — full implementation deferred)
        Object chatType = ctx.get("ChatType");
        if (chatType instanceof String ct) {
            String normalized = ct.toLowerCase().trim();
            if (opts.forceChatType() || !normalized.equals(ct)) {
                ctx.put("ChatType", normalized);
            }
        }

        // BodyForAgent
        String bodyForAgent;
        if (opts.forceBodyForAgent()) {
            bodyForAgent = (String) ctx.get("Body");
        } else {
            Object bfa = ctx.get("BodyForAgent");
            bodyForAgent = bfa instanceof String s ? s : (String) ctx.get("Body");
        }
        ctx.put("BodyForAgent", InboundText.normalizeInboundTextNewlines(bodyForAgent));

        // BodyForCommands
        String bodyForCommands;
        if (opts.forceBodyForCommands()) {
            bodyForCommands = coalesce(
                    (String) ctx.get("CommandBody"),
                    (String) ctx.get("RawBody"),
                    (String) ctx.get("Body"));
        } else {
            bodyForCommands = coalesce(
                    (String) ctx.get("BodyForCommands"),
                    (String) ctx.get("CommandBody"),
                    (String) ctx.get("RawBody"),
                    (String) ctx.get("Body"));
        }
        ctx.put("BodyForCommands", InboundText.normalizeInboundTextNewlines(bodyForCommands));

        // Conversation label (simplified)
        Object label = ctx.get("ConversationLabel");
        String explicitLabel = label instanceof String s ? s.trim() : null;
        if (opts.forceConversationLabel() || explicitLabel == null || explicitLabel.isEmpty()) {
            // Full label resolution deferred
        } else {
            ctx.put("ConversationLabel", explicitLabel);
        }

        // Sender meta line
        String finalBody = (String) ctx.get("Body");
        ctx.put("Body", InboundSenderMeta.formatInboundBodyWithSenderMeta(
                finalBody, ctx));
        String finalBfa = (String) ctx.get("BodyForAgent");
        ctx.put("BodyForAgent", InboundSenderMeta.formatInboundBodyWithSenderMeta(
                finalBfa, ctx));

        // Default-deny CommandAuthorized
        ctx.put("CommandAuthorized", Boolean.TRUE.equals(ctx.get("CommandAuthorized")));
    }

    private static String coalesce(String... values) {
        for (String v : values) {
            if (v != null)
                return v;
        }
        return "";
    }
}

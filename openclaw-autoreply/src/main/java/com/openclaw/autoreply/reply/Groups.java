package com.openclaw.autoreply.reply;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Group chat utilities — group ID extraction, mention resolution,
 * group intro prompt building.
 * Mirrors {@code auto-reply/reply/groups.ts}.
 */
public final class Groups {

    private Groups() {
    }

    /**
     * Extract a group ID from a raw session key.
     */
    public static String extractGroupId(String raw) {
        String trimmed = raw != null ? raw.trim() : "";
        if (trimmed.isEmpty())
            return null;

        String[] parts = trimmed.split(":");
        // Filter empty parts
        List<String> filtered = List.of(parts).stream()
                .filter(p -> !p.isEmpty()).collect(Collectors.toList());

        if (filtered.size() >= 3
                && ("group".equals(filtered.get(1)) || "channel".equals(filtered.get(1)))) {
            String joined = filtered.subList(2, filtered.size()).stream()
                    .collect(Collectors.joining(":"));
            return joined.isEmpty() ? null : joined;
        }
        if (filtered.size() >= 2
                && "whatsapp".equalsIgnoreCase(filtered.get(0))
                && trimmed.toLowerCase().contains("@g.us")) {
            String joined = filtered.subList(1, filtered.size()).stream()
                    .collect(Collectors.joining(":"));
            return joined.isEmpty() ? null : joined;
        }
        if (filtered.size() >= 2
                && ("group".equals(filtered.get(0)) || "channel".equals(filtered.get(0)))) {
            String joined = filtered.subList(1, filtered.size()).stream()
                    .collect(Collectors.joining(":"));
            return joined.isEmpty() ? null : joined;
        }
        return trimmed;
    }

    /**
     * Whether the agent should only respond when explicitly mentioned.
     * Simplified — returns {@code true} (require mention) by default.
     */
    public static boolean resolveGroupRequireMention(Object cfg, String channel) {
        // Full implementation deferred to channels subsystem
        return true;
    }

    /**
     * Default group activation based on mention requirement.
     */
    public static String defaultGroupActivation(boolean requireMention) {
        return requireMention ? "mention" : "always";
    }

    /**
     * Build the group intro system prompt fragment.
     *
     * @param groupSubject  group/channel name
     * @param groupMembers  CSV of member names
     * @param providerLabel display name for the messaging provider
     * @param activation    "always" | "mention"
     * @param silentToken   token the model emits to stay silent
     * @return assembled intro prompt
     */
    public static String buildGroupIntro(
            String groupSubject,
            String groupMembers,
            String providerLabel,
            String activation,
            String silentToken) {

        String label = providerLabel != null && !providerLabel.isEmpty() ? providerLabel : "chat";
        String subjectLine = groupSubject != null && !groupSubject.isBlank()
                ? String.format("You are replying inside the %s group \"%s\".", label, groupSubject.trim())
                : String.format("You are replying inside a %s group chat.", label);

        String membersLine = groupMembers != null && !groupMembers.isBlank()
                ? "Group members: " + groupMembers.trim() + "."
                : null;

        String activationLine = "always".equals(activation)
                ? "Activation: always-on (you receive every group message)."
                : "Activation: trigger-only (you are invoked only when explicitly mentioned; recent context may be included).";

        String silenceLine = "always".equals(activation)
                ? String.format(
                        "If no response is needed, reply with exactly \"%s\" (and nothing else) "
                                + "so OpenClaw stays silent. Do not add any other words, punctuation, "
                                + "tags, markdown/code blocks, or explanations.",
                        silentToken)
                : null;

        String cautionLine = "always".equals(activation)
                ? "Be extremely selective: reply only when directly addressed or clearly helpful. Otherwise stay silent."
                : null;

        String lurkLine = "Be a good group participant: mostly lurk and follow the conversation; "
                + "reply only when directly addressed or you can add clear value. Emoji reactions are welcome when available.";
        String styleLine = "Write like a human. Avoid Markdown tables. Don't type literal \\n sequences; "
                + "use real line breaks sparingly.";

        StringBuilder sb = new StringBuilder();
        for (String line : new String[] { subjectLine, membersLine, activationLine,
                silenceLine, cautionLine, lurkLine, styleLine }) {
            if (line != null) {
                if (sb.length() > 0)
                    sb.append(' ');
                sb.append(line);
            }
        }
        sb.append(" Address the specific sender noted in the message context.");
        return sb.toString();
    }
}

package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handle /tts (text-to-speech) commands — enable, disable, speak, settings.
 * Mirrors {@code auto-reply/reply/commands-tts.ts}.
 */
public final class CommandsTts {

    private static final Logger log = LoggerFactory.getLogger(CommandsTts.class);

    private CommandsTts() {
    }

    /** Parsed TTS command. */
    public record ParsedTtsCommand(String action, String args) {
    }

    /**
     * Parse a /tts command.
     *
     * @return parsed command or null
     */
    public static ParsedTtsCommand parseTtsCommand(String normalized) {
        if (normalized == null)
            return null;
        String lower = normalized.trim().toLowerCase();
        if (!lower.startsWith("/tts"))
            return null;

        String rest = normalized.trim().substring("/tts".length()).trim();
        if (rest.isEmpty())
            return new ParsedTtsCommand("help", "");

        String[] parts = rest.split("\\s+", 2);
        String action = parts[0].toLowerCase();
        String args = parts.length > 1 ? parts[1].trim() : "";
        return new ParsedTtsCommand(action, args);
    }

    /**
     * Build TTS usage help text.
     */
    public static AutoReplyTypes.ReplyPayload ttsUsage() {
        String text = """
                🔊 Text-to-Speech Commands

                /tts on — Enable TTS
                /tts off — Disable TTS
                /tts speak <text> — Speak text
                /tts provider [name] — Show/set provider
                /tts maxlen [n] — Show/set max length
                /tts summarize [on|off] — Enable summarization
                /tts status — Show TTS status""";
        return new AutoReplyTypes.ReplyPayload(text, null, null, null,
                false, false, false, false, null);
    }

    /**
     * Handle a /tts command.
     *
     * @return reply or null if not a TTS command
     */
    public static AutoReplyTypes.ReplyPayload handleTtsCommand(
            String normalized, boolean allowTextCommands,
            boolean isAuthorized, String senderId) {

        if (!allowTextCommands)
            return null;

        ParsedTtsCommand parsed = parseTtsCommand(normalized);
        if (parsed == null)
            return null;

        if (!isAuthorized) {
            log.debug("Ignoring /tts from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        return switch (parsed.action()) {
            case "help", "" -> ttsUsage();
            case "on" -> replyText("🔊 TTS enabled.");
            case "off" -> replyText("🔇 TTS disabled.");
            case "status" -> replyText("🔊 TTS status: integration deferred.");
            case "speak" -> {
                if (parsed.args().isEmpty()) {
                    yield replyText("Usage: /tts speak <text>");
                }
                // Full TTS synthesis deferred
                yield replyText("🔊 Speaking: " + truncate(parsed.args(), 100));
            }
            case "provider" -> {
                if (parsed.args().isEmpty()) {
                    yield replyText("🔊 TTS provider: (deferred)");
                }
                yield replyText("🔊 TTS provider set to: " + parsed.args());
            }
            case "maxlen" -> {
                if (parsed.args().isEmpty()) {
                    yield replyText("🔊 TTS max length: (deferred)");
                }
                yield replyText("🔊 TTS max length set to: " + parsed.args());
            }
            case "summarize" -> {
                String val = parsed.args().toLowerCase();
                if (val.isEmpty()) {
                    yield replyText("🔊 Summarization: (deferred)");
                }
                yield replyText("🔊 Summarization " + ("on".equals(val) ? "enabled" : "disabled") + ".");
            }
            default -> ttsUsage();
        };
    }

    /* ── helpers ────────────────────────────────────────────── */

    private static AutoReplyTypes.ReplyPayload replyText(String text) {
        return new AutoReplyTypes.ReplyPayload(text, null, null, null,
                false, false, false, false, null);
    }

    private static String truncate(String s, int maxLen) {
        return s.length() <= maxLen ? s : s.substring(0, maxLen) + "…";
    }
}

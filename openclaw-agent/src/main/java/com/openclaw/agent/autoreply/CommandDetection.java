package com.openclaw.agent.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.regex.Pattern;

/**
 * Detect control commands and inline directive tokens in inbound text.
 * Mirrors {@code auto-reply/command-detection.ts}.
 */
public final class CommandDetection {

    private static final Logger log = LoggerFactory.getLogger(CommandDetection.class);
    private static final Pattern INLINE_COMMAND_TOKEN = Pattern.compile("(?:^|\\s)[/!][a-z]", Pattern.CASE_INSENSITIVE);

    private CommandDetection() {
    }

    /**
     * Check whether inbound text matches a registered slash command.
     */
    public static boolean hasControlCommand(String text, Map<String, Object> cfg) {
        if (text == null || text.isBlank())
            return false;
        String normalized = CommandNormalizer.normalizeCommandBody(text.trim(), null);
        if (normalized == null || normalized.isBlank())
            return false;
        String lowered = normalized.toLowerCase();
        // Full listChatCommands integration deferred
        return lowered.startsWith("/");
    }

    /**
     * Check whether inbound text is a control command or abort trigger.
     */
    public static boolean isControlCommandMessage(String text, Map<String, Object> cfg) {
        if (text == null || text.isBlank())
            return false;
        String trimmed = text.trim();
        return hasControlCommand(trimmed, cfg);
    }

    /**
     * Coarse detection for inline directives/shortcuts (e.g. "hey /status")
     * so channel monitors can decide whether to compute CommandAuthorized.
     */
    public static boolean hasInlineCommandTokens(String text) {
        if (text == null || text.isBlank())
            return false;
        return INLINE_COMMAND_TOKEN.matcher(text).find();
    }

    /**
     * Determine whether the CommandAuthorized field should be computed.
     */
    public static boolean shouldComputeCommandAuthorized(String text, Map<String, Object> cfg) {
        return isControlCommandMessage(text, cfg) || hasInlineCommandTokens(text);
    }
}

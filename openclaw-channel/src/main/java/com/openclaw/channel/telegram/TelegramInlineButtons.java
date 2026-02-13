package com.openclaw.channel.telegram;

/**
 * Telegram inline buttons scope resolution.
 * Corresponds to TypeScript's telegram/inline-buttons.ts.
 */
public final class TelegramInlineButtons {

    private TelegramInlineButtons() {
    }

    /** Inline button scope options. */
    public enum Scope {
        OFF, DM, GROUP, ALL, ALLOWLIST
    }

    /**
     * Parse a raw scope string into a typed Scope enum value.
     */
    public static Scope parseScope(String value) {
        if (value == null)
            return Scope.ALLOWLIST; // default
        return switch (value.trim().toLowerCase()) {
            case "off" -> Scope.OFF;
            case "dm" -> Scope.DM;
            case "group" -> Scope.GROUP;
            case "all" -> Scope.ALL;
            case "allowlist" -> Scope.ALLOWLIST;
            default -> Scope.ALLOWLIST;
        };
    }

    /**
     * Determine the target chat type from a target string.
     */
    public static String resolveTargetChatType(String target) {
        if (target == null || target.isBlank())
            return "unknown";
        var parsed = TelegramTargets.parse(target);
        String chatId = parsed.chatId().trim();
        if (chatId.isEmpty())
            return "unknown";
        if (chatId.matches("-?\\d+")) {
            return chatId.startsWith("-") ? "group" : "direct";
        }
        return "unknown";
    }
}

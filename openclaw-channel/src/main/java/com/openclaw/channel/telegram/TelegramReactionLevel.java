package com.openclaw.channel.telegram;

/**
 * Telegram reaction level configuration.
 * Corresponds to TypeScript's telegram/reaction-level.ts.
 */
public final class TelegramReactionLevel {

    private TelegramReactionLevel() {
    }

    /** Reaction level options. */
    public enum Level {
        OFF, ACK, MINIMAL, EXTENSIVE
    }

    /**
     * Resolved reaction level with derived flags.
     */
    public record Resolved(
            Level level,
            boolean ackEnabled,
            boolean agentReactionsEnabled,
            String agentReactionGuidance // "minimal" | "extensive" | null
    ) {
    }

    /**
     * Resolve the effective reaction level from a raw config string.
     */
    public static Resolved resolve(String rawLevel) {
        Level level;
        try {
            level = rawLevel != null ? Level.valueOf(rawLevel.toUpperCase()) : Level.MINIMAL;
        } catch (IllegalArgumentException e) {
            level = Level.ACK;
        }

        return switch (level) {
            case OFF -> new Resolved(level, false, false, null);
            case ACK -> new Resolved(level, true, false, null);
            case MINIMAL -> new Resolved(level, false, true, "minimal");
            case EXTENSIVE -> new Resolved(level, false, true, "extensive");
        };
    }
}

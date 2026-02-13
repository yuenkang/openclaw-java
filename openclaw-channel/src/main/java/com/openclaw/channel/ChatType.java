package com.openclaw.channel;

/**
 * Normalized chat type enumeration and parser.
 * Corresponds to TypeScript's channels/chat-type.ts.
 */
public final class ChatType {

    private ChatType() {
    }

    public static final String DIRECT = "direct";
    public static final String GROUP = "group";
    public static final String CHANNEL = "channel";

    /**
     * Normalize a raw chat type string to one of the canonical values.
     *
     * @param raw raw chat type string
     * @return "direct", "group", "channel", or null if invalid
     */
    public static String normalize(String raw) {
        if (raw == null) {
            return null;
        }
        String value = raw.trim().toLowerCase();
        if (value.isEmpty()) {
            return null;
        }
        return switch (value) {
            case "direct", "dm" -> DIRECT;
            case "group" -> GROUP;
            case "channel" -> CHANNEL;
            default -> null;
        };
    }
}

package com.openclaw.agent.autoreply.reply;

/**
 * Build a {@link CommandsTypes.CommandContext} from message context.
 * Mirrors {@code auto-reply/reply/commands-context.ts}.
 */
public final class CommandsContextBuilder {

    private CommandsContextBuilder() {
    }

    /**
     * Build a command context from the incoming message parameters.
     *
     * @param surface               the surface (telegram, whatsapp, etc.)
     * @param channel               the channel (provider)
     * @param channelId             provider-specific channel ID
     * @param senderId              sender identifier
     * @param from                  sender address
     * @param to                    recipient address
     * @param isAuthorizedSender    whether sender is authorized
     * @param senderIsOwner         whether sender is the owner
     * @param ownerList             list of owner identifiers
     * @param rawBodyNormalized     the raw body normalized
     * @param commandBodyNormalized the normalized command body
     * @return command context
     */
    public static CommandsTypes.CommandContext buildCommandContext(
            String surface, String channel, String channelId,
            String senderId, String from, String to,
            boolean isAuthorizedSender, boolean senderIsOwner,
            java.util.List<String> ownerList,
            String rawBodyNormalized, String commandBodyNormalized) {

        return new CommandsTypes.CommandContext(
                surface != null ? surface : "",
                channel != null ? channel : "",
                channelId,
                ownerList != null ? ownerList : java.util.List.of(),
                senderIsOwner,
                isAuthorizedSender,
                senderId,
                null, // abortKey derived externally
                rawBodyNormalized != null ? rawBodyNormalized : "",
                commandBodyNormalized != null ? commandBodyNormalized : "",
                from,
                to);
    }
}

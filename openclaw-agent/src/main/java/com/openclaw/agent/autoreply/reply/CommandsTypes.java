package com.openclaw.agent.autoreply.reply;

import java.util.List;
import java.util.Map;

/**
 * Command handler types and context for the auto-reply command subsystem.
 * Mirrors {@code auto-reply/reply/commands-types.ts}.
 */
public final class CommandsTypes {

    private CommandsTypes() {
    }

    /** Context for a command invocation (surface, channel, sender info). */
    public record CommandContext(
            String surface,
            String channel,
            String channelId,
            List<String> ownerList,
            boolean senderIsOwner,
            boolean isAuthorizedSender,
            String senderId,
            String abortKey,
            String rawBodyNormalized,
            String commandBodyNormalized,
            String from,
            String to) {
    }

    /** Parameters passed to command handlers. */
    public record HandleCommandsParams(
            Map<String, Object> ctx,
            Map<String, Object> cfg,
            CommandContext command,
            String agentId,
            Map<String, Object> directives,
            ElevatedInfo elevated,
            Map<String, Object> sessionEntry,
            Map<String, Object> previousSessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String storePath,
            String sessionScope,
            String workspaceDir,
            String resolvedThinkLevel,
            String resolvedVerboseLevel,
            String resolvedReasoningLevel,
            String resolvedElevatedLevel,
            String provider,
            String model,
            int contextTokens,
            boolean isGroup) {
    }

    /** Elevated execution info. */
    public record ElevatedInfo(boolean enabled, boolean allowed, List<GateFailure> failures) {
    }

    /** A single gate failure entry. */
    public record GateFailure(String gate, String key) {
    }

    /** Result of a command handler. */
    public record CommandHandlerResult(
            Map<String, Object> reply,
            boolean shouldContinue) {
    }
}

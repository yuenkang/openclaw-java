package com.openclaw.agent.autoreply.reply;

import com.openclaw.agent.autoreply.AutoReplyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * Handle /models command — list providers and models.
 * Mirrors {@code auto-reply/reply/commands-models.ts}.
 */
public final class CommandsModels {

    private static final Logger log = LoggerFactory.getLogger(CommandsModels.class);

    private static final int PAGE_SIZE_DEFAULT = 20;
    private static final int PAGE_SIZE_MAX = 100;

    private CommandsModels() {
    }

    /** Aggregated provider/model data. */
    public record ModelsProviderData(
            Map<String, Set<String>> byProvider,
            List<String> providers,
            String defaultProvider,
            String defaultModel) {
    }

    /** Parsed /models command arguments. */
    public record ParsedModelsArgs(
            String provider,
            int page,
            int pageSize,
            boolean all) {
    }

    /**
     * Parse /models command arguments.
     */
    public static ParsedModelsArgs parseModelsArgs(String raw) {
        if (raw == null || raw.isBlank()) {
            return new ParsedModelsArgs(null, 1, PAGE_SIZE_DEFAULT, false);
        }
        String[] tokens = raw.trim().split("\\s+");
        String provider = null;
        int page = 1;
        int pageSize = PAGE_SIZE_DEFAULT;
        boolean all = false;

        for (String token : tokens) {
            String lower = token.toLowerCase();
            if ("all".equals(lower) || "--all".equals(lower)) {
                all = true;
            } else if (lower.startsWith("page=") || lower.startsWith("p=")) {
                String val = lower.substring(lower.indexOf('=') + 1);
                try {
                    page = Math.max(1, Integer.parseInt(val));
                } catch (NumberFormatException ignored) {
                }
            } else if (lower.startsWith("size=") || lower.startsWith("n=")) {
                String val = lower.substring(lower.indexOf('=') + 1);
                try {
                    pageSize = Math.max(1, Math.min(PAGE_SIZE_MAX, Integer.parseInt(val)));
                } catch (NumberFormatException ignored) {
                }
            } else if (provider == null && !lower.startsWith("-")) {
                provider = token;
            }
        }
        return new ParsedModelsArgs(provider, page, pageSize, all);
    }

    /**
     * Format a provider line for display.
     */
    public static String formatProviderLine(String provider, int count) {
        return "  " + provider + " (" + count + " models)";
    }

    /**
     * Handle a /models command.
     *
     * @return reply or null if not a /models command
     */
    public static AutoReplyTypes.ReplyPayload handleModelsCommand(
            String commandBodyNormalized, boolean allowTextCommands,
            boolean isAuthorized, String senderId) {

        if (!allowTextCommands)
            return null;
        if (commandBodyNormalized == null)
            return null;

        String lower = commandBodyNormalized.toLowerCase();
        if (!lower.equals("/models") && !lower.startsWith("/models "))
            return null;

        if (!isAuthorized) {
            log.debug("Ignoring /models from unauthorized sender: {}",
                    senderId != null ? senderId : "<unknown>");
            return null;
        }

        String rest = commandBodyNormalized.substring("/models".length()).trim();
        ParsedModelsArgs args = parseModelsArgs(rest);

        // Full model catalog integration deferred
        StringBuilder sb = new StringBuilder();
        sb.append("📦 Available models");
        if (args.provider() != null) {
            sb.append(" for provider: ").append(args.provider());
        }
        sb.append("\n\n");
        sb.append("(Model catalog integration deferred)");
        if (args.page() > 1) {
            sb.append("\n\nPage ").append(args.page());
        }

        return new AutoReplyTypes.ReplyPayload(
                sb.toString(), null, null, null,
                false, false, false, false, null);
    }
}

package com.openclaw.agent.autoreply.reply;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Elevated permissions — sender approval, allow-list matching, unavailable
 * message.
 * Mirrors {@code auto-reply/reply/reply-elevated.ts}.
 */
public final class ReplyElevated {

    private ReplyElevated() {
    }

    /* ── token helpers ─────────────────────────────────────── */

    static String normalizeAllowToken(String value) {
        return value != null ? value.trim().toLowerCase() : "";
    }

    static String slugAllowToken(String value) {
        if (value == null)
            return "";
        String text = value.trim().toLowerCase();
        if (text.isEmpty())
            return "";
        text = text.replaceAll("^[@#]+", "");
        text = text.replaceAll("[\\s_]+", "-");
        text = text.replaceAll("[^a-z0-9-]+", "-");
        text = text.replaceAll("-{2,}", "-").replaceAll("^-+|-+$", "");
        return text;
    }

    private static final Pattern SENDER_PREFIX_RE = Pattern.compile(
            "^(whatsapp|telegram|discord|slack|line|signal|matrix|webchat|internal|user|group|channel):",
            Pattern.CASE_INSENSITIVE);

    static String stripSenderPrefix(String value) {
        if (value == null)
            return "";
        return SENDER_PREFIX_RE.matcher(value.trim()).replaceFirst("");
    }

    /* ── allow-list resolution ─────────────────────────────── */

    /**
     * Resolve the elevated allow-list for a given provider.
     */
    public static List<String> resolveElevatedAllowList(
            Map<String, List<String>> allowFrom,
            String provider,
            List<String> fallback) {
        if (allowFrom == null)
            return fallback;
        List<String> value = allowFrom.get(provider);
        return value != null ? value : fallback;
    }

    /** Sender context for elevated permission checks. */
    public record SenderContext(
            String senderName,
            String senderUsername,
            String senderTag,
            String senderE164,
            String from,
            String to,
            String accountId) {
    }

    /**
     * Check whether the sender is approved for elevated access.
     */
    public static boolean isApprovedElevatedSender(
            String provider,
            SenderContext ctx,
            Map<String, List<String>> allowFrom,
            List<String> fallbackAllowFrom) {

        List<String> rawAllow = resolveElevatedAllowList(allowFrom, provider, fallbackAllowFrom);
        if (rawAllow == null || rawAllow.isEmpty())
            return false;

        List<String> allowTokens = rawAllow.stream()
                .map(e -> e != null ? e.trim() : "")
                .filter(e -> !e.isEmpty())
                .toList();
        if (allowTokens.isEmpty())
            return false;
        if (allowTokens.stream().anyMatch(e -> "*".equals(e)))
            return true;

        Set<String> tokens = new HashSet<>();
        for (String v : new String[] { ctx.senderName(), ctx.senderUsername(), ctx.senderTag(),
                ctx.senderE164(), ctx.from(), stripSenderPrefix(ctx.from()),
                ctx.to(), stripSenderPrefix(ctx.to()) }) {
            addToken(tokens, v);
        }

        for (String rawEntry : allowTokens) {
            String entry = rawEntry.trim();
            if (entry.isEmpty())
                continue;
            String stripped = stripSenderPrefix(entry);
            if (tokens.contains(entry) || tokens.contains(stripped))
                return true;
            String normalized = normalizeAllowToken(stripped);
            if (!normalized.isEmpty() && tokens.contains(normalized))
                return true;
            String slugged = slugAllowToken(stripped);
            if (!slugged.isEmpty() && tokens.contains(slugged))
                return true;
        }
        return false;
    }

    private static void addToken(Set<String> tokens, String value) {
        if (value == null)
            return;
        String trimmed = value.trim();
        if (trimmed.isEmpty())
            return;
        tokens.add(trimmed);
        String normalized = normalizeAllowToken(trimmed);
        if (!normalized.isEmpty())
            tokens.add(normalized);
        String slugged = slugAllowToken(trimmed);
        if (!slugged.isEmpty())
            tokens.add(slugged);
    }

    /* ── permission resolution ─────────────────────────────── */

    /** Gate failure record. */
    public record GateFailure(String gate, String key) {
    }

    /** Result of elevated permission resolution. */
    public record ElevatedPermissions(boolean enabled, boolean allowed, List<GateFailure> failures) {
    }

    /**
     * Resolve elevated permissions for a given agent + sender context.
     * Simplified — full config resolution deferred to config subsystem.
     */
    public static ElevatedPermissions resolveElevatedPermissions(
            boolean globalEnabled, boolean agentEnabled,
            String provider, SenderContext ctx,
            Map<String, List<String>> globalAllowFrom,
            Map<String, List<String>> agentAllowFrom,
            List<String> fallbackAllowFrom) {

        boolean enabled = globalEnabled && agentEnabled;
        List<GateFailure> failures = new ArrayList<>();

        if (!globalEnabled)
            failures.add(new GateFailure("enabled", "tools.elevated.enabled"));
        if (!agentEnabled)
            failures.add(new GateFailure("enabled", "agents.list[].tools.elevated.enabled"));
        if (!enabled)
            return new ElevatedPermissions(false, false, failures);

        if (provider == null || provider.isBlank()) {
            failures.add(new GateFailure("provider", "ctx.Provider"));
            return new ElevatedPermissions(true, false, failures);
        }

        boolean globalAllowed = isApprovedElevatedSender(provider, ctx, globalAllowFrom, fallbackAllowFrom);
        if (!globalAllowed) {
            failures.add(new GateFailure("allowFrom", "tools.elevated.allowFrom." + provider));
            return new ElevatedPermissions(true, false, failures);
        }

        boolean agentAllowed = agentAllowFrom == null
                || isApprovedElevatedSender(provider, ctx, agentAllowFrom, fallbackAllowFrom);
        if (!agentAllowed) {
            failures.add(new GateFailure("allowFrom", "agents.list[].tools.elevated.allowFrom." + provider));
        }

        return new ElevatedPermissions(true, globalAllowed && agentAllowed, failures);
    }

    /**
     * Format a message explaining why elevated is unavailable.
     */
    public static String formatElevatedUnavailableMessage(
            boolean runtimeSandboxed, List<GateFailure> failures, String sessionKey) {

        List<String> lines = new ArrayList<>();
        lines.add("elevated is not available right now (runtime="
                + (runtimeSandboxed ? "sandboxed" : "direct") + ").");

        if (failures != null && !failures.isEmpty()) {
            String failuresStr = failures.stream()
                    .map(f -> f.gate() + " (" + f.key() + ")")
                    .reduce((a, b) -> a + ", " + b).orElse("");
            lines.add("Failing gates: " + failuresStr);
        } else {
            lines.add("Failing gates: enabled (tools.elevated.enabled / agents.list[].tools.elevated.enabled), "
                    + "allowFrom (tools.elevated.allowFrom.<provider>).");
        }
        lines.add("Fix-it keys:");
        lines.add("- tools.elevated.enabled");
        lines.add("- tools.elevated.allowFrom.<provider>");
        lines.add("- agents.list[].tools.elevated.enabled");
        lines.add("- agents.list[].tools.elevated.allowFrom.<provider>");

        if (sessionKey != null && !sessionKey.isBlank()) {
            lines.add("See: `openclaw sandbox explain --session " + sessionKey + "`");
        }

        return String.join("\n", lines);
    }
}

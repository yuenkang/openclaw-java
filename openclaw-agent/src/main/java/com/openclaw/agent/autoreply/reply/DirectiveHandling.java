package com.openclaw.agent.autoreply.reply;

/**
 * Directive handling facade — re-exports from sub-modules.
 * Mirrors {@code auto-reply/reply/directive-handling.ts}.
 * <p>
 * In Java this is a utility class that provides static access to
 * the individual directive handling modules:
 * <ul>
 * <li>{@link DirectiveHandlingFastLane} — fast-lane directive application</li>
 * <li>{@link DirectiveHandlingParse} — inline directive parsing</li>
 * <li>{@link DirectiveHandlingPersist} — directive persistence</li>
 * <li>{@link DirectiveHandlingShared} — shared formatting</li>
 * <li>{@link DirectiveHandlingAuth} — auth profile resolution</li>
 * <li>{@link DirectiveHandlingQueueValidation} — queue directive
 * validation</li>
 * </ul>
 */
public final class DirectiveHandling {

    private DirectiveHandling() {
    }

    /* Convenience delegates — callers can use the specific classes directly. */

    public static DirectiveHandlingParse.InlineDirectives parseInlineDirectives(
            String body, boolean disableElevated) {
        return DirectiveHandlingParse.parseInlineDirectives(body, disableElevated);
    }

    public static boolean isDirectiveOnly(
            DirectiveHandlingParse.InlineDirectives directives,
            String cleanedBody, boolean isGroup) {
        return DirectiveHandlingParse.isDirectiveOnly(directives, cleanedBody, isGroup);
    }

    public static String formatDirectiveAck(String text) {
        return DirectiveHandlingShared.formatDirectiveAck(text);
    }
}

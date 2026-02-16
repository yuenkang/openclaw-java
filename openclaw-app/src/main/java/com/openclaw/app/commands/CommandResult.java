package com.openclaw.app.commands;

import java.util.List;

/**
 * Result of a command handler execution.
 * Can carry optional inline keyboard buttons for Telegram.
 */
public record CommandResult(
        String text,
        List<List<InlineButton>> buttons) {

    /** Inline keyboard button (text + callback_data). */
    public record InlineButton(String text, String callbackData) {
    }

    /** Create a plain text result with no buttons. */
    public static CommandResult text(String text) {
        return new CommandResult(text, null);
    }

    /** Create a result with text and inline keyboard buttons. */
    public static CommandResult withButtons(String text, List<List<InlineButton>> buttons) {
        return new CommandResult(text, buttons);
    }

    /** Whether this result has inline keyboard buttons. */
    public boolean hasButtons() {
        return buttons != null && !buttons.isEmpty();
    }
}

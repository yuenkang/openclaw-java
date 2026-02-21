package com.openclaw.autoreply;

import java.util.*;

/**
 * Command argument formatting for config, debug, queue commands.
 * Mirrors {@code auto-reply/commands-args.ts}.
 */
public final class CommandsArgs {

    private CommandsArgs() {
    }

    @FunctionalInterface
    public interface CommandArgsFormatter {
        String format(Map<String, Object> values);
    }

    /** Normalize an argument value to its string representation. */
    public static String normalizeArgValue(Object value) {
        if (value == null)
            return null;
        String text;
        if (value instanceof String s) {
            text = s.trim();
        } else if (value instanceof Number || value instanceof Boolean) {
            text = String.valueOf(value).trim();
        } else {
            text = String.valueOf(value);
        }
        return text.isEmpty() ? null : text;
    }

    private static String getStr(Map<String, Object> values, String key) {
        return normalizeArgValue(values.get(key));
    }

    /** Format config command args: show|get|set|unset path[=value]. */
    public static String formatConfigArgs(Map<String, Object> values) {
        String action = getStr(values, "action");
        if (action == null)
            return null;
        action = action.toLowerCase();
        String path = getStr(values, "path");
        String value = getStr(values, "value");

        if ("show".equals(action) || "get".equals(action))
            return path != null ? action + " " + path : action;
        if ("unset".equals(action))
            return path != null ? action + " " + path : action;
        if ("set".equals(action)) {
            if (path == null)
                return action;
            if (value == null)
                return action + " " + path;
            return action + " " + path + "=" + value;
        }
        return action;
    }

    /** Format debug command args: show|reset|set|unset path[=value]. */
    public static String formatDebugArgs(Map<String, Object> values) {
        String action = getStr(values, "action");
        if (action == null)
            return null;
        action = action.toLowerCase();
        String path = getStr(values, "path");
        String value = getStr(values, "value");

        if ("show".equals(action) || "reset".equals(action))
            return action;
        if ("unset".equals(action))
            return path != null ? action + " " + path : action;
        if ("set".equals(action)) {
            if (path == null)
                return action;
            if (value == null)
                return action + " " + path;
            return action + " " + path + "=" + value;
        }
        return action;
    }

    /** Format queue command args: mode debounce:N cap:N drop:N. */
    public static String formatQueueArgs(Map<String, Object> values) {
        String mode = getStr(values, "mode");
        String debounce = getStr(values, "debounce");
        String cap = getStr(values, "cap");
        String drop = getStr(values, "drop");
        List<String> parts = new ArrayList<>();
        if (mode != null)
            parts.add(mode);
        if (debounce != null)
            parts.add("debounce:" + debounce);
        if (cap != null)
            parts.add("cap:" + cap);
        if (drop != null)
            parts.add("drop:" + drop);
        return parts.isEmpty() ? null : String.join(" ", parts);
    }

    /** Registry of command arg formatters. */
    public static final Map<String, CommandArgsFormatter> FORMATTERS = Map.of(
            "config", CommandsArgs::formatConfigArgs,
            "debug", CommandsArgs::formatDebugArgs,
            "queue", CommandsArgs::formatQueueArgs);
}

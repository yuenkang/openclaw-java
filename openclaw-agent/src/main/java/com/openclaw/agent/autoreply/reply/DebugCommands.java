package com.openclaw.agent.autoreply.reply;

/**
 * Parse /debug chat commands (show, reset, set, unset).
 * Mirrors {@code auto-reply/reply/debug-commands.ts}.
 */
public final class DebugCommands {

    private DebugCommands() {
    }

    /** Sealed hierarchy of debug command actions. */
    public sealed interface DebugCommand {
        record Show() implements DebugCommand {
        }

        record Reset() implements DebugCommand {
        }

        record Set(String path, Object value) implements DebugCommand {
        }

        record Unset(String path) implements DebugCommand {
        }

        record Error(String message) implements DebugCommand {
        }
    }

    /**
     * Parse a raw message text as a /debug command.
     *
     * @return parsed command or null if not a /debug command
     */
    public static DebugCommand parseDebugCommand(String raw) {
        if (raw == null)
            return null;
        String trimmed = raw.trim();
        if (!trimmed.toLowerCase().startsWith("/debug"))
            return null;

        String rest = trimmed.substring("/debug".length()).trim();
        if (rest.isEmpty()) {
            return new DebugCommand.Show();
        }

        java.util.regex.Matcher m = java.util.regex.Pattern
                .compile("^(\\S+)(?:\\s+([\\s\\S]+))?$").matcher(rest);
        if (!m.matches()) {
            return new DebugCommand.Error("Invalid /debug syntax.");
        }
        String action = m.group(1).toLowerCase();
        String args = m.group(2) != null ? m.group(2).trim() : "";

        return switch (action) {
            case "show" -> new DebugCommand.Show();
            case "reset" -> new DebugCommand.Reset();
            case "unset" -> {
                if (args.isEmpty())
                    yield new DebugCommand.Error("Usage: /debug unset path");
                yield new DebugCommand.Unset(args);
            }
            case "set" -> {
                if (args.isEmpty())
                    yield new DebugCommand.Error("Usage: /debug set path=value");
                int eqIdx = args.indexOf('=');
                if (eqIdx <= 0)
                    yield new DebugCommand.Error("Usage: /debug set path=value");
                String path = args.substring(0, eqIdx).trim();
                String rawValue = args.substring(eqIdx + 1);
                if (path.isEmpty())
                    yield new DebugCommand.Error("Usage: /debug set path=value");
                Object value = ConfigValue.parseConfigValue(rawValue);
                yield new DebugCommand.Set(path, value);
            }
            default -> new DebugCommand.Error("Usage: /debug show|set|unset|reset");
        };
    }
}

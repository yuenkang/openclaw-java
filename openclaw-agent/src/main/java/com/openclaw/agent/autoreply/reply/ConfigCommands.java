package com.openclaw.agent.autoreply.reply;

/**
 * Parse /config chat commands (show, set, unset).
 * Mirrors {@code auto-reply/reply/config-commands.ts}.
 */
public final class ConfigCommands {

    private ConfigCommands() {
    }

    /** Sealed hierarchy of config command actions. */
    public sealed interface ConfigCommand {
        record Show(String path) implements ConfigCommand {
        }

        record Set(String path, Object value) implements ConfigCommand {
        }

        record Unset(String path) implements ConfigCommand {
        }

        record Error(String message) implements ConfigCommand {
        }
    }

    /**
     * Parse a raw message text as a /config command.
     *
     * @return parsed command or null if not a /config command
     */
    public static ConfigCommand parseConfigCommand(String raw) {
        if (raw == null)
            return null;
        String trimmed = raw.trim();
        if (!trimmed.toLowerCase().startsWith("/config"))
            return null;

        String rest = trimmed.substring("/config".length()).trim();
        if (rest.isEmpty()) {
            return new ConfigCommand.Show(null);
        }

        java.util.regex.Matcher m = java.util.regex.Pattern
                .compile("^(\\S+)(?:\\s+([\\s\\S]+))?$").matcher(rest);
        if (!m.matches()) {
            return new ConfigCommand.Error("Invalid /config syntax.");
        }
        String action = m.group(1).toLowerCase();
        String args = m.group(2) != null ? m.group(2).trim() : "";

        return switch (action) {
            case "show", "get" -> new ConfigCommand.Show(args.isEmpty() ? null : args);
            case "unset" -> {
                if (args.isEmpty())
                    yield new ConfigCommand.Error("Usage: /config unset path");
                yield new ConfigCommand.Unset(args);
            }
            case "set" -> {
                if (args.isEmpty())
                    yield new ConfigCommand.Error("Usage: /config set path=value");
                int eqIdx = args.indexOf('=');
                if (eqIdx <= 0)
                    yield new ConfigCommand.Error("Usage: /config set path=value");
                String path = args.substring(0, eqIdx).trim();
                String rawValue = args.substring(eqIdx + 1);
                if (path.isEmpty())
                    yield new ConfigCommand.Error("Usage: /config set path=value");
                Object value = ConfigValue.parseConfigValue(rawValue);
                yield new ConfigCommand.Set(path, value);
            }
            default -> new ConfigCommand.Error("Usage: /config show|set|unset");
        };
    }
}

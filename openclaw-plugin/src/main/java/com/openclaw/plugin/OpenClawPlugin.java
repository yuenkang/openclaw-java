package com.openclaw.plugin;

/**
 * Plugin interface for OpenClaw extensions.
 * Corresponds to TypeScript's OpenClawPluginDefinition (plugins/types.ts).
 *
 * <p>
 * Plugins are discovered via Java SPI (ServiceLoader) and registered
 * through the {@link OpenClawPluginApi}.
 * </p>
 */
public interface OpenClawPlugin {

    /** Unique plugin identifier. */
    String getId();

    /** Human-readable plugin name. */
    String getName();

    /** Plugin version (SemVer). */
    default String getVersion() {
        return "1.0.0";
    }

    /** Plugin author. */
    default String getAuthor() {
        return "";
    }

    /** Plugin description. */
    default String getDescription() {
        return "";
    }

    /**
     * Called during plugin loading to register tools, hooks, channels, etc.
     */
    void register(OpenClawPluginApi api);

    // --- Plugin API ---

    /**
     * API provided to plugins for registration.
     * Corresponds to TypeScript's PluginRegistry.register* methods.
     */
    interface OpenClawPluginApi {

        void registerTool(String name, Object handler);

        void registerHook(String event, Object handler);

        void registerCommand(String name, Object handler);
    }

    @FunctionalInterface
    interface HookHandler {
        void handle(String event, Object payload);
    }

    interface CommandDefinition {
        String getName();

        String getDescription();

        void execute(CommandContext context);
    }

    interface CommandContext {
        String getSessionKey();

        String getAgentId();

        String getInput();
    }
}

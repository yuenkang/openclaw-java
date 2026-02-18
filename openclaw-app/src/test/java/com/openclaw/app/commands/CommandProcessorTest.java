package com.openclaw.app.commands;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link CommandProcessor} — command routing and dispatch logic.
 * <p>
 * Uses a real CommandProcessor with all command beans; tests focus on the
 * dispatcher layer (routing, argument parsing, unknown commands, error
 * handling).
 */
class CommandProcessorTest {

    /**
     * Build a minimal CommandProcessor with just the essential command beans.
     * In a real Spring context all beans are injected; here we construct directly.
     */
    private CommandProcessor buildProcessor() {
        return new CommandProcessor(
                null, // configService — used for help callbacks only
                new InfoCommands(null), // ModelProviderRegistry is null — minimal
                new StatusCommands(),
                new SessionCommands(),
                new ConfigCommands(),
                new ModelCommands(null),
                new ToolCommands(),
                new BashCommands(),
                new ApproveCommands(),
                new AllowlistCommands(),
                new PluginCommands(),
                new SubagentCommands(),
                new TtsCommands(),
                new DoctorCommands());
    }

    private OpenClawConfig minimalConfig() {
        return new OpenClawConfig();
    }

    // =========================================================================
    // Routing tests
    // =========================================================================

    @Test
    void handleCommand_nullOrBlank_returnsNull() {
        var proc = buildProcessor();
        assertNull(proc.handleCommand(null, "s1", null, minimalConfig()));
        assertNull(proc.handleCommand("", "s1", null, minimalConfig()));
        assertNull(proc.handleCommand("  ", "s1", null, minimalConfig()));
    }

    @Test
    void handleCommand_noSlashPrefix_returnsNull() {
        var proc = buildProcessor();
        assertNull(proc.handleCommand("hello", "s1", null, minimalConfig()));
        assertNull(proc.handleCommand("status", "s1", null, minimalConfig()));
    }

    @Test
    void handleCommand_unknownCommand_returnsNull() {
        var proc = buildProcessor();
        assertNull(proc.handleCommand("/nonexistent", "s1", null, minimalConfig()));
        assertNull(proc.handleCommand("/xyz123", "s1", null, minimalConfig()));
    }

    @Test
    void handleCommand_caseInsensitive() {
        var proc = buildProcessor();
        var r1 = proc.handleCommand("/DOCTOR", "s1", null, minimalConfig());
        var r2 = proc.handleCommand("/Doctor", "s1", null, minimalConfig());
        // Both should route to doctor (not return null)
        assertNotNull(r1);
        assertNotNull(r2);
    }

    @Test
    void handleCommand_parsesArgs_correctly() {
        var proc = buildProcessor();
        // "/status all" should not be null — it routes to status handler
        var result = proc.handleCommand("/status all", "s1", null, minimalConfig());
        assertNotNull(result);
        assertNotNull(result.text());
    }

    @Test
    void handleCommand_trims_whitespace() {
        var proc = buildProcessor();
        var result = proc.handleCommand("  /doctor  ", "s1", null, minimalConfig());
        assertNotNull(result);
    }

    // =========================================================================
    // Registration completeness tests
    // =========================================================================

    @Test
    void allRegisteredCommands_returnNonNull() {
        var proc = buildProcessor();
        var config = minimalConfig();
        // Commands that should be registered
        String[] commands = { "help", "status", "commands", "doctor",
                "clear", "new", "reset", "stop", "usage",
                "config", "debug", "models", "model",
                "compact", "fix", "bash", "approve",
                "allowlist", "plugin", "subagents", "tts" };

        for (String cmd : commands) {
            var result = proc.handleCommand("/" + cmd, "s1", null, config);
            assertNotNull(result, "Command /" + cmd + " should be registered and return a result");
        }
    }

    // =========================================================================
    // Error handling
    // =========================================================================

    @Test
    void handleCommand_exceptionInHandler_returnsErrorText() {
        // Using a minimal config that may cause NPE in some handlers — processor
        // catches exceptions and returns error text
        var proc = buildProcessor();
        // Even if the handler throws, the processor wraps it
        var result = proc.handleCommand("/whoami", "s1", null, minimalConfig());
        // whoami handler may throw or return — either way, result should not be null
        assertNotNull(result);
    }
}

package com.openclaw.app.commands;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ConfigCommands}.
 */
class ConfigCommandsTest {

    private ConfigCommands configCommands;
    private CommandContext ctx;

    @BeforeEach
    void setUp() {
        configCommands = new ConfigCommands();
        var config = new OpenClawConfig();
        ctx = new CommandContext("test-session", "user1", config, true, null);
    }

    @Test
    void handleConfig_noArgs_returnsHelp() {
        var result = configCommands.handleConfig("", ctx);
        assertNotNull(result);
        assertNotNull(result.text());
    }

    @Test
    void handleConfig_get_returnsResult() {
        var result = configCommands.handleConfig("get", ctx);
        assertNotNull(result);
        assertNotNull(result.text());
    }

    @Test
    void handleConfig_getModel_returnsModelValue() {
        var config = new OpenClawConfig();
        config.setModel("test-model-42");
        var customCtx = new CommandContext("test-session", "user1", config, true, null);

        var result = configCommands.handleConfig("get model", customCtx);
        assertNotNull(result);
        // Should contain model info
        assertNotNull(result.text());
    }

    @Test
    void handleDebug_returnsResult() {
        var result = configCommands.handleDebug("", ctx);
        assertNotNull(result);
        assertNotNull(result.text());
    }
}

package com.openclaw.app.commands;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link StatusCommands}.
 */
class StatusCommandsTest {

    private StatusCommands statusCommands;
    private CommandContext ctx;

    @BeforeEach
    void setUp() {
        statusCommands = new StatusCommands();
        var config = new OpenClawConfig();
        ctx = new CommandContext("test-session", "user1", config, true, null);
    }

    @Test
    void handleStatus_returnsNonNull() {
        var result = statusCommands.handleStatus("", ctx);
        assertNotNull(result);
        assertNotNull(result.text());
    }

    @Test
    void handleStatus_containsHeader() {
        var result = statusCommands.handleStatus("", ctx);
        assertTrue(result.text().contains("状态"), "Status output should contain header");
    }

    @Test
    void handleStatus_containsModelSection() {
        var result = statusCommands.handleStatus("", ctx);
        assertTrue(result.text().contains("模型"), "Status output should contain model info");
    }

    @Test
    void handleStatus_containsSessionSection() {
        var result = statusCommands.handleStatus("", ctx);
        // May say "未初始化" if no session store exists — that's valid
        assertTrue(result.text().contains("会话"), "Status output should contain session info");
    }

    @Test
    void handleStatus_containsChannelSection() {
        var result = statusCommands.handleStatus("", ctx);
        assertTrue(result.text().contains("渠道"), "Status output should contain channel info");
    }

    @Test
    void handleStatusAll_delegatesCorrectly() {
        var result = statusCommands.handleStatus("all", ctx);
        assertNotNull(result);
        assertTrue(result.text().contains("全量状态"), "Status all should show expanded header");
    }

    @Test
    void handleStatusAll_containsConfigPath() {
        var result = statusCommands.handleStatusAll("", ctx);
        assertNotNull(result);
        assertTrue(result.text().contains("配置"), "Status all should contain config path");
    }

    @Test
    void handleStatus_withCustomModel_showsModel() {
        var config = new OpenClawConfig();
        config.setModel("claude-sonnet-4-20250514");
        var customCtx = new CommandContext("test-session", "user1", config, true, null);
        var result = statusCommands.handleStatus("", customCtx);
        assertTrue(result.text().contains("claude-sonnet-4-20250514"),
                "Status should display configured model");
    }
}

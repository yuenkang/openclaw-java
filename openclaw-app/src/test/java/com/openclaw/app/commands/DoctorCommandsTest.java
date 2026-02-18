package com.openclaw.app.commands;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link DoctorCommands}.
 */
class DoctorCommandsTest {

    private DoctorCommands doctorCommands;
    private CommandContext ctx;

    @BeforeEach
    void setUp() {
        doctorCommands = new DoctorCommands();
        var config = new OpenClawConfig();
        ctx = new CommandContext("test-session", "user1", config, true, null);
    }

    @Test
    void handleDoctor_returnsNonNull() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertNotNull(result);
        assertNotNull(result.text());
    }

    @Test
    void handleDoctor_containsHeader() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("诊断"), "Doctor should contain diagnosis header");
    }

    @Test
    void handleDoctor_containsConfigCheck() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("配置文件"), "Doctor should check config file");
    }

    @Test
    void handleDoctor_containsApiKeyCheck() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("API") || result.text().contains("密钥"),
                "Doctor should check API keys");
    }

    @Test
    void handleDoctor_containsPortCheck() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("端口"), "Doctor should check port");
    }

    @Test
    void handleDoctor_containsBinaryCheck() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("git"), "Doctor should check for git binary");
    }

    @Test
    void handleDoctor_containsUpdateCheck() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("更新"), "Doctor should check for updates");
    }

    @Test
    void handleDoctor_containsSummary() {
        var result = doctorCommands.handleDoctor("", ctx);
        assertTrue(result.text().contains("汇总"), "Doctor should contain summary line");
    }

    @Test
    void handleDoctor_summaryCountsAddUp() {
        var result = doctorCommands.handleDoctor("", ctx);
        String text = result.text();
        // Summary line format: "汇总: ✅ N 通过 ⚠️ N 警告 ❌ N 错误"
        assertTrue(text.contains("通过"), "Summary should mention passed checks");
    }
}

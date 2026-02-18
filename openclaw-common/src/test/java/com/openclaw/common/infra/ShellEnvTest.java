package com.openclaw.common.infra;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class ShellEnvTest {

    @AfterEach
    void tearDown() {
        ShellEnv.resetForTest();
    }

    @Test
    void parseShellEnv_nullDelimited() {
        String raw = "FOO=bar\0BAZ=qux\0EMPTY=\0";
        Map<String, String> result = ShellEnv.parseShellEnv(raw.getBytes(StandardCharsets.UTF_8));

        assertEquals("bar", result.get("FOO"));
        assertEquals("qux", result.get("BAZ"));
        assertEquals("", result.get("EMPTY"));
    }

    @Test
    void parseShellEnv_emptyInput() {
        Map<String, String> result = ShellEnv.parseShellEnv(new byte[0]);
        assertTrue(result.isEmpty());
    }

    @Test
    void loadShellEnvFallback_skipsWhenDisabled() {
        Map<String, String> target = new HashMap<>();
        ShellEnv.ShellEnvResult result = ShellEnv.loadShellEnvFallback(
                false, target, List.of("OPENAI_API_KEY"), null);

        assertTrue(result.ok());
        assertEquals("disabled", result.skippedReason());
    }

    @Test
    void loadShellEnvFallback_skipsWhenKeyExists() {
        Map<String, String> target = new HashMap<>();
        target.put("OPENAI_API_KEY", "sk-test123");

        ShellEnv.ShellEnvResult result = ShellEnv.loadShellEnvFallback(
                true, target, List.of("OPENAI_API_KEY"), null);

        assertTrue(result.ok());
        assertEquals("already-has-keys", result.skippedReason());
    }

    @Test
    void resolveShell_defaultsToSh() {
        // resolveShell should return something meaningful
        String shell = ShellEnv.resolveShell();
        assertNotNull(shell);
        assertFalse(shell.isBlank());
    }

    @Test
    void getAppliedKeys_emptyByDefault() {
        assertTrue(ShellEnv.getAppliedKeys().isEmpty());
    }
}

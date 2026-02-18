package com.openclaw.agent.extensions;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ContextPruner#pruneContextMessages} — the main public
 * API.
 */
class ContextPrunerTest {

    private Map<String, Object> msg(String role, String content) {
        var m = new LinkedHashMap<String, Object>();
        m.put("role", role);
        m.put("content", content);
        return m;
    }

    private ContextPruningSettings.Effective defaultSettings() {
        return ContextPruningSettings.Effective.builder()
                .mode(ContextPruningSettings.Mode.CACHE_TTL)
                .ttlMs(5 * 60_000L)
                .keepLastAssistants(3)
                .softTrimRatio(0.3)
                .hardClearRatio(0.5)
                .minPrunableToolChars(50_000)
                .tools(new ContextPruningSettings.ToolMatch())
                .softTrim(ContextPruningSettings.SoftTrim.builder()
                        .maxChars(4000).headChars(1500).tailChars(1500).build())
                .hardClear(ContextPruningSettings.HardClear.builder()
                        .enabled(true).placeholder("[cleared]").build())
                .build();
    }

    // =========================================================================
    // pruneContextMessages — no pruning needed
    // =========================================================================

    @Test
    void pruneContextMessages_smallMessagesNoPruning() {
        var messages = new ArrayList<>(List.<Map<String, Object>>of(
                msg("system", "system prompt"),
                msg("user", "Hello"),
                msg("assistant", "Hi there!")));

        var result = ContextPruner.pruneContextMessages(
                messages, defaultSettings(), null, 128000, s -> true);
        // Tiny messages, 128k window → no pruning
        assertSame(messages, result);
    }

    @Test
    void pruneContextMessages_singleMessage() {
        var messages = new ArrayList<>(List.<Map<String, Object>>of(
                msg("user", "Hello")));

        var result = ContextPruner.pruneContextMessages(
                messages, defaultSettings(), null, 128000, s -> true);
        // Single user message, large window → no pruning
        assertSame(messages, result);
    }

    @Test
    void pruneContextMessages_withToolResults_noPruning() {
        var toolResult = new LinkedHashMap<String, Object>();
        toolResult.put("role", "tool");
        toolResult.put("tool_call_id", "call_1");
        toolResult.put("content", "short result");

        var messages = new ArrayList<>(List.<Map<String, Object>>of(
                msg("system", "sys"),
                msg("user", "run tool"),
                msg("assistant", "ok"),
                toolResult));

        var result = ContextPruner.pruneContextMessages(
                messages, defaultSettings(), null, 128000, s -> true);
        // Small tool result + large window → no pruning
        assertSame(messages, result);
    }

    @Test
    void pruneContextMessages_emptyMessages() {
        var messages = new ArrayList<Map<String, Object>>();
        var result = ContextPruner.pruneContextMessages(
                messages, defaultSettings(), null, 128000, s -> true);
        assertSame(messages, result);
    }

    // =========================================================================
    // ContextPruningSettings.DEFAULTS
    // =========================================================================

    @Test
    void defaults_haveSaneValues() {
        var d = ContextPruningSettings.DEFAULTS;
        assertEquals(ContextPruningSettings.Mode.CACHE_TTL, d.getMode());
        assertTrue(d.getTtlMs() > 0);
        assertTrue(d.getKeepLastAssistants() > 0);
        assertTrue(d.getSoftTrimRatio() > 0 && d.getSoftTrimRatio() < 1);
        assertTrue(d.getHardClearRatio() > 0 && d.getHardClearRatio() < 1);
    }

    @Test
    void defaults_softTrim_haveSaneValues() {
        var st = ContextPruningSettings.DEFAULTS.getSoftTrim();
        assertTrue(st.getMaxChars() > 0);
        assertTrue(st.getHeadChars() > 0);
        assertTrue(st.getTailChars() > 0);
    }

    // =========================================================================
    // computeEffective
    // =========================================================================

    @Test
    void computeEffective_cacheTtlMode() {
        var raw = Map.of("mode", "cache-ttl");
        var result = ContextPruningSettings.computeEffective(raw);
        assertNotNull(result);
        assertEquals(ContextPruningSettings.Mode.CACHE_TTL, result.getMode());
    }

    @Test
    void computeEffective_unknownModeReturnsNull() {
        var raw = Map.of("mode", "unknown");
        assertNull(ContextPruningSettings.computeEffective(raw));
    }

    @Test
    void computeEffective_nonMapReturnsNull() {
        assertNull(ContextPruningSettings.computeEffective("not-a-map"));
    }
}

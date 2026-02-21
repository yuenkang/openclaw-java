package com.openclaw.agent.runtime;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ContextWindowGuard}.
 */
class ContextWindowGuardTest {

    // ── resolveContextWindow ──────────────────────────────────────

    @Nested
    class ResolveContextWindow {

        @Test
        void fallsBackToDefault() {
            var info = ContextWindowGuard.resolveContextWindow(
                    null, "anthropic", "claude-3-sonnet", 0, 128000);
            assertEquals(128000, info.tokens());
            assertEquals("default", info.source());
        }

        @Test
        void usesModelReportedValue() {
            var info = ContextWindowGuard.resolveContextWindow(
                    null, "anthropic", "claude-3-sonnet", 200000, 128000);
            assertEquals(200000, info.tokens());
            assertEquals("model", info.source());
        }

        @Test
        void defaultAtLeastOne() {
            var info = ContextWindowGuard.resolveContextWindow(
                    null, "p", "m", 0, 0);
            assertEquals(1, info.tokens());
        }

        @Test
        void nullConfigUsesModelOrDefault() {
            var info = ContextWindowGuard.resolveContextWindow(
                    new OpenClawConfig(), "p", "m", 50000, 10000);
            assertEquals(50000, info.tokens());
            assertEquals("model", info.source());
        }
    }

    // ── evaluate ──────────────────────────────────────────────────

    @Nested
    class Evaluate {

        @Test
        void largeWindow_noWarningNoBlock() {
            var info = new ContextWindowGuard.ContextWindowInfo(128000, "model");
            var result = ContextWindowGuard.evaluate(info);
            assertFalse(result.shouldWarn());
            assertFalse(result.shouldBlock());
            assertEquals(128000, result.tokens());
            assertEquals("model", result.source());
        }

        @Test
        void smallWindow_warnOnly() {
            var info = new ContextWindowGuard.ContextWindowInfo(20000, "model");
            var result = ContextWindowGuard.evaluate(info);
            assertTrue(result.shouldWarn()); // < 32000
            assertFalse(result.shouldBlock()); // > 16000
        }

        @Test
        void tinyWindow_warnAndBlock() {
            var info = new ContextWindowGuard.ContextWindowInfo(10000, "model");
            var result = ContextWindowGuard.evaluate(info);
            assertTrue(result.shouldWarn());
            assertTrue(result.shouldBlock());
        }

        @Test
        void customThresholds() {
            var info = new ContextWindowGuard.ContextWindowInfo(5000, "model");
            var result = ContextWindowGuard.evaluate(info, 10000, 3000);
            assertTrue(result.shouldWarn()); // 5000 < 10000
            assertFalse(result.shouldBlock()); // 5000 > 3000
        }

        @Test
        void atExactWarnThreshold_noWarn() {
            var info = new ContextWindowGuard.ContextWindowInfo(32000, "model");
            var result = ContextWindowGuard.evaluate(info);
            assertFalse(result.shouldWarn());
        }

        @Test
        void atExactBlockThreshold_noBlock() {
            var info = new ContextWindowGuard.ContextWindowInfo(16000, "model");
            var result = ContextWindowGuard.evaluate(info);
            assertFalse(result.shouldBlock());
        }
    }

    // ── Constants ─────────────────────────────────────────────────

    @Test
    void defaultConstants() {
        assertEquals(16_000, ContextWindowGuard.HARD_MIN_TOKENS);
        assertEquals(32_000, ContextWindowGuard.WARN_BELOW_TOKENS);
    }

    // ── Records ───────────────────────────────────────────────────

    @Test
    void contextWindowInfoRecord() {
        var info = new ContextWindowGuard.ContextWindowInfo(50000, "custom");
        assertEquals(50000, info.tokens());
        assertEquals("custom", info.source());
    }

    @Test
    void guardResultRecord() {
        var result = new ContextWindowGuard.GuardResult(50000, "model", false, false);
        assertEquals(50000, result.tokens());
        assertEquals("model", result.source());
        assertFalse(result.shouldWarn());
        assertFalse(result.shouldBlock());
    }
}

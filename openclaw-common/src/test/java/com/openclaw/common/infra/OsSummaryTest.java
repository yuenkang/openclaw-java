package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class OsSummaryTest {

    @Test
    void resolve_returnsNonNullFields() {
        var summary = OsSummary.resolve();
        assertNotNull(summary.getPlatform());
        assertNotNull(summary.getArch());
        assertNotNull(summary.getRelease());
        assertNotNull(summary.getLabel());
        assertFalse(summary.getLabel().isEmpty());
    }

    @Test
    void resolve_platformIsDarwinOnMac() {
        var summary = OsSummary.resolve();
        String osName = System.getProperty("os.name", "").toLowerCase();
        if (osName.contains("mac")) {
            assertEquals("darwin", summary.getPlatform());
            assertTrue(summary.getLabel().startsWith("macos"));
        }
    }
}

package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class FormatAgeTest {

    @Test
    void negative_returnsUnknown() {
        assertEquals("unknown", FormatAge.formatAge(-1));
    }

    @Test
    void lessThanOneMinute_returnsJustNow() {
        assertEquals("just now", FormatAge.formatAge(20_000));
        assertEquals("just now", FormatAge.formatAge(0));
    }

    @Test
    void minutes() {
        assertEquals("5m ago", FormatAge.formatAge(5 * 60_000));
        assertEquals("59m ago", FormatAge.formatAge(59 * 60_000));
    }

    @Test
    void hours() {
        assertEquals("1h ago", FormatAge.formatAge(60 * 60_000));
        assertEquals("24h ago", FormatAge.formatAge(24 * 60 * 60_000));
    }

    @Test
    void days() {
        assertEquals("3d ago", FormatAge.formatAge(3L * 24 * 60 * 60_000));
    }

    @Test
    void durationCompact_ms() {
        assertEquals("500ms", FormatAge.formatDurationCompact(500));
    }

    @Test
    void durationCompact_seconds() {
        assertEquals("1.5s", FormatAge.formatDurationCompact(1500));
    }

    @Test
    void durationCompact_minutes() {
        assertEquals("2m 30s", FormatAge.formatDurationCompact(150_000));
    }

    @Test
    void durationCompact_hours() {
        assertEquals("1h 30m", FormatAge.formatDurationCompact(5_400_000));
    }
}

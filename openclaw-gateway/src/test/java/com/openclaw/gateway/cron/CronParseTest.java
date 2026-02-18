package com.openclaw.gateway.cron;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link CronParse}.
 */
class CronParseTest {

    // =========================================================================
    // normalizeUtcIso
    // =========================================================================

    @Test
    void normalizeUtcIso_alreadyHasTimezone_returnsAsIs() {
        assertEquals("2024-01-01T12:00:00Z", CronParse.normalizeUtcIso("2024-01-01T12:00:00Z"));
        assertEquals("2024-01-01T12:00:00+08:00", CronParse.normalizeUtcIso("2024-01-01T12:00:00+08:00"));
    }

    @Test
    void normalizeUtcIso_dateOnly_appendsT000000Z() {
        assertEquals("2024-01-01T00:00:00Z", CronParse.normalizeUtcIso("2024-01-01"));
    }

    @Test
    void normalizeUtcIso_dateTimeNoTZ_appendsZ() {
        assertEquals("2024-01-01T12:30:00Z", CronParse.normalizeUtcIso("2024-01-01T12:30:00"));
    }

    // =========================================================================
    // parseAbsoluteTimeMs
    // =========================================================================

    @ParameterizedTest
    @NullAndEmptySource
    void parseAbsoluteTimeMs_nullOrEmpty_returnsNull(String input) {
        assertNull(CronParse.parseAbsoluteTimeMs(input));
    }

    @Test
    void parseAbsoluteTimeMs_numericEpochMs() {
        Long result = CronParse.parseAbsoluteTimeMs("1704067200000");
        assertNotNull(result);
        assertEquals(1704067200000L, result);
    }

    @Test
    void parseAbsoluteTimeMs_zeroReturnsNull() {
        assertNull(CronParse.parseAbsoluteTimeMs("0"));
    }

    @Test
    void parseAbsoluteTimeMs_isoDate() {
        Long result = CronParse.parseAbsoluteTimeMs("2024-01-01");
        assertNotNull(result);
        // 2024-01-01T00:00:00Z = 1704067200000
        assertEquals(1704067200000L, result);
    }

    @Test
    void parseAbsoluteTimeMs_isoDateTime() {
        Long result = CronParse.parseAbsoluteTimeMs("2024-01-01T12:00:00Z");
        assertNotNull(result);
        assertEquals(1704110400000L, result);
    }

    @Test
    void parseAbsoluteTimeMs_isoDateTimeNoTZ() {
        Long result = CronParse.parseAbsoluteTimeMs("2024-01-01T12:00:00");
        assertNotNull(result);
        assertEquals(1704110400000L, result);
    }

    @ParameterizedTest
    @CsvSource({ "garbage", "not-a-date", "2024-13-01" })
    void parseAbsoluteTimeMs_invalidInput_returnsNull(String input) {
        assertNull(CronParse.parseAbsoluteTimeMs(input));
    }
}

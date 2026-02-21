package com.openclaw.hooks;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class SoulEvilTest {

    @Nested
    class ConfigResolution {
        @Test
        void nullEntry_returnsNull() {
            assertNull(SoulEvil.resolveSoulEvilConfig(null));
        }

        @Test
        void emptyEntry_returnsNull() {
            assertNull(SoulEvil.resolveSoulEvilConfig(Map.of()));
        }

        @Test
        void validConfig_allFields() {
            var config = SoulEvil.resolveSoulEvilConfig(Map.of(
                    "file", "MY_EVIL.md",
                    "chance", 0.5,
                    "purge", Map.of("at", "23:00", "duration", "30m")));

            assertNotNull(config);
            assertEquals("MY_EVIL.md", config.getFile());
            assertEquals(0.5, config.getChance());
            assertEquals("23:00", config.getPurgeAt());
            assertEquals("30m", config.getPurgeDuration());
        }

        @Test
        void chanceOnly() {
            var config = SoulEvil.resolveSoulEvilConfig(Map.of("chance", 0.1));
            assertNotNull(config);
            assertEquals(0.1, config.getChance());
            assertNull(config.getFile());
        }
    }

    @Nested
    class Decision {
        @Test
        void nullConfig_noEvil() {
            var decision = SoulEvil.decideSoulEvil(null);
            assertFalse(decision.isUseEvil());
            assertEquals(SoulEvil.DEFAULT_SOUL_EVIL_FILENAME, decision.getFileName());
        }

        @Test
        void zeroChance_noEvil() {
            var config = SoulEvil.SoulEvilConfig.builder().chance(0.0).build();
            var decision = SoulEvil.decideSoulEvil(config, null, null, () -> 0.5);
            assertFalse(decision.isUseEvil());
        }

        @Test
        void highChance_alwaysEvil() {
            var config = SoulEvil.SoulEvilConfig.builder().chance(1.0).build();
            var decision = SoulEvil.decideSoulEvil(config, null, null, () -> 0.5);
            assertTrue(decision.isUseEvil());
            assertEquals("chance", decision.getReason());
        }

        @Test
        void chance_respectedByRng() {
            var config = SoulEvil.SoulEvilConfig.builder().chance(0.3).build();

            // roll 0.2 < 0.3 → evil
            var d1 = SoulEvil.decideSoulEvil(config, null, null, () -> 0.2);
            assertTrue(d1.isUseEvil());

            // roll 0.5 >= 0.3 → no evil
            var d2 = SoulEvil.decideSoulEvil(config, null, null, () -> 0.5);
            assertFalse(d2.isUseEvil());
        }

        @Test
        void customFileName() {
            var config = SoulEvil.SoulEvilConfig.builder()
                    .file("DARK_SOUL.md").chance(1.0).build();
            var decision = SoulEvil.decideSoulEvil(config, null, null, () -> 0.0);
            assertEquals("DARK_SOUL.md", decision.getFileName());
        }
    }

    @Nested
    class PurgeWindow {
        @Test
        void withinWindow_triggersEvil() {
            // Window: 14:00 to 14:30, current time: 14:15
            var now = ZonedDateTime.of(2026, 2, 17, 14, 15, 0, 0,
                    ZoneId.of("Asia/Shanghai")).toInstant();
            var config = SoulEvil.SoulEvilConfig.builder()
                    .purgeAt("14:00").purgeDuration("30m").build();

            var decision = SoulEvil.decideSoulEvil(config, "Asia/Shanghai", now, () -> 1.0);
            assertTrue(decision.isUseEvil());
            assertEquals("purge", decision.getReason());
        }

        @Test
        void outsideWindow_noEvil() {
            // Window: 14:00 to 14:30, current time: 15:00
            var now = ZonedDateTime.of(2026, 2, 17, 15, 0, 0, 0,
                    ZoneId.of("Asia/Shanghai")).toInstant();
            var config = SoulEvil.SoulEvilConfig.builder()
                    .purgeAt("14:00").purgeDuration("30m").build();

            var decision = SoulEvil.decideSoulEvil(config, "Asia/Shanghai", now, () -> 1.0);
            assertFalse(decision.isUseEvil());
        }

        @Test
        void midnightWrap_works() {
            // Window: 23:30 to 00:30 (wraps midnight), current: 00:15
            var now = ZonedDateTime.of(2026, 2, 18, 0, 15, 0, 0,
                    ZoneId.of("Asia/Shanghai")).toInstant();
            var config = SoulEvil.SoulEvilConfig.builder()
                    .purgeAt("23:30").purgeDuration("1h").build();

            var decision = SoulEvil.decideSoulEvil(config, "Asia/Shanghai", now, () -> 1.0);
            assertTrue(decision.isUseEvil());
            assertEquals("purge", decision.getReason());
        }
    }

    @Nested
    class TimeHelpers {
        @Test
        void parsePurgeAt_valid() {
            assertEquals(14 * 60, SoulEvil.parsePurgeAt("14:00"));
            assertEquals(0, SoulEvil.parsePurgeAt("00:00"));
            assertEquals(23 * 60 + 59, SoulEvil.parsePurgeAt("23:59"));
        }

        @Test
        void parsePurgeAt_invalid() {
            assertEquals(-1, SoulEvil.parsePurgeAt(null));
            assertEquals(-1, SoulEvil.parsePurgeAt(""));
            assertEquals(-1, SoulEvil.parsePurgeAt("25:00"));
            assertEquals(-1, SoulEvil.parsePurgeAt("abc"));
        }

        @Test
        void parseDurationMs_variousUnits() {
            assertEquals(500, SoulEvil.parseDurationMs("500ms"));
            assertEquals(30_000, SoulEvil.parseDurationMs("30s"));
            assertEquals(600_000, SoulEvil.parseDurationMs("10m"));
            assertEquals(3_600_000, SoulEvil.parseDurationMs("1h"));
            assertEquals(86_400_000, SoulEvil.parseDurationMs("1d"));
        }

        @Test
        void parseDurationMs_defaultMinutes() {
            assertEquals(300_000, SoulEvil.parseDurationMs("5")); // 5 minutes
        }

        @Test
        void parseDurationMs_invalid() {
            assertEquals(0, SoulEvil.parseDurationMs(null));
            assertEquals(0, SoulEvil.parseDurationMs(""));
            assertEquals(0, SoulEvil.parseDurationMs("abc"));
        }

        @Test
        void clampChance() {
            assertEquals(0.0, SoulEvil.clampChance(null));
            assertEquals(0.5, SoulEvil.clampChance(0.5));
            assertEquals(0.0, SoulEvil.clampChance(-1.0));
            assertEquals(1.0, SoulEvil.clampChance(2.0));
        }
    }
}

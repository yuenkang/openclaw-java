package com.openclaw.agent.hooks;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.*;
import java.util.List;
import java.util.Map;
import java.util.function.DoubleSupplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Soul Evil: personality override mechanism.
 * <p>
 * When configured, can replace the agent's SOUL.md with SOUL_EVIL.md
 * based on probability or a daily time window schedule.
 * <p>
 * Port of: hooks/soul-evil.ts
 */
@Slf4j
public class SoulEvil {

    public static final String DEFAULT_SOUL_EVIL_FILENAME = "SOUL_EVIL.md";
    private static final Pattern TIME_PATTERN = Pattern.compile("^([01]?\\d|2[0-3]):([0-5]\\d)$");
    private static final Pattern DURATION_PATTERN = Pattern.compile("^(\\d+)\\s*(ms|s|m|h|d)?$",
            Pattern.CASE_INSENSITIVE);

    // =========================================================================
    // Config
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SoulEvilConfig {
        /** Alternate SOUL file name (default: SOUL_EVIL.md). */
        private String file;
        /** Random chance (0-1) to use SOUL_EVIL on any message. */
        private Double chance;
        /** Daily purge window start time in HH:mm 24h format. */
        private String purgeAt;
        /** Duration of purge window (e.g. "30s", "10m", "1h"). */
        private String purgeDuration;
    }

    // =========================================================================
    // Decision
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SoulEvilDecision {
        private boolean useEvil;
        /** null | "purge" | "chance" */
        private String reason;
        private String fileName;
    }

    // =========================================================================
    // Bootstrap file (simplified)
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BootstrapFile {
        private String name;
        private String content;
        private boolean missing;
    }

    // =========================================================================
    // Config resolution from hook entry
    // =========================================================================

    /**
     * Resolve SoulEvilConfig from a hook configuration entry.
     */
    public static SoulEvilConfig resolveSoulEvilConfig(Map<String, Object> entry) {
        if (entry == null || entry.isEmpty())
            return null;

        String file = entry.get("file") instanceof String s ? s.trim() : null;
        Double chance = null;
        Object chanceObj = entry.get("chance");
        if (chanceObj instanceof Number n) {
            double d = n.doubleValue();
            if (Double.isFinite(d))
                chance = d;
        }

        String purgeAt = null;
        String purgeDuration = null;
        Object purgeObj = entry.get("purge");
        if (purgeObj instanceof Map<?, ?> purgeMap) {
            Object at = purgeMap.get("at");
            if (at instanceof String s)
                purgeAt = s.trim();
            Object dur = purgeMap.get("duration");
            if (dur instanceof String s)
                purgeDuration = s.trim();
        }

        if (file == null && chance == null && purgeAt == null && purgeDuration == null) {
            return null;
        }

        return SoulEvilConfig.builder()
                .file(file)
                .chance(chance)
                .purgeAt(purgeAt)
                .purgeDuration(purgeDuration)
                .build();
    }

    // =========================================================================
    // Decision logic
    // =========================================================================

    /**
     * Decide whether to use SOUL_EVIL.md based on config, timezone, and randomness.
     */
    public static SoulEvilDecision decideSoulEvil(SoulEvilConfig config,
            String userTimezone,
            Instant now,
            DoubleSupplier random) {
        String fileName = (config != null && config.getFile() != null && !config.getFile().isBlank())
                ? config.getFile().trim()
                : DEFAULT_SOUL_EVIL_FILENAME;

        if (config == null) {
            return SoulEvilDecision.builder().useEvil(false).fileName(fileName).build();
        }

        Instant effectiveNow = now != null ? now : Instant.now();
        String tz = resolveTimezone(userTimezone);

        // Check purge window
        if (isWithinDailyPurgeWindow(config.getPurgeAt(), config.getPurgeDuration(),
                effectiveNow, tz)) {
            return SoulEvilDecision.builder().useEvil(true).reason("purge").fileName(fileName).build();
        }

        // Check chance
        double chance = clampChance(config.getChance());
        if (chance > 0) {
            double roll = (random != null ? random : (DoubleSupplier) Math::random).getAsDouble();
            if (roll < chance) {
                return SoulEvilDecision.builder().useEvil(true).reason("chance").fileName(fileName).build();
            }
        }

        return SoulEvilDecision.builder().useEvil(false).fileName(fileName).build();
    }

    /** Convenience overload with defaults. */
    public static SoulEvilDecision decideSoulEvil(SoulEvilConfig config) {
        return decideSoulEvil(config, null, null, null);
    }

    // =========================================================================
    // Bootstrap override
    // =========================================================================

    /**
     * Apply soul evil override to workspace bootstrap files.
     * If decision is to use evil, replaces SOUL.md content with SOUL_EVIL.md.
     */
    public static List<BootstrapFile> applySoulEvilOverride(
            List<BootstrapFile> files, Path workspaceDir,
            SoulEvilConfig config, String userTimezone,
            Instant now, DoubleSupplier random) {

        SoulEvilDecision decision = decideSoulEvil(config, userTimezone, now, random);
        if (!decision.isUseEvil()) {
            return files;
        }

        Path evilPath = workspaceDir.resolve(decision.getFileName());
        String evilContent;
        try {
            evilContent = Files.readString(evilPath, StandardCharsets.UTF_8);
        } catch (IOException e) {
            log.warn("SOUL_EVIL active ({}) but file missing: {}",
                    decision.getReason(), evilPath);
            return files;
        }

        if (evilContent.isBlank()) {
            log.warn("SOUL_EVIL active ({}) but file empty: {}",
                    decision.getReason(), evilPath);
            return files;
        }

        boolean hasSoul = files.stream().anyMatch(f -> "SOUL.md".equals(f.getName()));
        if (!hasSoul) {
            log.warn("SOUL_EVIL active ({}) but SOUL.md not in bootstrap files",
                    decision.getReason());
            return files;
        }

        log.debug("SOUL_EVIL active ({}) using {}", decision.getReason(), decision.getFileName());

        return files.stream()
                .map(f -> {
                    if ("SOUL.md".equals(f.getName())) {
                        return BootstrapFile.builder()
                                .name(f.getName())
                                .content(evilContent)
                                .missing(false)
                                .build();
                    }
                    return f;
                })
                .toList();
    }

    // =========================================================================
    // Time helpers
    // =========================================================================

    /**
     * Check if the current time is within the daily purge window.
     */
    static boolean isWithinDailyPurgeWindow(String purgeAt, String purgeDuration,
            Instant now, String timeZone) {
        if (purgeAt == null || purgeAt.isBlank() || purgeDuration == null || purgeDuration.isBlank()) {
            return false;
        }

        int startMinutes = parsePurgeAt(purgeAt);
        if (startMinutes < 0)
            return false;

        long durationMs = parseDurationMs(purgeDuration);
        if (durationMs <= 0)
            return false;

        long dayMs = 24L * 60 * 60 * 1000;
        if (durationMs >= dayMs)
            return true;

        long nowMs = timeOfDayMs(now, timeZone);
        if (nowMs < 0)
            return false;

        long startMs = (long) startMinutes * 60 * 1000;
        long endMs = startMs + durationMs;

        if (endMs < dayMs) {
            return nowMs >= startMs && nowMs < endMs;
        }
        // Wraps past midnight
        long wrappedEnd = endMs % dayMs;
        return nowMs >= startMs || nowMs < wrappedEnd;
    }

    /**
     * Parse "HH:mm" to minutes-since-midnight. Returns -1 on failure.
     */
    static int parsePurgeAt(String raw) {
        if (raw == null)
            return -1;
        Matcher m = TIME_PATTERN.matcher(raw.trim());
        if (!m.matches())
            return -1;
        int hour = Integer.parseInt(m.group(1));
        int minute = Integer.parseInt(m.group(2));
        return hour * 60 + minute;
    }

    /**
     * Parse a duration string (e.g. "30s", "10m", "1h") to milliseconds.
     * Default unit is minutes if no suffix given.
     */
    static long parseDurationMs(String raw) {
        if (raw == null || raw.isBlank())
            return 0;
        Matcher m = DURATION_PATTERN.matcher(raw.trim());
        if (!m.matches())
            return 0;
        long value = Long.parseLong(m.group(1));
        String unit = m.group(2);
        if (unit == null)
            unit = "m"; // default: minutes
        return switch (unit.toLowerCase()) {
            case "ms" -> value;
            case "s" -> value * 1000;
            case "m" -> value * 60_000;
            case "h" -> value * 3_600_000;
            case "d" -> value * 86_400_000;
            default -> value * 60_000;
        };
    }

    /**
     * Get milliseconds since midnight in the given timezone.
     */
    static long timeOfDayMs(Instant instant, String timeZone) {
        try {
            ZoneId zone = ZoneId.of(timeZone);
            LocalTime localTime = instant.atZone(zone).toLocalTime();
            return localTime.toNanoOfDay() / 1_000_000;
        } catch (Exception e) {
            return -1;
        }
    }

    static double clampChance(Double value) {
        if (value == null || !Double.isFinite(value))
            return 0;
        return Math.min(1.0, Math.max(0.0, value));
    }

    private static String resolveTimezone(String userTimezone) {
        if (userTimezone != null && !userTimezone.isBlank()) {
            try {
                ZoneId.of(userTimezone);
                return userTimezone;
            } catch (Exception ignored) {
            }
        }
        return ZoneId.systemDefault().getId();
    }
}

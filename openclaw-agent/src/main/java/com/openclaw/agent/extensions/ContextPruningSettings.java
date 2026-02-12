package com.openclaw.agent.extensions;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Context pruning configuration and effective settings.
 * Mirrors pi-extensions/context-pruning/settings.ts.
 */
public final class ContextPruningSettings {

    private ContextPruningSettings() {
    }

    // --- Config types (raw from config file) ---

    public enum Mode {
        OFF, CACHE_TTL
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ToolMatch {
        private List<String> allow;
        private List<String> deny;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SoftTrimConfig {
        private Integer maxChars;
        private Integer headChars;
        private Integer tailChars;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HardClearConfig {
        private Boolean enabled;
        private String placeholder;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Config {
        private String mode;
        private String ttl;
        private Integer keepLastAssistants;
        private Double softTrimRatio;
        private Double hardClearRatio;
        private Integer minPrunableToolChars;
        private ToolMatch tools;
        private SoftTrimConfig softTrim;
        private HardClearConfig hardClear;
    }

    // --- Effective settings (fully resolved defaults) ---

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SoftTrim {
        private int maxChars;
        private int headChars;
        private int tailChars;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HardClear {
        private boolean enabled;
        private String placeholder;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Effective {
        private Mode mode;
        private long ttlMs;
        private int keepLastAssistants;
        private double softTrimRatio;
        private double hardClearRatio;
        private int minPrunableToolChars;
        private ToolMatch tools;
        private SoftTrim softTrim;
        private HardClear hardClear;
    }

    // --- Default constants ---

    public static final Effective DEFAULTS = Effective.builder()
            .mode(Mode.CACHE_TTL)
            .ttlMs(5 * 60 * 1000L)
            .keepLastAssistants(3)
            .softTrimRatio(0.3)
            .hardClearRatio(0.5)
            .minPrunableToolChars(50_000)
            .tools(new ToolMatch())
            .softTrim(SoftTrim.builder()
                    .maxChars(4_000)
                    .headChars(1_500)
                    .tailChars(1_500)
                    .build())
            .hardClear(HardClear.builder()
                    .enabled(true)
                    .placeholder("[Old tool result content cleared]")
                    .build())
            .build();

    /**
     * Compute effective settings from raw config.
     * Returns null if pruning should be off.
     */
    public static Effective computeEffective(Object raw) {
        if (!(raw instanceof Map<?, ?> rawMap)) {
            return null;
        }

        String modeStr = getStr(rawMap, "mode");
        if (!"cache-ttl".equals(modeStr)) {
            return null;
        }

        Effective s = Effective.builder()
                .mode(Mode.CACHE_TTL)
                .ttlMs(DEFAULTS.getTtlMs())
                .keepLastAssistants(DEFAULTS.getKeepLastAssistants())
                .softTrimRatio(DEFAULTS.getSoftTrimRatio())
                .hardClearRatio(DEFAULTS.getHardClearRatio())
                .minPrunableToolChars(DEFAULTS.getMinPrunableToolChars())
                .tools(DEFAULTS.getTools())
                .softTrim(SoftTrim.builder()
                        .maxChars(DEFAULTS.getSoftTrim().getMaxChars())
                        .headChars(DEFAULTS.getSoftTrim().getHeadChars())
                        .tailChars(DEFAULTS.getSoftTrim().getTailChars())
                        .build())
                .hardClear(HardClear.builder()
                        .enabled(DEFAULTS.getHardClear().isEnabled())
                        .placeholder(DEFAULTS.getHardClear().getPlaceholder())
                        .build())
                .build();

        // Parse TTL duration
        String ttl = getStr(rawMap, "ttl");
        if (ttl != null) {
            long parsed = parseDurationMs(ttl);
            if (parsed >= 0) {
                s.setTtlMs(parsed);
            }
        }

        Integer keepLast = getInt(rawMap, "keepLastAssistants");
        if (keepLast != null)
            s.setKeepLastAssistants(Math.max(0, keepLast));

        Double softRatio = getDbl(rawMap, "softTrimRatio");
        if (softRatio != null)
            s.setSoftTrimRatio(Math.min(1, Math.max(0, softRatio)));

        Double hardRatio = getDbl(rawMap, "hardClearRatio");
        if (hardRatio != null)
            s.setHardClearRatio(Math.min(1, Math.max(0, hardRatio)));

        Integer minChars = getInt(rawMap, "minPrunableToolChars");
        if (minChars != null)
            s.setMinPrunableToolChars(Math.max(0, minChars));

        if (rawMap.get("softTrim") instanceof Map<?, ?> stMap) {
            Integer mc = getInt(stMap, "maxChars");
            if (mc != null)
                s.getSoftTrim().setMaxChars(Math.max(0, mc));
            Integer hc = getInt(stMap, "headChars");
            if (hc != null)
                s.getSoftTrim().setHeadChars(Math.max(0, hc));
            Integer tc = getInt(stMap, "tailChars");
            if (tc != null)
                s.getSoftTrim().setTailChars(Math.max(0, tc));
        }

        if (rawMap.get("hardClear") instanceof Map<?, ?> hcMap) {
            if (hcMap.get("enabled") instanceof Boolean en) {
                s.getHardClear().setEnabled(en);
            }
            String ph = getStr(hcMap, "placeholder");
            if (ph != null && !ph.isBlank()) {
                s.getHardClear().setPlaceholder(ph.strip());
            }
        }

        return s;
    }

    // --- helpers ---

    private static final Pattern DURATION_RE = Pattern.compile("^(\\d+(?:\\.\\d+)?)\\s*([smhd]?)$");

    static long parseDurationMs(String input) {
        String trimmed = input.strip().toLowerCase();
        var m = DURATION_RE.matcher(trimmed);
        if (!m.matches())
            return -1;
        double val = Double.parseDouble(m.group(1));
        String unit = m.group(2);
        if (unit == null || unit.isEmpty() || "m".equals(unit))
            return (long) (val * 60_000);
        return switch (unit) {
            case "s" -> (long) (val * 1_000);
            case "h" -> (long) (val * 3_600_000);
            case "d" -> (long) (val * 86_400_000);
            default -> (long) (val * 60_000);
        };
    }

    private static String getStr(Map<?, ?> map, String key) {
        Object v = map.get(key);
        return v instanceof String s ? s : null;
    }

    private static Integer getInt(Map<?, ?> map, String key) {
        Object v = map.get(key);
        if (v instanceof Number n)
            return n.intValue();
        return null;
    }

    private static Double getDbl(Map<?, ?> map, String key) {
        Object v = map.get(key);
        if (v instanceof Number n) {
            double d = n.doubleValue();
            return Double.isFinite(d) ? d : null;
        }
        return null;
    }
}

package com.openclaw.agent.runtime;

import java.util.Map;

/**
 * Token-usage normalization utilities.
 * <p>
 * Provider SDKs report token usage under many different field names.
 * This class maps them all to a single canonical {@link NormalizedUsage}
 * record. Mirrors {@code agents/usage.ts}.
 */
public final class UsageUtils {

    private UsageUtils() {
    }

    // --- Canonical usage record ---

    public record NormalizedUsage(
            Integer input,
            Integer output,
            Integer cacheRead,
            Integer cacheWrite,
            Integer total) {
    }

    // --- Public API ---

    public static boolean hasNonzeroUsage(NormalizedUsage usage) {
        if (usage == null)
            return false;
        return isPositive(usage.input())
                || isPositive(usage.output())
                || isPositive(usage.cacheRead())
                || isPositive(usage.cacheWrite())
                || isPositive(usage.total());
    }

    /**
     * Normalize a raw usage map, accepting many provider-specific key variants.
     */
    public static NormalizedUsage normalizeUsage(Map<String, Object> raw) {
        if (raw == null || raw.isEmpty())
            return null;

        Integer input = firstFinite(raw,
                "input", "inputTokens", "input_tokens", "promptTokens", "prompt_tokens");
        Integer output = firstFinite(raw,
                "output", "outputTokens", "output_tokens", "completionTokens", "completion_tokens");
        Integer cacheRead = firstFinite(raw,
                "cacheRead", "cache_read", "cache_read_input_tokens");
        Integer cacheWrite = firstFinite(raw,
                "cacheWrite", "cache_write", "cache_creation_input_tokens");
        Integer total = firstFinite(raw,
                "total", "totalTokens", "total_tokens");

        if (input == null && output == null && cacheRead == null
                && cacheWrite == null && total == null) {
            return null;
        }
        return new NormalizedUsage(input, output, cacheRead, cacheWrite, total);
    }

    /**
     * Derive the prompt-side total: input + cacheRead + cacheWrite.
     * Returns {@code null} when the resulting sum is zero or absent.
     */
    public static Integer derivePromptTokens(Integer input, Integer cacheRead, Integer cacheWrite) {
        int i = input != null ? input : 0;
        int cr = cacheRead != null ? cacheRead : 0;
        int cw = cacheWrite != null ? cacheWrite : 0;
        int sum = i + cr + cw;
        return sum > 0 ? sum : null;
    }

    // --- Internal helpers ---

    private static boolean isPositive(Integer v) {
        return v != null && v > 0;
    }

    private static Integer firstFinite(Map<String, Object> map, String... keys) {
        for (String key : keys) {
            Object v = map.get(key);
            if (v instanceof Number n) {
                double d = n.doubleValue();
                if (Double.isFinite(d)) {
                    return n.intValue();
                }
            }
        }
        return null;
    }
}

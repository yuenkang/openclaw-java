package com.openclaw.common.infra;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Diagnostic flag resolution and matching. Flags can be set via config or
 * environment variable {@code OPENCLAW_DIAGNOSTICS} and support glob-like
 * patterns ({@code *}, {@code prefix.*}).
 * Corresponds to TypeScript's infra/diagnostic-flags.ts.
 */
public final class DiagnosticFlags {

    private DiagnosticFlags() {
    }

    private static final String DIAGNOSTICS_ENV = "OPENCLAW_DIAGNOSTICS";

    private static final Set<String> OFF_VALUES = Set.of("0", "false", "off", "none");
    private static final Set<String> ALL_VALUES = Set.of("1", "true", "all", "*");

    // ── Normalization ───────────────────────────────────────────────────

    static String normalizeFlag(String value) {
        return value != null ? value.trim().toLowerCase(Locale.ROOT) : "";
    }

    // ── Parsing ─────────────────────────────────────────────────────────

    /**
     * Parse the environment variable value into a list of diagnostic flags.
     */
    static List<String> parseEnvFlags(String raw) {
        if (raw == null || raw.isBlank()) {
            return List.of();
        }
        String lowered = raw.trim().toLowerCase(Locale.ROOT);
        if (OFF_VALUES.contains(lowered)) {
            return List.of();
        }
        if (ALL_VALUES.contains(lowered)) {
            return List.of("*");
        }
        return Arrays.stream(raw.trim().split("[,\\s]+"))
                .map(DiagnosticFlags::normalizeFlag)
                .filter(s -> !s.isEmpty())
                .toList();
    }

    /**
     * Deduplicate flags while preserving order.
     */
    static List<String> uniqueFlags(List<String> flags) {
        Set<String> seen = new LinkedHashSet<>();
        List<String> out = new ArrayList<>();
        for (String flag : flags) {
            String normalized = normalizeFlag(flag);
            if (!normalized.isEmpty() && seen.add(normalized)) {
                out.add(normalized);
            }
        }
        return out;
    }

    // ── Resolution ──────────────────────────────────────────────────────

    /**
     * Resolve diagnostic flags from config flags and environment.
     *
     * @param configFlags flags from configuration (may be {@code null})
     * @return merged list of unique flags
     */
    public static List<String> resolve(List<String> configFlags) {
        List<String> merged = new ArrayList<>();
        if (configFlags != null) {
            merged.addAll(configFlags);
        }
        String envValue = System.getenv(DIAGNOSTICS_ENV);
        merged.addAll(parseEnvFlags(envValue));
        return uniqueFlags(merged);
    }

    // ── Matching ────────────────────────────────────────────────────────

    /**
     * Check if a flag matches any of the enabled flags.
     * Supports exact match, wildcard {@code *} (all), prefix {@code prefix.*},
     * and trailing wildcard {@code prefix*}.
     *
     * @param flag         the flag to check
     * @param enabledFlags list of enabled flag patterns
     * @return {@code true} if the flag matches
     */
    public static boolean matches(String flag, List<String> enabledFlags) {
        String target = normalizeFlag(flag);
        if (target.isEmpty()) {
            return false;
        }
        for (String raw : enabledFlags) {
            String enabled = normalizeFlag(raw);
            if (enabled.isEmpty()) {
                continue;
            }
            if ("*".equals(enabled) || "all".equals(enabled)) {
                return true;
            }
            if (enabled.endsWith(".*")) {
                String prefix = enabled.substring(0, enabled.length() - 2);
                if (target.equals(prefix) || target.startsWith(prefix + ".")) {
                    return true;
                }
            }
            if (enabled.endsWith("*")) {
                String prefix = enabled.substring(0, enabled.length() - 1);
                if (target.startsWith(prefix)) {
                    return true;
                }
            }
            if (enabled.equals(target)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Convenience: check if a flag is enabled given config flags.
     */
    public static boolean isEnabled(String flag, List<String> configFlags) {
        return matches(flag, resolve(configFlags));
    }
}

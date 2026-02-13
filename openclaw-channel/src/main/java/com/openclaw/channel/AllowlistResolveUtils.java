package com.openclaw.channel;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Allowlist resolution utilities.
 * Corresponds to TypeScript's channels/allowlists/resolve-utils.ts.
 */
public final class AllowlistResolveUtils {

    private AllowlistResolveUtils() {
    }

    /**
     * Merge existing and new allowlist entries, deduplicating case-insensitively
     * while preserving the original casing of the first occurrence.
     */
    public static List<String> mergeAllowlist(List<String> existing, List<String> additions) {
        Set<String> seen = new LinkedHashSet<>();
        List<String> merged = new ArrayList<>();

        Consumer<String> push = value -> {
            String normalized = value.trim();
            if (normalized.isEmpty())
                return;
            String key = normalized.toLowerCase();
            if (seen.contains(key))
                return;
            seen.add(key);
            merged.add(normalized);
        };

        if (existing != null) {
            existing.forEach(push);
        }
        if (additions != null) {
            additions.forEach(push);
        }
        return merged;
    }

    /**
     * Summarize a mapping result with a sample of up to 6 entries.
     */
    public static String summarizeMapping(String label, List<String> mapping, List<String> unresolved) {
        StringBuilder sb = new StringBuilder();
        if (mapping != null && !mapping.isEmpty()) {
            List<String> sample = mapping.subList(0, Math.min(6, mapping.size()));
            String suffix = mapping.size() > sample.size()
                    ? " (+" + (mapping.size() - sample.size()) + ")"
                    : "";
            sb.append(label).append(" resolved: ").append(String.join(", ", sample)).append(suffix);
        }
        if (unresolved != null && !unresolved.isEmpty()) {
            if (!sb.isEmpty())
                sb.append("\n");
            List<String> sample = unresolved.subList(0, Math.min(6, unresolved.size()));
            String suffix = unresolved.size() > sample.size()
                    ? " (+" + (unresolved.size() - sample.size()) + ")"
                    : "";
            sb.append(label).append(" unresolved: ").append(String.join(", ", sample)).append(suffix);
        }
        return sb.isEmpty() ? null : sb.toString();
    }
}

package com.openclaw.autoreply.reply;

import java.util.*;

/**
 * Model picker — build sorted model items from a catalog and resolve
 * provider endpoint labels.
 * Mirrors {@code auto-reply/reply/directive-handling.model-picker.ts}.
 */
public final class DirectiveHandlingModelPicker {

    private DirectiveHandlingModelPicker() {
    }

    /** A catalog entry (provider + model id). */
    public record ModelPickerCatalogEntry(String provider, String id, String name) {
    }

    /** A resolved model reference (provider + model). */
    public record ModelPickerItem(String provider, String model) {
    }

    /** Provider display priority — lower index = higher rank. */
    private static final List<String> PROVIDER_PREFERENCE = List.of(
            "anthropic", "openai", "openai-codex", "minimax", "synthetic",
            "google", "zai", "openrouter", "opencode", "github-copilot",
            "groq", "cerebras", "mistral", "xai", "lmstudio");

    private static final Map<String, Integer> PROVIDER_RANK;

    static {
        Map<String, Integer> map = new HashMap<>();
        for (int i = 0; i < PROVIDER_PREFERENCE.size(); i++) {
            map.put(PROVIDER_PREFERENCE.get(i), i);
        }
        PROVIDER_RANK = Collections.unmodifiableMap(map);
    }

    private static int compareProvidersForPicker(String a, String b) {
        Integer pa = PROVIDER_RANK.get(a);
        Integer pb = PROVIDER_RANK.get(b);
        if (pa != null && pb != null)
            return pa - pb;
        if (pa != null)
            return -1;
        if (pb != null)
            return 1;
        return a.compareTo(b);
    }

    /**
     * Build sorted model items from catalog entries.
     */
    public static List<ModelPickerItem> buildModelPickerItems(List<ModelPickerCatalogEntry> catalog) {
        Set<String> seen = new LinkedHashSet<>();
        List<ModelPickerItem> out = new ArrayList<>();

        for (ModelPickerCatalogEntry entry : catalog) {
            String provider = entry.provider() != null ? entry.provider().trim().toLowerCase() : null;
            String model = entry.id() != null ? entry.id().trim() : null;
            if (provider == null || provider.isEmpty() || model == null || model.isEmpty())
                continue;

            String key = provider + "/" + model;
            if (seen.contains(key))
                continue;
            seen.add(key);
            out.add(new ModelPickerItem(provider, model));
        }

        out.sort((a, b) -> {
            int cmp = compareProvidersForPicker(a.provider(), b.provider());
            if (cmp != 0)
                return cmp;
            return a.model().toLowerCase().compareTo(b.model().toLowerCase());
        });

        return out;
    }

    /** Resolved endpoint/api labels for a provider. */
    public record ProviderEndpointLabel(String endpoint, String api) {
    }

    /**
     * Resolve provider endpoint label from config.
     * Full config integration deferred.
     */
    public static ProviderEndpointLabel resolveProviderEndpointLabel(
            String provider, Map<String, Object> cfg) {
        // Simplified — full config lookup deferred
        return new ProviderEndpointLabel(null, null);
    }
}

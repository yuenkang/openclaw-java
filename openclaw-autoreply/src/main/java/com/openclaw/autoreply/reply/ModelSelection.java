package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Model selection — resolve stored model overrides, fuzzy-match user
 * input against the allowed model catalog, bounded Levenshtein scoring,
 * and session-level model-override state management.
 * Mirrors {@code auto-reply/reply/model-selection.ts}.
 */
public final class ModelSelection {

    private static final Logger log = LoggerFactory.getLogger(ModelSelection.class);

    private ModelSelection() {
    }

    // --- Types ---

    /** A resolved model directive selection. */
    public record ModelDirectiveSelection(
            String provider,
            String model,
            boolean isDefault,
            String alias) {
    }

    /** A stored model override from session or parent session. */
    public record StoredModelOverride(
            String provider,
            String model,
            String source) { // "session" | "parent"
    }

    /** State produced by createModelSelectionState. */
    public record ModelSelectionState(
            String provider,
            String model,
            Set<String> allowedModelKeys,
            List<Map<String, Object>> allowedModelCatalog,
            boolean resetModelOverride,
            boolean needsModelCatalog) {
    }

    // --- Fuzzy variant tokens ---

    private static final List<String> FUZZY_VARIANT_TOKENS = List.of(
            "lightning", "preview", "mini", "fast", "turbo",
            "lite", "beta", "small", "nano");

    // --- Bounded Levenshtein ---

    /**
     * Compute Levenshtein distance bounded by maxDistance.
     *
     * @return distance or null if exceeds maxDistance
     */
    static Integer boundedLevenshteinDistance(String a, String b, int maxDistance) {
        if (a.equals(b))
            return 0;
        if (a.isEmpty() || b.isEmpty())
            return null;
        int aLen = a.length();
        int bLen = b.length();
        if (Math.abs(aLen - bLen) > maxDistance)
            return null;

        int[] prev = new int[bLen + 1];
        int[] curr = new int[bLen + 1];
        for (int j = 0; j <= bLen; j++)
            prev[j] = j;

        for (int i = 1; i <= aLen; i++) {
            curr[0] = i;
            int rowMin = curr[0];
            char aChar = a.charAt(i - 1);

            for (int j = 1; j <= bLen; j++) {
                int cost = aChar == b.charAt(j - 1) ? 0 : 1;
                curr[j] = Math.min(Math.min(prev[j] + 1, curr[j - 1] + 1), prev[j - 1] + cost);
                if (curr[j] < rowMin)
                    rowMin = curr[j];
            }

            if (rowMin > maxDistance)
                return null;
            System.arraycopy(curr, 0, prev, 0, bLen + 1);
        }

        return prev[bLen] <= maxDistance ? prev[bLen] : null;
    }

    // --- Stored model override resolution ---

    private static StoredModelOverride resolveModelOverrideFromEntry(Map<String, Object> entry) {
        if (entry == null)
            return null;
        String model = entry.get("modelOverride") != null
                ? entry.get("modelOverride").toString().trim()
                : null;
        if (model == null || model.isEmpty())
            return null;
        String provider = entry.get("providerOverride") != null
                ? entry.get("providerOverride").toString().trim()
                : null;
        if (provider != null && provider.isEmpty())
            provider = null;
        return new StoredModelOverride(provider, model, "session");
    }

    /**
     * Resolve stored model override from session entry or parent session.
     */
    public static StoredModelOverride resolveStoredModelOverride(
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String parentSessionKey) {
        StoredModelOverride direct = resolveModelOverrideFromEntry(sessionEntry);
        if (direct != null)
            return direct;

        String parentKey = resolveParentSessionKeyCandidate(sessionKey, parentSessionKey);
        if (parentKey == null || sessionStore == null)
            return null;

        @SuppressWarnings("unchecked")
        Map<String, Object> parentEntry = (Map<String, Object>) sessionStore.get(parentKey);
        StoredModelOverride parent = resolveModelOverrideFromEntry(parentEntry);
        if (parent == null)
            return null;
        return new StoredModelOverride(parent.provider(), parent.model(), "parent");
    }

    private static String resolveParentSessionKeyCandidate(
            String sessionKey, String parentSessionKey) {
        if (parentSessionKey != null && !parentSessionKey.trim().isEmpty()
                && !parentSessionKey.equals(sessionKey)) {
            return parentSessionKey.trim();
        }
        // resolveThreadParentSessionKey deferred
        return null;
    }

    // --- Fuzzy scoring ---

    record FuzzyScore(
            int score, boolean isDefault,
            int variantCount, int variantMatchCount,
            int modelLength, String key) {
    }

    static FuzzyScore scoreFuzzyMatch(
            String provider, String model, String fragment,
            Map<String, List<String>> aliasIndex,
            String defaultProvider, String defaultModel) {

        String normalizedProvider = normalizeProviderId(provider);
        String fragLower = fragment.trim().toLowerCase();
        String providerLower = normalizedProvider.toLowerCase();
        String modelLower = model.toLowerCase();
        String haystack = providerLower + "/" + modelLower;
        String key = modelKey(normalizedProvider, model);

        int score = 0;
        score += scoreFragment(haystack, fragLower, 220, 140, 110);
        score += scoreFragment(providerLower, fragLower, 180, 120, 90);
        score += scoreFragment(modelLower, fragLower, 160, 110, 80);

        // Typo tolerance
        Integer dist = boundedLevenshteinDistance(fragLower, modelLower, 3);
        if (dist != null) {
            score += (3 - dist) * 70;
        }

        // Alias scoring
        List<String> aliases = aliasIndex != null ? aliasIndex.getOrDefault(key, List.of()) : List.of();
        for (String alias : aliases) {
            score += scoreFragment(alias.toLowerCase(), fragLower, 140, 90, 60);
        }

        if (modelLower.startsWith(providerLower)) {
            score += 30;
        }

        // Variant tokens
        List<String> fragVariants = FUZZY_VARIANT_TOKENS.stream()
                .filter(fragLower::contains).toList();
        List<String> modelVariants = FUZZY_VARIANT_TOKENS.stream()
                .filter(modelLower::contains).toList();
        int variantMatchCount = (int) fragVariants.stream()
                .filter(modelLower::contains).count();
        int variantCount = modelVariants.size();

        if (fragVariants.isEmpty() && variantCount > 0) {
            score -= variantCount * 30;
        } else if (!fragVariants.isEmpty()) {
            if (variantMatchCount > 0)
                score += variantMatchCount * 40;
            if (variantMatchCount == 0)
                score -= 20;
        }

        String normDefault = normalizeProviderId(defaultProvider);
        boolean isDefault = normalizedProvider.equals(normDefault) && model.equals(defaultModel);
        if (isDefault)
            score += 20;

        return new FuzzyScore(score, isDefault, variantCount, variantMatchCount,
                modelLower.length(), key);
    }

    private static int scoreFragment(String value, String fragment,
            int exact, int starts, int includes) {
        if (fragment.isEmpty())
            return 0;
        int score = 0;
        if (value.equals(fragment))
            score = Math.max(score, exact);
        if (value.startsWith(fragment))
            score = Math.max(score, starts);
        if (value.contains(fragment))
            score = Math.max(score, includes);
        return score;
    }

    // --- Model selection state creation ---

    /**
     * Create model selection state with catalog loading and override resolution.
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<ModelSelectionState> createModelSelectionState(
            Map<String, Object> cfg,
            Map<String, Object> agentCfg,
            Map<String, Object> sessionEntry,
            Map<String, Object> sessionStore,
            String sessionKey,
            String parentSessionKey,
            String storePath,
            String defaultProvider,
            String defaultModel,
            String provider,
            String model,
            boolean hasModelDirective) {

        boolean hasAllowlist = agentCfg != null
                && agentCfg.get("models") instanceof Map<?, ?> m && !m.isEmpty();
        StoredModelOverride initialOverride = resolveStoredModelOverride(
                sessionEntry, sessionStore, sessionKey, parentSessionKey);
        boolean hasStoredOverride = initialOverride != null;
        boolean needsModelCatalog = hasModelDirective || hasAllowlist || hasStoredOverride;

        Set<String> allowedModelKeys = new HashSet<>();
        List<Map<String, Object>> allowedModelCatalog = List.of();
        boolean resetModelOverride = false;

        String resolvedProvider = provider;
        String resolvedModel = model;

        // Catalog loading deferred — integrate with ModelCatalog service

        // Apply stored override
        StoredModelOverride storedOverride = resolveStoredModelOverride(
                sessionEntry, sessionStore, sessionKey, parentSessionKey);
        if (storedOverride != null && storedOverride.model() != null) {
            String candidateProvider = storedOverride.provider() != null
                    ? storedOverride.provider()
                    : defaultProvider;
            String key = modelKey(candidateProvider, storedOverride.model());
            if (allowedModelKeys.isEmpty() || allowedModelKeys.contains(key)) {
                resolvedProvider = candidateProvider;
                resolvedModel = storedOverride.model();
            }
        }

        ModelSelectionState state = new ModelSelectionState(
                resolvedProvider, resolvedModel,
                allowedModelKeys, allowedModelCatalog,
                resetModelOverride, needsModelCatalog);
        return CompletableFuture.completedFuture(state);
    }

    // --- Model directive resolution ---

    /** Result of resolving a model directive. */
    public record ModelResolution(
            ModelDirectiveSelection selection,
            String error) {
    }

    /**
     * Resolve a model directive selection from user input.
     */
    public static ModelResolution resolveModelDirectiveSelection(
            String raw, String defaultProvider, String defaultModel,
            Map<String, List<String>> aliasIndex,
            Set<String> allowedModelKeys) {

        String rawTrimmed = raw.trim();
        if (rawTrimmed.isEmpty()) {
            return new ModelResolution(null, "Empty model specifier.");
        }

        // Try exact resolution first
        // Full resolveModelRefFromString deferred — try fuzzy
        return resolveFuzzy(rawTrimmed, null, allowedModelKeys,
                aliasIndex, defaultProvider, defaultModel);
    }

    private static ModelResolution resolveFuzzy(
            String fragment, String providerFilter,
            Set<String> allowedModelKeys,
            Map<String, List<String>> aliasIndex,
            String defaultProvider, String defaultModel) {

        String fragLower = fragment.trim().toLowerCase();
        if (fragLower.isEmpty())
            return new ModelResolution(null, null);

        String normProvFilter = providerFilter != null ? normalizeProviderId(providerFilter) : null;

        List<String[]> candidates = new ArrayList<>();
        for (String key : allowedModelKeys) {
            int slash = key.indexOf('/');
            if (slash <= 0)
                continue;
            String prov = normalizeProviderId(key.substring(0, slash));
            String model = key.substring(slash + 1);
            if (normProvFilter != null && !prov.equals(normProvFilter))
                continue;
            candidates.add(new String[] { prov, model });
        }

        if (candidates.isEmpty()) {
            return new ModelResolution(null,
                    "Unrecognized model \"" + fragment
                            + "\". Use /models to list providers.");
        }

        // Score and sort
        List<Map.Entry<String[], FuzzyScore>> scored = candidates.stream()
                .map(c -> Map.entry(c, scoreFuzzyMatch(
                        c[0], c[1], fragment, aliasIndex, defaultProvider, defaultModel)))
                .sorted((a, b) -> {
                    int cmp = Integer.compare(b.getValue().score(), a.getValue().score());
                    if (cmp != 0)
                        return cmp;
                    if (a.getValue().isDefault() != b.getValue().isDefault())
                        return a.getValue().isDefault() ? -1 : 1;
                    cmp = Integer.compare(b.getValue().variantMatchCount(), a.getValue().variantMatchCount());
                    if (cmp != 0)
                        return cmp;
                    cmp = Integer.compare(a.getValue().variantCount(), b.getValue().variantCount());
                    if (cmp != 0)
                        return cmp;
                    cmp = Integer.compare(a.getValue().modelLength(), b.getValue().modelLength());
                    if (cmp != 0)
                        return cmp;
                    return a.getValue().key().compareTo(b.getValue().key());
                })
                .toList();

        if (scored.isEmpty())
            return new ModelResolution(null, null);

        Map.Entry<String[], FuzzyScore> best = scored.get(0);
        int minScore = normProvFilter != null ? 90 : 120;
        if (best.getValue().score() < minScore) {
            return new ModelResolution(null, null);
        }

        String[] c = best.getKey();
        String alias = aliasIndex != null
                ? aliasIndex.getOrDefault(modelKey(c[0], c[1]), List.of())
                        .stream().findFirst().orElse(null)
                : null;
        ModelDirectiveSelection sel = new ModelDirectiveSelection(
                c[0], c[1], best.getValue().isDefault(), alias);
        return new ModelResolution(sel, null);
    }

    // --- Context tokens ---

    /** Resolve context token budget. */
    public static int resolveContextTokens(Map<String, Object> agentCfg, String model) {
        if (agentCfg != null && agentCfg.get("contextTokens") instanceof Number n) {
            return n.intValue();
        }
        // lookupContextTokens deferred
        return 128_000; // DEFAULT_CONTEXT_TOKENS
    }

    // --- Utility ---

    static String modelKey(String provider, String model) {
        return normalizeProviderId(provider) + "/" + model;
    }

    static String normalizeProviderId(String provider) {
        if (provider == null)
            return "";
        return provider.trim().toLowerCase();
    }
}

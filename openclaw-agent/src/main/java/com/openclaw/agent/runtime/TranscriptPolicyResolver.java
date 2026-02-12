package com.openclaw.agent.runtime;

import java.util.Set;

/**
 * Determine transcript sanitisation policies for a given model/provider.
 * Different providers have different requirements for tool-call IDs,
 * turn ordering, thinking blocks, etc.
 * Mirrors {@code agents/transcript-policy.ts}.
 */
public final class TranscriptPolicyResolver {

    private TranscriptPolicyResolver() {
    }

    // --- Result type ---

    public enum SanitizeMode {
        FULL, IMAGES_ONLY
    }

    public record TranscriptPolicy(
            SanitizeMode sanitizeMode,
            boolean sanitizeToolCallIds,
            ToolCallIdSanitizer.Mode toolCallIdMode,
            boolean repairToolUseResultPairing,
            boolean preserveSignatures,
            boolean sanitizeThoughtSignatures,
            boolean sanitizeThoughtSignaturesAllowBase64Only,
            boolean sanitizeThoughtSignaturesIncludeCamelCase,
            boolean normalizeAntigravityThinkingBlocks,
            boolean applyGoogleTurnOrdering,
            boolean validateGeminiTurns,
            boolean validateAnthropicTurns,
            boolean allowSyntheticToolResults) {
    }

    // --- Known model hints ---

    private static final Set<String> MISTRAL_HINTS = Set.of(
            "mistral", "mixtral", "codestral", "pixtral",
            "devstral", "ministral", "mistralai");

    private static final Set<String> OPENAI_APIS = Set.of(
            "openai", "openai-completions", "openai-responses", "openai-codex-responses");

    private static final Set<String> OPENAI_PROVIDERS = Set.of("openai", "openai-codex");

    private static final Set<String> GOOGLE_APIS = Set.of(
            "google", "google-genai", "vertex", "vertex-ai");

    // --- Resolution ---

    public static TranscriptPolicy resolve(String modelApi, String provider, String modelId) {
        String normProvider = normalizeProvider(provider);
        String normModelId = modelId != null ? modelId.toLowerCase() : "";

        boolean isGoogle = isGoogleApi(modelApi);
        boolean isAnthropic = isAnthropicApi(modelApi, normProvider);
        boolean isOpenAi = isOpenAiProvider(normProvider)
                || (normProvider.isEmpty() && isOpenAiApi(modelApi));
        boolean isMistral = isMistralModel(normProvider, normModelId);
        boolean isOpenRouterGemini = ("openrouter".equals(normProvider) || "opencode".equals(normProvider))
                && normModelId.contains("gemini");
        boolean isAntigravityClaude = isAntigravityClaudeModel(modelApi, normProvider, normModelId);

        boolean needsNonImageSanitize = isGoogle || isAnthropic || isMistral || isOpenRouterGemini;
        boolean sanitizeToolCallIds = isGoogle || isMistral;

        ToolCallIdSanitizer.Mode toolCallIdMode = null;
        if (!isOpenAi && sanitizeToolCallIds) {
            toolCallIdMode = isMistral ? ToolCallIdSanitizer.Mode.STRICT9
                    : ToolCallIdSanitizer.Mode.STRICT;
        }

        boolean repairPairing = isGoogle || isAnthropic;

        boolean sanitizeThoughtSigs = !isOpenAi && isOpenRouterGemini;
        boolean base64Only = isOpenRouterGemini;
        boolean includeCamelCase = isOpenRouterGemini;

        return new TranscriptPolicy(
                isOpenAi ? SanitizeMode.IMAGES_ONLY
                        : (needsNonImageSanitize ? SanitizeMode.FULL : SanitizeMode.IMAGES_ONLY),
                !isOpenAi && sanitizeToolCallIds,
                toolCallIdMode,
                !isOpenAi && repairPairing,
                isAntigravityClaude,
                sanitizeThoughtSigs,
                base64Only,
                includeCamelCase,
                isAntigravityClaude,
                !isOpenAi && isGoogle,
                !isOpenAi && isGoogle,
                !isOpenAi && isAnthropic,
                !isOpenAi && (isGoogle || isAnthropic));
    }

    // --- Classifiers ---

    private static boolean isGoogleApi(String api) {
        return api != null && GOOGLE_APIS.contains(api);
    }

    private static boolean isOpenAiApi(String api) {
        return api != null && OPENAI_APIS.contains(api);
    }

    private static boolean isOpenAiProvider(String provider) {
        return OPENAI_PROVIDERS.contains(provider);
    }

    private static boolean isAnthropicApi(String api, String provider) {
        if ("anthropic-messages".equals(api))
            return true;
        return "anthropic".equals(provider);
    }

    private static boolean isMistralModel(String provider, String modelId) {
        if ("mistral".equals(provider))
            return true;
        if (modelId.isEmpty())
            return false;
        return MISTRAL_HINTS.stream().anyMatch(modelId::contains);
    }

    private static boolean isAntigravityClaudeModel(String api, String provider, String modelId) {
        // AntigravityClaude = Anthropic models accessed via Antigravity's API
        if ("anthropic-messages".equals(api) && "antigravity".equals(provider))
            return true;
        return modelId.contains("claude") && "antigravity".equals(provider);
    }

    private static String normalizeProvider(String provider) {
        if (provider == null)
            return "";
        String trimmed = provider.trim().toLowerCase();
        // Normalize common provider aliases
        return switch (trimmed) {
            case "open-ai", "openai-compatible" -> "openai";
            case "google-ai-studio", "google-genai" -> "google";
            default -> trimmed;
        };
    }
}

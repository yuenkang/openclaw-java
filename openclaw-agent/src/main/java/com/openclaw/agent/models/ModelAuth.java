package com.openclaw.agent.models;

import java.util.*;

/**
 * Provider API key resolution from environment variables, config, and profiles.
 * Mirrors {@code agents/model-auth.ts}.
 */
public final class ModelAuth {

    private ModelAuth() {
    }

    // --- AWS env vars ---
    private static final String AWS_BEARER_ENV = "AWS_BEARER_TOKEN_BEDROCK";
    private static final String AWS_ACCESS_KEY_ENV = "AWS_ACCESS_KEY_ID";
    private static final String AWS_SECRET_KEY_ENV = "AWS_SECRET_ACCESS_KEY";
    private static final String AWS_PROFILE_ENV = "AWS_PROFILE";

    // --- Result types ---

    public record ResolvedProviderAuth(
            String apiKey,
            String profileId,
            String source,
            String mode // "api-key" | "oauth" | "token" | "aws-sdk"
    ) {
    }

    public record EnvApiKeyResult(String apiKey, String source) {
    }

    // --- Public API ---

    /**
     * Resolve an API key from a config-level provider entry.
     */
    public static String getCustomProviderApiKey(
            Map<String, Map<String, Object>> providers, String provider) {
        if (providers == null)
            return null;
        Map<String, Object> entry = resolveProviderConfig(providers, provider);
        if (entry == null)
            return null;
        Object key = entry.get("apiKey");
        if (key instanceof String s) {
            String trimmed = s.trim();
            return trimmed.isEmpty() ? null : trimmed;
        }
        return null;
    }

    /**
     * Resolve the AWS SDK env var name that is set, if any.
     */
    public static String resolveAwsSdkEnvVarName() {
        if (hasEnv(AWS_BEARER_ENV))
            return AWS_BEARER_ENV;
        if (hasEnv(AWS_ACCESS_KEY_ENV) && hasEnv(AWS_SECRET_KEY_ENV))
            return AWS_ACCESS_KEY_ENV;
        if (hasEnv(AWS_PROFILE_ENV))
            return AWS_PROFILE_ENV;
        return null;
    }

    /**
     * Resolve an API key from environment variables for a specific provider.
     */
    public static EnvApiKeyResult resolveEnvApiKey(String provider) {
        String normalized = normalizeProviderId(provider);

        // Provider-specific multi-key lookups
        switch (normalized) {
            case "github-copilot":
                return pickFirst("COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN");
            case "anthropic":
                return pickFirst("ANTHROPIC_OAUTH_TOKEN", "ANTHROPIC_API_KEY");
            case "chutes":
                return pickFirst("CHUTES_OAUTH_TOKEN", "CHUTES_API_KEY");
            case "zai":
                return pickFirst("ZAI_API_KEY", "Z_AI_API_KEY");
            case "opencode":
                return pickFirst("OPENCODE_API_KEY", "OPENCODE_ZEN_API_KEY");
            case "qwen-portal":
                return pickFirst("QWEN_OAUTH_TOKEN", "QWEN_PORTAL_API_KEY");
            case "minimax-portal":
                return pickFirst("MINIMAX_OAUTH_TOKEN", "MINIMAX_API_KEY");
            case "kimi-coding":
                return pickFirst("KIMI_API_KEY", "KIMICODE_API_KEY");
        }

        // Standard single-key lookups
        Map<String, String> envMap = Map.ofEntries(
                Map.entry("openai", "OPENAI_API_KEY"),
                Map.entry("google", "GEMINI_API_KEY"),
                Map.entry("groq", "GROQ_API_KEY"),
                Map.entry("deepgram", "DEEPGRAM_API_KEY"),
                Map.entry("cerebras", "CEREBRAS_API_KEY"),
                Map.entry("xai", "XAI_API_KEY"),
                Map.entry("openrouter", "OPENROUTER_API_KEY"),
                Map.entry("vercel-ai-gateway", "AI_GATEWAY_API_KEY"),
                Map.entry("cloudflare-ai-gateway", "CLOUDFLARE_AI_GATEWAY_API_KEY"),
                Map.entry("moonshot", "MOONSHOT_API_KEY"),
                Map.entry("minimax", "MINIMAX_API_KEY"),
                Map.entry("xiaomi", "XIAOMI_API_KEY"),
                Map.entry("synthetic", "SYNTHETIC_API_KEY"),
                Map.entry("venice", "VENICE_API_KEY"),
                Map.entry("mistral", "MISTRAL_API_KEY"),
                Map.entry("ollama", "OLLAMA_API_KEY"));

        String envVar = envMap.get(normalized);
        if (envVar == null)
            return null;
        return pick(envVar);
    }

    /**
     * Require a non-empty API key, throwing if not available.
     */
    public static String requireApiKey(ResolvedProviderAuth auth, String provider) {
        String key = auth.apiKey() != null ? auth.apiKey().trim() : null;
        if (key != null && !key.isEmpty())
            return key;
        throw new IllegalStateException(
                "No API key resolved for provider \"" + provider
                        + "\" (auth mode: " + auth.mode() + ").");
    }

    // --- Helpers ---

    /**
     * Normalize a provider ID to a standard form.
     */
    public static String normalizeProviderId(String provider) {
        if (provider == null)
            return "";
        String lower = provider.toLowerCase().trim();
        // Common normalizations
        return switch (lower) {
            case "bedrock", "aws" -> "amazon-bedrock";
            case "vertex", "google-vertex-ai" -> "google-vertex";
            case "z.ai" -> "zai";
            default -> lower;
        };
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveProviderConfig(
            Map<String, Map<String, Object>> providers, String provider) {
        Map<String, Object> direct = providers.get(provider);
        if (direct != null)
            return direct;
        String normalized = normalizeProviderId(provider);
        Map<String, Object> byNorm = providers.get(normalized);
        if (byNorm != null)
            return byNorm;
        for (Map.Entry<String, Map<String, Object>> e : providers.entrySet()) {
            if (normalizeProviderId(e.getKey()).equals(normalized))
                return e.getValue();
        }
        return null;
    }

    private static EnvApiKeyResult pick(String envVar) {
        String value = System.getenv(envVar);
        if (value != null && !value.trim().isEmpty()) {
            return new EnvApiKeyResult(value.trim(), "env: " + envVar);
        }
        return null;
    }

    private static EnvApiKeyResult pickFirst(String... envVars) {
        for (String envVar : envVars) {
            EnvApiKeyResult result = pick(envVar);
            if (result != null)
                return result;
        }
        return null;
    }

    private static boolean hasEnv(String name) {
        String value = System.getenv(name);
        return value != null && !value.trim().isEmpty();
    }
}

package com.openclaw.agent.embedded;

/**
 * Google model detection and turn ordering utilities.
 * Mirrors {@code agents/pi-embedded-helpers/google.ts}.
 */
public final class GoogleHelpers {

    private GoogleHelpers() {
    }

    /** Check if an API string identifies a Google model API. */
    public static boolean isGoogleModelApi(String api) {
        if (api == null)
            return false;
        return "google-gemini-cli".equals(api)
                || "google-generative-ai".equals(api)
                || "google-antigravity".equals(api);
    }

    /**
     * Check if the given model is Antigravity Claude (Google-hosted Claude).
     */
    public static boolean isAntigravityClaude(String api, String provider, String modelId) {
        String provLower = provider != null ? provider.toLowerCase() : "";
        String apiLower = api != null ? api.toLowerCase() : "";
        if (!"google-antigravity".equals(provLower) && !"google-antigravity".equals(apiLower)) {
            return false;
        }
        return modelId != null && modelId.toLowerCase().contains("claude");
    }
}

package com.openclaw.agent.runtime;

/**
 * Cloudflare AI Gateway integration constants and URL builder.
 * Corresponds to TypeScript agents/cloudflare-ai-gateway.ts.
 */
public final class CloudflareAIGateway {

    public static final String PROVIDER_ID = "cloudflare-ai-gateway";
    public static final String DEFAULT_MODEL_ID = "claude-sonnet-4-5";
    public static final String DEFAULT_MODEL_REF = PROVIDER_ID + "/" + DEFAULT_MODEL_ID;
    public static final int DEFAULT_CONTEXT_WINDOW = 200_000;
    public static final int DEFAULT_MAX_TOKENS = 64_000;

    private CloudflareAIGateway() {
    }

    /**
     * Cost per million tokens.
     */
    public record Cost(double input, double output, double cacheRead, double cacheWrite) {
    }

    public static final Cost DEFAULT_COST = new Cost(3.0, 15.0, 0.3, 3.75);

    /**
     * Model definition for Cloudflare AI Gateway.
     */
    public record ModelDefinition(
            String id, String name, boolean reasoning,
            java.util.List<String> input,
            Cost cost, int contextWindow, int maxTokens) {
    }

    /**
     * Build a model definition for the Cloudflare AI Gateway.
     */
    public static ModelDefinition buildModelDefinition(
            String id, String name, Boolean reasoning, java.util.List<String> input) {
        return new ModelDefinition(
                (id != null && !id.isBlank()) ? id.trim() : DEFAULT_MODEL_ID,
                name != null ? name : "Claude Sonnet 4.5",
                reasoning != null ? reasoning : true,
                input != null ? input : java.util.List.of("text", "image"),
                DEFAULT_COST,
                DEFAULT_CONTEXT_WINDOW,
                DEFAULT_MAX_TOKENS);
    }

    /**
     * Resolve the base URL for the Cloudflare AI Gateway Anthropic endpoint.
     *
     * @param accountId Cloudflare account ID
     * @param gatewayId Gateway ID
     * @return URL string or empty if invalid
     */
    public static String resolveBaseUrl(String accountId, String gatewayId) {
        String acct = accountId != null ? accountId.trim() : "";
        String gw = gatewayId != null ? gatewayId.trim() : "";
        if (acct.isEmpty() || gw.isEmpty())
            return "";
        return "https://gateway.ai.cloudflare.com/v1/" + acct + "/" + gw + "/anthropic";
    }
}

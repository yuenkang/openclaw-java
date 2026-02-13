package com.openclaw.channel;

import lombok.Data;

/**
 * Reply prefix context for agent responses.
 * Corresponds to TypeScript's channels/reply-prefix.ts.
 */
public final class ReplyPrefix {

    private ReplyPrefix() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    public static class PrefixContext {
        private String identityName;
        private String provider;
        private String model;
        private String modelFull;
        private String thinkingLevel;
    }

    @Data
    public static class ReplyPrefixBundle {
        private PrefixContext prefixContext;
        private String responsePrefix;
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Create a reply prefix context bundle.
     *
     * @param identityName   the agent's display name
     * @param responsePrefix the configured response prefix template
     * @return a bundle containing the mutable prefix context and initial prefix
     */
    public static ReplyPrefixBundle create(String identityName, String responsePrefix) {
        PrefixContext ctx = new PrefixContext();
        ctx.setIdentityName(identityName);

        ReplyPrefixBundle bundle = new ReplyPrefixBundle();
        bundle.setPrefixContext(ctx);
        bundle.setResponsePrefix(responsePrefix);
        return bundle;
    }

    /**
     * Update the prefix context after model selection.
     */
    public static void onModelSelected(PrefixContext ctx,
            String provider,
            String model,
            String thinkLevel) {
        ctx.setProvider(provider);
        ctx.setModel(extractShortModelName(model));
        ctx.setModelFull(provider + "/" + model);
        ctx.setThinkingLevel(thinkLevel != null ? thinkLevel : "off");
    }

    /**
     * Extract a short model name by removing provider prefixes.
     */
    public static String extractShortModelName(String model) {
        if (model == null)
            return null;
        int slashIndex = model.lastIndexOf('/');
        return slashIndex >= 0 ? model.substring(slashIndex + 1) : model;
    }
}

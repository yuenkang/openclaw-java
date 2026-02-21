package com.openclaw.autoreply.reply;

import java.util.regex.Pattern;

/**
 * Response prefix template interpolation and short model name extraction.
 * Supports template variables like {model}, {provider}, {thinkingLevel}.
 * Mirrors {@code auto-reply/reply/response-prefix-template.ts}.
 */
public final class ResponsePrefixTemplate {

    private ResponsePrefixTemplate() {
    }

    /** Context for template variable resolution. */
    public record ResponsePrefixContext(
            String model,
            String modelFull,
            String provider,
            String thinkingLevel,
            String identityName) {
    }

    private static final Pattern TEMPLATE_VAR_RE = Pattern.compile("\\{([a-zA-Z][a-zA-Z0-9.]*)\\}");

    /**
     * Interpolate template variables in a response prefix string.
     * Unresolved variables remain as literal text.
     */
    public static String resolveResponsePrefixTemplate(String template, ResponsePrefixContext context) {
        if (template == null || template.isEmpty())
            return null;
        return TEMPLATE_VAR_RE.matcher(template).replaceAll(mr -> {
            String varName = mr.group(1).toLowerCase();
            return switch (varName) {
                case "model" -> context.model() != null ? context.model() : mr.group(0);
                case "modelfull" -> context.modelFull() != null ? context.modelFull() : mr.group(0);
                case "provider" -> context.provider() != null ? context.provider() : mr.group(0);
                case "thinkinglevel", "think" ->
                    context.thinkingLevel() != null ? context.thinkingLevel() : mr.group(0);
                case "identity.name", "identityname" ->
                    context.identityName() != null ? context.identityName() : mr.group(0);
                default -> mr.group(0);
            };
        });
    }

    /**
     * Extract short model name from a full model string.
     * Strips provider prefix, date suffixes, and -latest.
     */
    public static String extractShortModelName(String fullModel) {
        if (fullModel == null || fullModel.isEmpty())
            return fullModel;
        int slash = fullModel.lastIndexOf('/');
        String modelPart = slash >= 0 ? fullModel.substring(slash + 1) : fullModel;
        return modelPart.replaceAll("-\\d{8}$", "").replaceAll("-latest$", "");
    }

    /** Check if a template string contains any template variables. */
    public static boolean hasTemplateVariables(String template) {
        if (template == null || template.isEmpty())
            return false;
        return TEMPLATE_VAR_RE.matcher(template).find();
    }
}

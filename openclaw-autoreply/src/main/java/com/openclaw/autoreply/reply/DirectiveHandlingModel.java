package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Model directive handling — /model status, /model info, and model selection
 * resolution from inline directives.
 * Mirrors {@code auto-reply/reply/directive-handling.model.ts}.
 */
public final class DirectiveHandlingModel {

    private static final Logger log = LoggerFactory.getLogger(DirectiveHandlingModel.class);

    private DirectiveHandlingModel() {
    }

    /** Model picker catalog entry. */
    public record ModelPickerCatalogEntry(
            String provider,
            String id,
            String name) {
    }

    /** Model selection result from a directive. */
    public record ModelSelectionResult(
            ModelSelection modelSelection,
            String profileOverride,
            String errorText) {
    }

    /** Resolved model selection. */
    public record ModelSelection(
            String provider,
            String model,
            boolean isDefault,
            String alias) {
    }

    /**
     * Handle /model directive info commands (status, list, summary).
     * Returns reply payload or null if the directive is not an info command.
     */
    public static CompletableFuture<Map<String, Object>> maybeHandleModelDirectiveInfo(
            DirectiveHandlingParse.InlineDirectives directives,
            Map<String, Object> cfg,
            String provider,
            String model,
            String defaultProvider,
            String defaultModel,
            String surface) {

        if (!directives.hasModelDirective()) {
            return CompletableFuture.completedFuture(null);
        }

        String rawDirective = directives.rawModelDirective() != null
                ? directives.rawModelDirective().trim()
                : null;
        String directive = rawDirective != null ? rawDirective.toLowerCase() : null;

        boolean wantsStatus = "status".equals(directive);
        boolean wantsSummary = rawDirective == null || rawDirective.isEmpty();
        boolean wantsLegacyList = "list".equals(directive);

        if (!wantsSummary && !wantsStatus && !wantsLegacyList) {
            return CompletableFuture.completedFuture(null);
        }

        String current = provider + "/" + model;

        if (wantsSummary) {
            String text = String.join("\n",
                    "Current: " + current,
                    "",
                    "Switch: /model <provider/model>",
                    "Browse: /models (providers) or /models <provider> (models)",
                    "More: /model status");
            return CompletableFuture.completedFuture(Map.of("text", text));
        }

        if (wantsLegacyList) {
            // Delegate to CommandsModels
            return CompletableFuture.completedFuture(
                    Map.of("text", "Use /models to browse available models."));
        }

        // wantsStatus
        String defaultLabel = defaultProvider + "/" + defaultModel;
        String text = String.join("\n",
                "Current: " + current,
                "Default: " + defaultLabel,
                "",
                "Switch: /model <provider/model>",
                "Browse: /models");
        return CompletableFuture.completedFuture(Map.of("text", text));
    }

    /**
     * Resolve model selection from an inline directive.
     *
     * @param directives       parsed directives
     * @param defaultProvider  default provider
     * @param defaultModel     default model
     * @param allowedModelKeys set of allowed provider/model keys
     * @param provider         current provider
     * @return model selection result
     */
    public static ModelSelectionResult resolveModelSelectionFromDirective(
            DirectiveHandlingParse.InlineDirectives directives,
            String defaultProvider,
            String defaultModel,
            Set<String> allowedModelKeys,
            String provider) {

        if (!directives.hasModelDirective() || directives.rawModelDirective() == null) {
            return new ModelSelectionResult(null, null, null);
        }

        String raw = directives.rawModelDirective().trim();

        // Numeric selection not supported
        if (raw.matches("^[0-9]+$")) {
            return new ModelSelectionResult(null, null,
                    "Numeric model selection is not supported in chat.\n\n"
                            + "Browse: /models or /models <provider>\n"
                            + "Switch: /model <provider/model>");
        }

        // Parse provider/model
        String resolvedProvider;
        String resolvedModel;
        int slash = raw.indexOf('/');
        if (slash > 0) {
            resolvedProvider = raw.substring(0, slash).trim().toLowerCase();
            resolvedModel = raw.substring(slash + 1).trim();
        } else {
            resolvedProvider = defaultProvider;
            resolvedModel = raw;
        }

        if (resolvedProvider.isEmpty() || resolvedModel.isEmpty()) {
            return new ModelSelectionResult(null, null, null);
        }

        String key = resolvedProvider + "/" + resolvedModel;
        if (!allowedModelKeys.isEmpty() && !allowedModelKeys.contains(key)) {
            return new ModelSelectionResult(null, null, null);
        }

        boolean isDefault = resolvedProvider.equals(defaultProvider) && resolvedModel.equals(defaultModel);
        ModelSelection selection = new ModelSelection(
                resolvedProvider, resolvedModel, isDefault, null);
        return new ModelSelectionResult(selection, null, null);
    }
}

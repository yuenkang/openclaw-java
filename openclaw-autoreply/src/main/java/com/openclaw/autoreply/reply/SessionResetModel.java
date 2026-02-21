package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;

/**
 * Session model reset — parse a model specifier from the body on /new or
 * /reset,
 * resolve it against the allowed model catalog, and apply the override.
 * Mirrors {@code auto-reply/reply/session-reset-model.ts}.
 */
public final class SessionResetModel {

    private static final Logger log = LoggerFactory.getLogger(SessionResetModel.class);

    private SessionResetModel() {
    }

    /** Result of model resolution from a reset body. */
    public record ResetModelResult(
            ModelDirectiveSelection selection,
            String cleanedBody) {
    }

    /** Model directive selection (provider + model + metadata). */
    public record ModelDirectiveSelection(
            String provider,
            String model,
            boolean isDefault,
            String alias) {
    }

    /**
     * Split body into tokens.
     */
    private static String[] splitBody(String body) {
        return body.split("\\s+");
    }

    /**
     * Build a selection from an explicit provider/model string.
     */
    public static ModelDirectiveSelection buildSelectionFromExplicit(
            String raw, String defaultProvider, String defaultModel,
            Set<String> allowedModelKeys) {

        if (raw == null || raw.isBlank())
            return null;

        String provider;
        String model;
        int slash = raw.indexOf('/');
        if (slash > 0) {
            provider = raw.substring(0, slash).trim().toLowerCase();
            model = raw.substring(slash + 1).trim();
        } else {
            provider = defaultProvider;
            model = raw.trim();
        }

        if (provider.isEmpty() || model.isEmpty())
            return null;

        String key = provider + "/" + model;
        if (!allowedModelKeys.isEmpty() && !allowedModelKeys.contains(key)) {
            return null;
        }

        boolean isDefault = provider.equals(defaultProvider) && model.equals(defaultModel);
        return new ModelDirectiveSelection(provider, model, isDefault, null);
    }

    /**
     * Apply a model override from a /new or /reset command body.
     *
     * @param resetTriggered   whether /new or /reset was detected
     * @param bodyStripped     the stripped body after /new or /reset
     * @param defaultProvider  config default provider
     * @param defaultModel     config default model
     * @param allowedModelKeys set of allowed provider/model keys
     * @param sessionEntry     current session entry (nullable, mutable)
     * @return reset model result
     */
    public static ResetModelResult applyResetModelOverride(
            boolean resetTriggered,
            String bodyStripped,
            String defaultProvider,
            String defaultModel,
            Set<String> allowedModelKeys,
            Map<String, Object> sessionEntry) {

        if (!resetTriggered)
            return new ResetModelResult(null, null);

        String rawBody = bodyStripped != null ? bodyStripped.trim() : null;
        if (rawBody == null || rawBody.isEmpty())
            return new ResetModelResult(null, null);

        String[] tokens = splitBody(rawBody);
        if (tokens.length == 0 || tokens[0].isEmpty())
            return new ResetModelResult(null, null);

        String first = tokens[0];
        String second = tokens.length > 1 ? tokens[1] : null;

        // Providers set from allowed keys
        Set<String> providers = new java.util.HashSet<>();
        for (String key : allowedModelKeys) {
            int slash = key.indexOf('/');
            if (slash > 0)
                providers.add(key.substring(0, slash).toLowerCase());
        }

        ModelDirectiveSelection selection = null;
        int consumed = 0;

        // Try composite: "provider model"
        if (providers.contains(first.toLowerCase()) && second != null) {
            String composite = first.toLowerCase() + "/" + second;
            selection = buildSelectionFromExplicit(
                    composite, defaultProvider, defaultModel, allowedModelKeys);
            if (selection != null)
                consumed = 2;
        }

        // Try explicit: "provider/model" or just "model"
        if (selection == null) {
            selection = buildSelectionFromExplicit(
                    first, defaultProvider, defaultModel, allowedModelKeys);
            if (selection != null)
                consumed = 1;
        }

        if (selection == null)
            return new ResetModelResult(null, null);

        // Build cleaned body from remaining tokens
        StringBuilder sb = new StringBuilder();
        for (int i = consumed; i < tokens.length; i++) {
            if (sb.length() > 0)
                sb.append(' ');
            sb.append(tokens[i]);
        }
        String cleanedBody = sb.toString().trim();

        // Apply to session entry
        if (sessionEntry != null) {
            if (selection.isDefault()) {
                sessionEntry.remove("providerOverride");
                sessionEntry.remove("modelOverride");
            } else {
                sessionEntry.put("providerOverride", selection.provider());
                sessionEntry.put("modelOverride", selection.model());
            }
            log.debug("Reset model override: {} → {}/{}",
                    first, selection.provider(), selection.model());
        }

        return new ResetModelResult(selection, cleanedBody);
    }
}

package com.openclaw.common.config;

import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Config validation — structural and semantic checks beyond JSON schema.
 * Replaces Zod validation with imperative checks for the Java runtime.
 * Corresponds to TypeScript's validation.ts.
 */
@Slf4j
public final class ConfigValidation {

    private ConfigValidation() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    public enum Severity {
        ERROR, WARNING
    }

    public record ValidationIssue(
            String path,
            String message,
            Severity severity) {
    }

    public record ValidationResult(
            boolean ok,
            OpenClawConfig config,
            List<ValidationIssue> issues,
            List<ValidationIssue> warnings) {
        public static ValidationResult success(OpenClawConfig config, List<ValidationIssue> warnings) {
            return new ValidationResult(true, config, List.of(), warnings);
        }

        public static ValidationResult failure(List<ValidationIssue> issues, List<ValidationIssue> warnings) {
            return new ValidationResult(false, null, issues, warnings);
        }
    }

    // =========================================================================
    // Constants
    // =========================================================================

    private static final Pattern AVATAR_HTTP_RE = Pattern.compile("^https?://", Pattern.CASE_INSENSITIVE);
    private static final Pattern AVATAR_DATA_RE = Pattern.compile("^data:", Pattern.CASE_INSENSITIVE);
    private static final Pattern AVATAR_SCHEME_RE = Pattern.compile("^[a-z][a-z0-9+.-]*:", Pattern.CASE_INSENSITIVE);

    private static final Set<String> KNOWN_PROVIDERS = Set.of(
            "anthropic", "openai", "gemini", "deepseek", "openrouter",
            "ai-gateway", "bedrock", "vertex", "ollama", "azure",
            "custom", "xai", "mistral", "groq", "cerebras", "fireworks",
            "together", "cohere", "sambanova");

    private static final Set<String> KNOWN_BIND_MODES = Set.of(
            "loopback", "wildcard", "lan");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Validate a config object after deserialization.
     */
    public static ValidationResult validate(OpenClawConfig config) {
        List<ValidationIssue> issues = new ArrayList<>();
        List<ValidationIssue> warnings = new ArrayList<>();

        validateGateway(config, issues, warnings);
        validateModels(config, issues, warnings);
        validateAgents(config, issues, warnings);
        validateIdentityAvatar(config, warnings);

        if (!issues.isEmpty()) {
            return ValidationResult.failure(issues, warnings);
        }
        return ValidationResult.success(config, warnings);
    }

    // =========================================================================
    // Gateway validation
    // =========================================================================

    private static void validateGateway(OpenClawConfig config,
            List<ValidationIssue> issues, List<ValidationIssue> warnings) {
        var gw = config.getGateway();
        if (gw == null)
            return;

        if (gw.getPort() != 0 && (gw.getPort() < 1 || gw.getPort() > 65535)) {
            issues.add(new ValidationIssue("gateway.port",
                    "Port must be 1-65535, got " + gw.getPort(), Severity.ERROR));
        }

        String bind = gw.getBind();
        if (bind != null && !KNOWN_BIND_MODES.contains(bind)) {
            warnings.add(new ValidationIssue("gateway.bind",
                    "Unknown bind mode: \"" + bind + "\"; expected one of: " + KNOWN_BIND_MODES,
                    Severity.WARNING));
        }
    }

    // =========================================================================
    // Models validation
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static void validateModels(OpenClawConfig config,
            List<ValidationIssue> issues, List<ValidationIssue> warnings) {
        var models = config.getModels();
        if (models == null)
            return;

        // Validate providers and their model definitions
        var providers = models.getProviders();
        if (providers != null) {
            for (var entry : providers.entrySet()) {
                String id = entry.getKey();
                if (!KNOWN_PROVIDERS.contains(id) && !id.startsWith("custom-")) {
                    warnings.add(new ValidationIssue("models.providers." + id,
                            "Unknown provider: \"" + id + "\"", Severity.WARNING));
                }
                var providerConfig = entry.getValue();
                if (providerConfig != null && providerConfig.getModels() != null) {
                    var modelList = providerConfig.getModels();
                    for (int i = 0; i < modelList.size(); i++) {
                        var def = modelList.get(i);
                        if (def.getId() == null || def.getId().isBlank()) {
                            issues.add(new ValidationIssue(
                                    "models.providers." + id + ".models[" + i + "].id",
                                    "Model definition must have an id", Severity.ERROR));
                        }
                    }
                }
            }
        }
    }

    // =========================================================================
    // Agents validation
    // =========================================================================

    private static void validateAgents(OpenClawConfig config,
            List<ValidationIssue> issues, List<ValidationIssue> warnings) {
        var agents = config.getAgents();
        if (agents == null)
            return;

        var entries = agents.getEntries();
        for (int i = 0; i < entries.size(); i++) {
            var agent = entries.get(i);
            if (agent.getId() == null || agent.getId().isBlank()) {
                issues.add(new ValidationIssue("agents.list[" + i + "].id",
                        "Agent must have an id", Severity.ERROR));
            }
        }

        var defaults = agents.getDefaults();
        if (defaults != null) {
            Integer maxConcurrent = defaults.getMaxConcurrent();
            if (maxConcurrent != null && maxConcurrent < 1) {
                warnings.add(new ValidationIssue("agents.defaults.maxConcurrent",
                        "maxConcurrent should be >= 1, got " + maxConcurrent, Severity.WARNING));
            }
        }
    }

    // =========================================================================
    // Identity avatar validation
    // =========================================================================

    private static void validateIdentityAvatar(OpenClawConfig config, List<ValidationIssue> warnings) {
        var ui = config.getUi();
        if (ui == null)
            return;
        var assistant = ui.getAssistant();
        if (assistant == null)
            return;
        String avatar = assistant.getAvatar();
        if (avatar == null || avatar.isBlank())
            return;

        String trimmed = avatar.trim();
        // URLs and data URIs are always valid
        if (AVATAR_HTTP_RE.matcher(trimmed).find() || AVATAR_DATA_RE.matcher(trimmed).find())
            return;
        // Other scheme-like values are suspicious
        if (AVATAR_SCHEME_RE.matcher(trimmed).find()) {
            warnings.add(new ValidationIssue("ui.assistant.avatar",
                    "Avatar value looks like an unsupported URL scheme: \"" + trimmed + "\"",
                    Severity.WARNING));
        }
    }
}

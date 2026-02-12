package com.openclaw.agent.tools.policy;

import com.openclaw.agent.tools.policy.ToolPolicyTypes.ToolPolicy;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Tool creation orchestrator — resolves policies, creates tool list, applies
 * wrappers.
 * Corresponds to TypeScript pi-tools.ts (createOpenClawCodingTools).
 *
 * This is a simplified Java port focusing on policy resolution and tool
 * filtering.
 * Tool creation itself is handled by the Java coding agent framework.
 */
@Slf4j
public final class ToolCreator {

    private ToolCreator() {
    }

    /**
     * Options for creating the coding tool set.
     */
    public static class CreateToolsOptions {
        public String sessionKey;
        public String agentDir;
        public String workspaceDir;
        public Map<String, Object> config;
        public String modelProvider;
        public String modelId;
        public String messageProvider;
        public String agentAccountId;
        public String groupId;
        public String groupChannel;
        public String groupSpace;
        public String spawnedBy;
        public String senderId;
        public boolean senderIsOwner;
        public boolean modelHasVision;
        public boolean disableMessageTool;
        public boolean requireExplicitMessageTarget;
        public String verboseLevel;
    }

    /**
     * Check if model provider is OpenAI or OpenAI-Codex.
     */
    public static boolean isOpenAIProvider(String provider) {
        if (provider == null)
            return false;
        String normalized = provider.trim().toLowerCase();
        return "openai".equals(normalized) || "openai-codex".equals(normalized);
    }

    /**
     * Check if apply_patch is allowed for the current model.
     */
    public static boolean isApplyPatchAllowedForModel(String modelProvider, String modelId, List<String> allowModels) {
        if (allowModels == null || allowModels.isEmpty())
            return true;
        if (modelId == null || modelId.isBlank())
            return false;
        String normalizedModelId = modelId.trim().toLowerCase();
        String provider = modelProvider != null ? modelProvider.trim().toLowerCase() : null;
        String normalizedFull = provider != null && !normalizedModelId.contains("/")
                ? provider + "/" + normalizedModelId
                : normalizedModelId;
        return allowModels.stream().anyMatch(entry -> {
            String normalized = entry.trim().toLowerCase();
            if (normalized.isEmpty())
                return false;
            return normalized.equals(normalizedModelId) || normalized.equals(normalizedFull);
        });
    }

    /**
     * Resolve and filter a list of tool names using the full policy chain.
     *
     * @param availableToolNames all available tool names before policy filtering
     * @param options            creation options including config, session, and
     *                           model info
     * @return filtered list of tool names after applying all policies
     */
    public static List<String> resolveAndFilterToolNames(
            List<String> availableToolNames, CreateToolsOptions options) {

        var effective = ToolPolicyResolver.resolveEffectiveToolPolicy(
                options.config, options.sessionKey, options.modelProvider, options.modelId);

        // Resolve profile policies
        ToolPolicy profilePolicy = ToolPolicyUtils.resolveToolProfilePolicy(effective.profile());
        ToolPolicy providerProfilePolicy = ToolPolicyUtils.resolveToolProfilePolicy(effective.providerProfile());

        // Merge alsoAllow
        profilePolicy = ToolPolicyResolver.mergeAlsoAllow(profilePolicy, effective.profileAlsoAllow());
        providerProfilePolicy = ToolPolicyResolver.mergeAlsoAllow(providerProfilePolicy,
                effective.providerProfileAlsoAllow());

        // Apply policies in sequence
        List<String> filtered = availableToolNames;
        filtered = applyPolicyFilter(filtered, profilePolicy);
        filtered = applyPolicyFilter(filtered, providerProfilePolicy);
        filtered = applyPolicyFilter(filtered, effective.globalPolicy());
        filtered = applyPolicyFilter(filtered, effective.globalProviderPolicy());
        filtered = applyPolicyFilter(filtered, effective.agentPolicy());
        filtered = applyPolicyFilter(filtered, effective.agentProviderPolicy());

        // Owner-only filtering
        if (!options.senderIsOwner) {
            filtered = filtered.stream()
                    .filter(name -> !ToolPolicyUtils.isOwnerOnlyToolName(name))
                    .toList();
        }

        return filtered;
    }

    private static List<String> applyPolicyFilter(List<String> toolNames, ToolPolicy policy) {
        if (policy == null)
            return toolNames;
        return ToolPolicyMatcher.filterToolNamesByPolicy(toolNames, policy);
    }
}

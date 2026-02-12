package com.openclaw.agent.tools.policy;

import com.openclaw.agent.tools.policy.ToolPolicyTypes.ToolPolicy;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Effective tool policy resolution — combines global, agent, provider, and
 * group policies.
 * Corresponds to TypeScript pi-tools.policy.ts (resolve* functions).
 */
@Slf4j
public final class ToolPolicyResolver {

    private ToolPolicyResolver() {
    }

    /**
     * Result of resolving the effective tool policy chain.
     */
    public record EffectiveToolPolicy(
            String agentId,
            ToolPolicy globalPolicy,
            ToolPolicy globalProviderPolicy,
            ToolPolicy agentPolicy,
            ToolPolicy agentProviderPolicy,
            String profile,
            String providerProfile,
            List<String> profileAlsoAllow,
            List<String> providerProfileAlsoAllow) {
    }

    /**
     * Resolve the full policy chain for a session.
     */
    public static EffectiveToolPolicy resolveEffectiveToolPolicy(
            Map<String, Object> config,
            String sessionKey,
            String modelProvider,
            String modelId) {
        // Extract agentId from sessionKey
        String agentId = resolveAgentIdFromSessionKey(sessionKey);
        Map<String, Object> agentTools = resolveAgentToolsConfig(config, agentId);
        Map<String, Object> globalTools = getMapField(config, "tools");

        String profile = getStringField(agentTools, "profile");
        if (profile == null)
            profile = getStringField(globalTools, "profile");

        ToolPolicy globalPolicy = pickToolPolicy(globalTools);
        ToolPolicy agentPolicy = pickToolPolicy(agentTools);

        Map<String, Object> globalByProvider = getMapField(globalTools, "byProvider");
        Map<String, Object> agentByProvider = getMapField(agentTools, "byProvider");
        Map<String, Object> providerPolicyConfig = resolveProviderToolPolicy(globalByProvider, modelProvider, modelId);
        Map<String, Object> agentProviderPolicyConfig = resolveProviderToolPolicy(agentByProvider, modelProvider,
                modelId);

        ToolPolicy globalProviderPolicy = pickToolPolicy(providerPolicyConfig);
        ToolPolicy agentProviderPolicy = pickToolPolicy(agentProviderPolicyConfig);

        String providerProfile = getStringField(agentProviderPolicyConfig, "profile");
        if (providerProfile == null)
            providerProfile = getStringField(providerPolicyConfig, "profile");

        List<String> profileAlsoAllow = getStringListField(agentTools, "alsoAllow");
        if (profileAlsoAllow == null)
            profileAlsoAllow = getStringListField(globalTools, "alsoAllow");

        List<String> providerProfileAlsoAllow = getStringListField(agentProviderPolicyConfig, "alsoAllow");
        if (providerProfileAlsoAllow == null)
            providerProfileAlsoAllow = getStringListField(providerPolicyConfig, "alsoAllow");

        return new EffectiveToolPolicy(
                agentId, globalPolicy, globalProviderPolicy,
                agentPolicy, agentProviderPolicy,
                profile, providerProfile,
                profileAlsoAllow, providerProfileAlsoAllow);
    }

    /**
     * Merge alsoAllow into a profile policy.
     */
    public static ToolPolicy mergeAlsoAllow(ToolPolicy policy, List<String> alsoAllow) {
        if (policy == null || policy.allow() == null
                || alsoAllow == null || alsoAllow.isEmpty()) {
            return policy;
        }
        Set<String> merged = new LinkedHashSet<>(policy.allow());
        merged.addAll(alsoAllow);
        return new ToolPolicy(new ArrayList<>(merged), policy.deny());
    }

    // ── Helpers ──────────────────────────────────────────────────────

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getMapField(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object val = map.get(key);
        return val instanceof Map<?, ?> m ? (Map<String, Object>) m : null;
    }

    private static String getStringField(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object val = map.get(key);
        return val instanceof String s && !s.isBlank() ? s.trim() : null;
    }

    @SuppressWarnings("unchecked")
    private static List<String> getStringListField(Map<String, Object> map, String key) {
        if (map == null)
            return null;
        Object val = map.get(key);
        if (!(val instanceof List<?> l))
            return null;
        List<String> result = new ArrayList<>();
        for (Object item : l) {
            if (item instanceof String s)
                result.add(s);
        }
        return result.isEmpty() ? null : result;
    }

    static ToolPolicy pickToolPolicy(Map<String, Object> config) {
        if (config == null)
            return null;
        List<String> allow = getStringListField(config, "allow");
        List<String> alsoAllow = getStringListField(config, "alsoAllow");
        if (alsoAllow != null && !alsoAllow.isEmpty()) {
            if (allow == null || allow.isEmpty()) {
                List<String> star = new ArrayList<>();
                star.add("*");
                star.addAll(alsoAllow);
                allow = star.stream().distinct().toList();
            } else {
                Set<String> merged = new LinkedHashSet<>(allow);
                merged.addAll(alsoAllow);
                allow = new ArrayList<>(merged);
            }
        }
        List<String> deny = getStringListField(config, "deny");
        if (allow == null && deny == null)
            return null;
        return new ToolPolicy(allow, deny);
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> resolveProviderToolPolicy(
            Map<String, Object> byProvider, String modelProvider, String modelId) {
        if (byProvider == null || modelProvider == null || modelProvider.isBlank())
            return null;
        String normalizedProvider = modelProvider.trim().toLowerCase();
        String rawModelId = modelId != null ? modelId.trim().toLowerCase() : null;
        String fullModelId = rawModelId != null && !rawModelId.contains("/")
                ? normalizedProvider + "/" + rawModelId
                : rawModelId;

        List<String> candidates = new ArrayList<>();
        if (fullModelId != null)
            candidates.add(fullModelId);
        candidates.add(normalizedProvider);

        for (String key : candidates) {
            for (Map.Entry<String, Object> entry : byProvider.entrySet()) {
                if (entry.getKey().trim().toLowerCase().equals(key)
                        && entry.getValue() instanceof Map<?, ?> m) {
                    return (Map<String, Object>) m;
                }
            }
        }
        return null;
    }

    static String resolveAgentIdFromSessionKey(String sessionKey) {
        if (sessionKey == null || sessionKey.isBlank())
            return null;
        String trimmed = sessionKey.trim();
        if (!trimmed.startsWith("agent:"))
            return null;
        String[] parts = trimmed.split(":", 3);
        return parts.length >= 2 && !parts[1].isBlank() ? parts[1].trim() : null;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveAgentToolsConfig(Map<String, Object> config, String agentId) {
        if (config == null || agentId == null)
            return null;
        Map<String, Object> agents = getMapField(config, "agents");
        if (agents == null)
            return null;
        Map<String, Object> agentConfig = getMapField(agents, agentId);
        if (agentConfig == null) {
            // Try entries list
            Object entries = agents.get("entries");
            if (entries instanceof List<?> l) {
                for (Object item : l) {
                    if (item instanceof Map<?, ?> m) {
                        Map<String, Object> entry = (Map<String, Object>) m;
                        if (agentId.equals(entry.get("id"))) {
                            agentConfig = entry;
                            break;
                        }
                    }
                }
            }
        }
        return agentConfig != null ? getMapField(agentConfig, "tools") : null;
    }
}

package com.openclaw.gateway.routing;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Resolves which agent should handle a given routing context.
 * Corresponds to TypeScript's resolve-route.ts.
 *
 * <p>
 * Matching priority order: peer → guild → team → account → channel → default.
 * </p>
 */
@Slf4j
public class RouteResolver {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RouteContext {
        private String channelId;
        private String accountId;
        private String peerId;
        private String guildId;
        private String teamId;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ResolvedRoute {
        private String agentId;
        private String channelId;
        private String accountId;
        private String matchType;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RouteBinding {
        /** peer | guild | team | account | channel | default */
        private String type;
        private String key;
        private String agentId;
    }

    private final List<RouteBinding> bindings = new ArrayList<>();

    /**
     * Add a route binding.
     */
    public void addBinding(RouteBinding binding) {
        bindings.add(binding);
    }

    /**
     * Load bindings from config.
     */
    public void loadFromConfig(OpenClawConfig config) {
        bindings.clear();

        if (config.getAgents() == null || config.getAgents().getList() == null) {
            return;
        }

        // Build default binding for first agent
        List<OpenClawConfig.AgentEntry> agents = config.getAgents().getList();
        if (!agents.isEmpty()) {
            addBinding(RouteBinding.builder()
                    .type("default")
                    .key("*")
                    .agentId(agents.get(0).getId())
                    .build());
        }
    }

    /**
     * Resolve the route for a given context.
     * Priority: peer → guild → team → account → channel → default.
     */
    public Optional<ResolvedRoute> resolve(RouteContext context) {
        // Priority ordered keys
        List<Map.Entry<String, String>> candidates = new ArrayList<>();

        if (context.getPeerId() != null) {
            candidates.add(Map.entry("peer", context.getPeerId()));
        }
        if (context.getGuildId() != null) {
            candidates.add(Map.entry("guild", context.getGuildId()));
        }
        if (context.getTeamId() != null) {
            candidates.add(Map.entry("team", context.getTeamId()));
        }
        if (context.getAccountId() != null) {
            candidates.add(Map.entry("account", context.getAccountId()));
        }
        if (context.getChannelId() != null) {
            candidates.add(Map.entry("channel", context.getChannelId()));
        }
        candidates.add(Map.entry("default", "*"));

        for (Map.Entry<String, String> candidate : candidates) {
            String type = candidate.getKey();
            String key = candidate.getValue();

            Optional<RouteBinding> match = bindings.stream()
                    .filter(b -> type.equals(b.getType()) && key.equals(b.getKey()))
                    .findFirst();

            if (match.isPresent()) {
                RouteBinding binding = match.get();
                return Optional.of(ResolvedRoute.builder()
                        .agentId(binding.getAgentId())
                        .channelId(context.getChannelId())
                        .accountId(context.getAccountId())
                        .matchType(type)
                        .build());
            }
        }

        // Fallback to default binding
        return bindings.stream()
                .filter(b -> "default".equals(b.getType()))
                .findFirst()
                .map(binding -> ResolvedRoute.builder()
                        .agentId(binding.getAgentId())
                        .channelId(context.getChannelId())
                        .accountId(context.getAccountId())
                        .matchType("default")
                        .build());
    }
}

package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Top-level getReplyFromConfig — orchestrates the full reply pipeline:
 * load config, resolve agent/session, finalize inbound context, apply
 * media/link understanding, command authorization, session init,
 * directive resolution, inline actions, sandbox staging, and finally
 * run the prepared reply.
 * Mirrors {@code auto-reply/reply/get-reply.ts}.
 */
public final class GetReply {

    private static final Logger log = LoggerFactory.getLogger(GetReply.class);

    private GetReply() {
    }

    // --- Skill filter merge ---

    /**
     * Merge channel-level and agent-level skill filters.
     *
     * <ul>
     * <li>Both null → null (no filtering)</li>
     * <li>One null → use the other</li>
     * <li>Both present → intersection</li>
     * <li>Either empty list → empty (block all)</li>
     * </ul>
     */
    static List<String> mergeSkillFilters(List<String> channelFilter, List<String> agentFilter) {
        List<String> channel = normalizeFilterList(channelFilter);
        List<String> agent = normalizeFilterList(agentFilter);
        if (channel == null && agent == null)
            return null;
        if (channel == null)
            return agent;
        if (agent == null)
            return channel;
        if (channel.isEmpty() || agent.isEmpty())
            return List.of();
        Set<String> agentSet = new HashSet<>(agent);
        return channel.stream().filter(agentSet::contains).toList();
    }

    private static List<String> normalizeFilterList(List<String> list) {
        if (list == null)
            return null;
        return list.stream()
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .toList();
    }

    /**
     * Main entry point — get reply from config for an inbound message.
     *
     * @param ctx            inbound message context
     * @param opts           reply options (nullable)
     * @param configOverride config override (nullable)
     * @return reply payloads
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<List<Map<String, Object>>> getReplyFromConfig(
            Map<String, Object> ctx,
            Map<String, Object> opts,
            Map<String, Object> configOverride) {

        Map<String, Object> cfg = configOverride != null ? configOverride : loadConfig();

        // 1. Resolve agent identity
        String targetSessionKey = "native".equals(ctx.get("CommandSource"))
                ? trimOrNull((String) ctx.get("CommandTargetSessionKey"))
                : null;
        String agentSessionKey = targetSessionKey != null ? targetSessionKey
                : (String) ctx.get("SessionKey");
        String agentId = resolveSessionAgentId(agentSessionKey, cfg);

        // 2. Merge skill filters
        List<String> channelSkillFilter = opts != null
                ? (List<String>) opts.get("skillFilter")
                : null;
        List<String> agentSkillFilter = resolveAgentSkillsFilter(cfg, agentId);
        List<String> mergedSkillFilter = mergeSkillFilters(channelSkillFilter, agentSkillFilter);
        Map<String, Object> resolvedOpts = opts;
        if (mergedSkillFilter != null) {
            resolvedOpts = new LinkedHashMap<>(opts != null ? opts : Map.of());
            resolvedOpts.put("skillFilter", mergedSkillFilter);
        }

        // 3. Resolve default model
        Map<String, Object> agentCfg = getAgentDefaults(cfg);
        // resolveDefaultModel deferred — use placeholders
        String defaultProvider = "openai";
        String defaultModel = "gpt-4o";
        String provider = defaultProvider;
        String model = defaultModel;

        // 4. Heartbeat model override
        if (opts != null && Boolean.TRUE.equals(opts.get("isHeartbeat"))) {
            String heartbeatRaw = agentCfg != null && agentCfg.containsKey("heartbeat")
                    ? resolveHeartbeatModel(agentCfg)
                    : null;
            if (heartbeatRaw != null && !heartbeatRaw.isEmpty()) {
                // resolveModelRefFromString deferred
                log.debug("Heartbeat model override: {}", heartbeatRaw);
            }
        }

        // 5. Ensure workspace
        log.debug("Ensuring agent workspace for agent {}", agentId);

        // 6. Finalize inbound context
        Map<String, Object> finalized = InboundContext.finalizeInboundContext(ctx);

        // 7. Media/link understanding (deferred)

        // 8. Command authorization
        log.debug("Resolving command authorization");

        // 9. Session init
        log.debug("Initializing session state for {}", agentSessionKey);

        // 10. Resolve directives, inline actions (deferred)

        // 11. Stage sandbox media (deferred)

        // 12. Run prepared reply (deferred)

        log.info("getReplyFromConfig: full pipeline integration deferred");
        return CompletableFuture.completedFuture(List.of());
    }

    // --- Internal helpers ---

    @SuppressWarnings("unchecked")
    private static Map<String, Object> loadConfig() {
        return Map.of();
    }

    private static String resolveSessionAgentId(String sessionKey, Map<String, Object> cfg) {
        // Full agent scope resolution deferred
        return null;
    }

    @SuppressWarnings("unchecked")
    private static List<String> resolveAgentSkillsFilter(Map<String, Object> cfg, String agentId) {
        return null;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getAgentDefaults(Map<String, Object> cfg) {
        Object agents = cfg.get("agents");
        if (agents instanceof Map<?, ?> m) {
            Object defaults = m.get("defaults");
            if (defaults instanceof Map<?, ?> d) {
                return (Map<String, Object>) d;
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static String resolveHeartbeatModel(Map<String, Object> agentCfg) {
        Object heartbeat = agentCfg.get("heartbeat");
        if (heartbeat instanceof Map<?, ?> m) {
            Object model = m.get("model");
            return model != null ? model.toString().trim() : null;
        }
        return null;
    }

    private static String trimOrNull(String s) {
        if (s == null)
            return null;
        String trimmed = s.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }
}

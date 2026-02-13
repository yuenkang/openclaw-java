package com.openclaw.gateway.session;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Session key resolution, classification, listing, and title derivation.
 * <p>
 * Corresponds to TypeScript's {@code session-utils.ts} (26 functions, 725
 * lines).
 */
@Slf4j
public class SessionUtils {

    private static final int DERIVED_TITLE_MAX_LEN = 60;
    private static final String DEFAULT_AGENT_ID = "default";
    private static final Pattern AGENT_KEY_RE = Pattern.compile("^agent:([^:]+):(.+)$");
    private static final Pattern WHITESPACE_COLLAPSE = Pattern.compile("\\s+");
    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd")
            .withZone(ZoneId.of("UTC"));

    private final ConfigService configService;

    public SessionUtils(ConfigService configService) {
        this.configService = configService;
    }

    // ─── Session key normalization ───────────────────────────────────────

    /**
     * Normalize an agent ID: lowercase, trim.
     */
    public static String normalizeAgentId(String agentId) {
        if (agentId == null || agentId.isBlank()) {
            return DEFAULT_AGENT_ID;
        }
        return agentId.trim().toLowerCase(Locale.ROOT);
    }

    /**
     * Parse an "agent:{agentId}:{rest}" key.
     *
     * @return [agentId, rest] or null if not matching
     */
    public static String[] parseAgentSessionKey(String key) {
        if (key == null)
            return null;
        var m = AGENT_KEY_RE.matcher(key);
        if (m.matches()) {
            return new String[] { m.group(1), m.group(2) };
        }
        return null;
    }

    /**
     * Canonicalize a session key for an agent.
     */
    public static String canonicalizeSessionKeyForAgent(String agentId, String key) {
        if ("global".equals(key) || "unknown".equals(key))
            return key;
        if (key.startsWith("agent:"))
            return key;
        return "agent:" + normalizeAgentId(agentId) + ":" + key;
    }

    /**
     * Resolve session store key: handles "main", agent-prefixed, and bare keys.
     */
    public String resolveSessionStoreKey(String sessionKey) {
        var cfg = configService.loadConfig();
        return resolveSessionStoreKey(cfg, sessionKey);
    }

    public static String resolveSessionStoreKey(OpenClawConfig cfg, String sessionKey) {
        String raw = sessionKey != null ? sessionKey.trim() : "";
        if (raw.isEmpty())
            return raw;
        if ("global".equals(raw) || "unknown".equals(raw))
            return raw;

        var parsed = parseAgentSessionKey(raw);
        if (parsed != null) {
            // Already has agent prefix
            return raw;
        }

        // Bare "main" → resolve to the configured main session key
        String mainKey = resolveMainKey(cfg);
        if ("main".equals(raw) || raw.equals(mainKey)) {
            return resolveMainSessionKey(cfg);
        }

        String agentId = resolveDefaultStoreAgentId(cfg);
        return canonicalizeSessionKeyForAgent(agentId, raw);
    }

    /**
     * Resolve the agent ID from a canonical session key.
     */
    public static String resolveSessionStoreAgentId(OpenClawConfig cfg, String canonicalKey) {
        if ("global".equals(canonicalKey) || "unknown".equals(canonicalKey)) {
            return resolveDefaultStoreAgentId(cfg);
        }
        var parsed = parseAgentSessionKey(canonicalKey);
        if (parsed != null && parsed[0] != null) {
            return normalizeAgentId(parsed[0]);
        }
        return resolveDefaultStoreAgentId(cfg);
    }

    public static String resolveDefaultStoreAgentId(OpenClawConfig cfg) {
        String defaultId = null;
        if (cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            // No explicit "defaultId" field in config; the first agent in list or "default"
        }
        var agentsList = cfg.getAgents() != null ? cfg.getAgents().getEntries() : List.<OpenClawConfig.AgentEntry>of();
        if (!agentsList.isEmpty() && agentsList.get(0).getId() != null) {
            defaultId = agentsList.get(0).getId();
        }
        return normalizeAgentId(defaultId != null ? defaultId : DEFAULT_AGENT_ID);
    }

    public static String resolveMainKey(OpenClawConfig cfg) {
        String mk = cfg.getSession() != null ? cfg.getSession().getMainKey() : null;
        return mk != null && !mk.isBlank() ? mk.trim() : "default";
    }

    public static String resolveMainSessionKey(OpenClawConfig cfg) {
        String defaultId = resolveDefaultStoreAgentId(cfg);
        String mainKey = resolveMainKey(cfg);
        return canonicalizeSessionKeyForAgent(defaultId, mainKey);
    }

    // ─── Session classification ─────────────────────────────────────────

    /**
     * Classify a session key into kind: global, unknown, group, or direct.
     */
    public static String classifySessionKey(String key, AcpSessionEntry entry) {
        if ("global".equals(key))
            return "global";
        if ("unknown".equals(key))
            return "unknown";
        if (entry != null) {
            String chatType = entry.chatType();
            if ("group".equals(chatType) || "channel".equals(chatType))
                return "group";
        }
        if (key != null && (key.contains(":group:") || key.contains(":channel:")))
            return "group";
        return "direct";
    }

    /**
     * Parse a group key pattern: "{channel}:{group|channel}:{id}".
     */
    public static GroupKeyParts parseGroupKey(String key) {
        var parsed = parseAgentSessionKey(key);
        String rawKey = parsed != null ? parsed[1] : key;
        if (rawKey == null)
            return null;
        String[] parts = rawKey.split(":");
        if (parts.length >= 3 && ("group".equals(parts[1]) || "channel".equals(parts[1]))) {
            String id = String.join(":", Arrays.copyOfRange(parts, 2, parts.length));
            return new GroupKeyParts(parts[0], parts[1], id);
        }
        return null;
    }

    public record GroupKeyParts(String channel, String kind, String id) {
    }

    // ─── Title derivation ───────────────────────────────────────────────

    /**
     * Derive a session title from entry metadata or first user message.
     */
    public static String deriveSessionTitle(AcpSessionEntry entry, String firstUserMessage) {
        if (entry == null)
            return null;
        if (entry.displayName() != null && !entry.displayName().isBlank()) {
            return entry.displayName().trim();
        }
        if (entry.subject() != null && !entry.subject().isBlank()) {
            return entry.subject().trim();
        }
        if (firstUserMessage != null && !firstUserMessage.isBlank()) {
            String normalized = WHITESPACE_COLLAPSE.matcher(firstUserMessage).replaceAll(" ").trim();
            return truncateTitle(normalized, DERIVED_TITLE_MAX_LEN);
        }
        if (entry.sessionId() != null) {
            return formatSessionIdPrefix(entry.sessionId(), entry.updatedAt());
        }
        return null;
    }

    static String truncateTitle(String text, int maxLen) {
        if (text.length() <= maxLen)
            return text;
        String cut = text.substring(0, maxLen - 1);
        int lastSpace = cut.lastIndexOf(' ');
        if (lastSpace > maxLen * 0.6) {
            return cut.substring(0, lastSpace) + "…";
        }
        return cut + "…";
    }

    static String formatSessionIdPrefix(String sessionId, Long updatedAt) {
        String prefix = sessionId.length() >= 8 ? sessionId.substring(0, 8) : sessionId;
        if (updatedAt != null && updatedAt > 0) {
            String date = DATE_FMT.format(Instant.ofEpochMilli(updatedAt));
            return prefix + " (" + date + ")";
        }
        return prefix;
    }

    // ─── Agent listing ──────────────────────────────────────────────────

    /**
     * List agents configured for the gateway.
     */
    public AgentsInfo listAgentsForGateway() {
        var cfg = configService.loadConfig();
        String defaultId = resolveDefaultStoreAgentId(cfg);
        String mainKey = resolveMainKey(cfg);
        String scope = cfg.getSession() != null && cfg.getSession().getScope() != null
                ? cfg.getSession().getScope()
                : "per-sender";

        List<Map<String, Object>> agents = new ArrayList<>();
        // Add the default agent at minimum
        Map<String, Object> defaultAgent = new LinkedHashMap<>();
        defaultAgent.put("id", defaultId);
        agents.add(defaultAgent);

        // Add configured agents
        var agentEntries = cfg.getAgents() != null ? cfg.getAgents().getEntries()
                : List.<OpenClawConfig.AgentEntry>of();
        if (!agentEntries.isEmpty()) {
            Set<String> seen = new HashSet<>();
            seen.add(defaultId);
            for (var entry : agentEntries) {
                if (entry == null || entry.getId() == null)
                    continue;
                String id = normalizeAgentId(entry.getId());
                if (id.isEmpty() || seen.contains(id))
                    continue;
                seen.add(id);
                Map<String, Object> agent = new LinkedHashMap<>();
                agent.put("id", id);
                if (entry.getName() != null && !entry.getName().isBlank()) {
                    agent.put("name", entry.getName().trim());
                }
                agents.add(agent);
            }
        }

        return new AgentsInfo(defaultId, mainKey, scope, agents);
    }

    public record AgentsInfo(
            String defaultId,
            String mainKey,
            String scope,
            List<Map<String, Object>> agents) {
    }

    // ─── Session store target resolution ────────────────────────────────

    /**
     * Resolve complete session store target from a key.
     */
    public SessionStoreTarget resolveGatewaySessionStoreTarget(String key) {
        var cfg = configService.loadConfig();
        String canonicalKey = resolveSessionStoreKey(cfg, key.trim());
        String agentId = resolveSessionStoreAgentId(cfg, canonicalKey);

        Set<String> storeKeys = new LinkedHashSet<>();
        storeKeys.add(canonicalKey);
        if (!key.trim().equals(canonicalKey)) {
            storeKeys.add(key.trim());
        }

        return new SessionStoreTarget(agentId, canonicalKey, new ArrayList<>(storeKeys));
    }

    public record SessionStoreTarget(
            String agentId,
            String canonicalKey,
            List<String> storeKeys) {
    }

    // ─── Session entry DTO ──────────────────────────────────────────────

    /**
     * Lightweight record representing a session entry from the persisted store.
     * Fields correspond to TypeScript's SessionEntry in config/sessions.ts.
     */
    public record AcpSessionEntry(
            String sessionId,
            String displayName,
            String subject,
            String chatType,
            String channel,
            String label,
            String spawnedBy,
            String origin,
            String originLabel,
            String groupChannel,
            String space,
            Long updatedAt,
            Long inputTokens,
            Long outputTokens,
            Long totalTokens,
            String modelOverride,
            String providerOverride,
            Integer contextTokens,
            String thinkingLevel,
            String verboseLevel,
            String reasoningLevel,
            String elevatedLevel,
            String sendPolicy,
            String lastChannel,
            String lastTo,
            String lastAccountId,
            Boolean systemSent,
            Boolean abortedLastRun,
            String sessionFile) {
    }
}

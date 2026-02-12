package com.openclaw.common.config;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * Root configuration type for OpenClaw.
 * Corresponds to TypeScript's OpenClawConfig (types.ts).
 */
@Data
public class OpenClawConfig {

    /** Primary model identifier (e.g. "anthropic/claude-sonnet-4-5"). */
    private String model;

    /** Model alias mappings (e.g. "sonnet" -> "anthropic/claude-sonnet-4-5"). */
    private Map<String, String> modelAliases;

    /** Model provider configurations. */
    private ModelsConfig models;

    /** Agent configurations. */
    private AgentsConfig agents;

    /** Channel-specific configurations. */
    private ChannelsConfig channels;

    /** Gateway settings. */
    private GatewayConfig gateway;

    /** Authentication settings. */
    private AuthConfig auth;

    /** Cron/scheduling settings. */
    private CronConfig cron;

    /** Session settings. */
    private SessionConfig session;

    /** Tools settings. */
    private ToolsConfig tools;

    /** Global sandbox settings. */
    private SandboxConfig sandbox;

    /** Logging settings. */
    private LoggingConfig logging;

    /** Skills settings. */
    private SkillsConfig skills;

    /** Plugins settings. */
    private Map<String, Object> plugins;

    // --- Nested config types ---

    @Data
    public static class ModelsConfig {
        /** Provider configs keyed by provider id (e.g. "anthropic", "openai"). */
        private Map<String, ProviderConfig> providers;
        private List<ModelDefinition> definitions;
    }

    @Data
    public static class ProviderConfig {
        private String id;
        private String apiBaseUrl;
        private String apiKey;
        private List<ModelDefinition> models;
        /** Whether this provider is enabled (default true). */
        private boolean enabled = true;
    }

    @Data
    public static class ModelDefinition {
        private String id;
        private String name;
        private String provider;
        /** Context window size in tokens (nullable — null means unknown). */
        private Integer contextWindow;
        private int maxTokens;
        private ModelCost cost;
        private List<String> input;
        /** Whether the model supports reasoning/thinking. */
        private Boolean reasoning;
    }

    @Data
    public static class ModelCost {
        private double input;
        private double output;
        private double cacheRead;
        private double cacheWrite;
    }

    @Data
    public static class AgentsConfig {
        private List<AgentEntry> list;
        private AgentDefaults defaults;

        /** Convenience: returns list or empty. */
        public List<AgentEntry> getEntries() {
            return list != null ? list : List.of();
        }
    }

    @Data
    public static class AgentEntry {
        private String id;
        private String name;
        private String model;
        private String description;
        private String systemPrompt;
        private SandboxConfig sandbox;
        /** Per-agent fallback model list. */
        private List<String> modelFallbacks;
    }

    @Data
    public static class AgentDefaults {
        private int maxConcurrent = 3;
        private int subagentMaxConcurrent = 2;

        /** Primary model ref ("provider/model" or just model name). */
        private String model;

        /** Ordered list of fallback model refs to try when primary fails. */
        private List<String> modelFallbacks;

        /**
         * Per-model config map (keyed by "provider/model"). Used for aliases and
         * allowlists.
         */
        private Map<String, Object> models;

        /** Max context tokens cap (overrides model-reported context window). */
        private Integer contextTokens;

        /** Default thinking level: off | minimal | low | medium | high | xhigh. */
        private String thinkingDefault;

        /** Allowlist of model keys ("provider/model"). Null means allow any. */
        private List<String> modelAllowlist;

        /** Sandbox defaults for agents. */
        private SandboxDefaults sandbox;

        /** Subagent defaults. */
        private SubagentDefaults subagents;
    }

    @Data
    public static class SandboxDefaults {
        /** "spawned" | "all" | "none" */
        private String sessionToolsVisibility;
    }

    @Data
    public static class SubagentDefaults {
        private String model;
        private String thinking;
        private List<String> allowAgents;
    }

    @Data
    public static class ChannelsConfig {
        private Map<String, ChannelConfig> config;
    }

    @Data
    public static class ChannelConfig {
        private boolean enabled;
        private String botToken;
        private List<String> allowFrom;
    }

    @Data
    public static class GatewayConfig {
        private int port = 3578;
        private String host = "127.0.0.1";
        /** Authentication settings (corresponds to TS gateway.auth). */
        private GatewayAuthConfig auth;
        /** IPs of trusted reverse proxies for X-Forwarded-For resolution. */
        private List<String> trustedProxies;
    }

    /**
     * Authentication config for Gateway connections.
     * Corresponds to TS GatewayAuthConfig.
     */
    @Data
    public static class GatewayAuthConfig {
        /** "token" | "password" — defaults to "token" when token is set. */
        private String mode;
        /** Shared token for token mode. */
        private String token;
        /** Shared password for password mode. */
        private String password;
    }

    @Data
    public static class AuthConfig {
        private int cooldownMs = 10000;
        private int maxFailures = 5;
    }

    @Data
    public static class CronConfig {
        private boolean enabled = true;
        private String store;
    }

    @Data
    public static class SandboxConfig {
        /** off | non-main | all */
        private String mode = "off";
        /** session | agent | shared */
        private String scope = "session";
        /** none | ro | rw */
        private String workspaceAccess = "rw";
        private String workspaceRoot;
        private SandboxToolPolicy tools;
    }

    @Data
    public static class SandboxToolPolicy {
        private List<String> allow;
        private List<String> deny;
    }

    @Data
    public static class LoggingConfig {
        private String level = "info";
    }

    @Data
    public static class SessionConfig {
        /** Main session key. */
        private String mainKey;
        /** "per-sender" | "global" */
        private String scope;
        /** Agent-to-agent session config. */
        private SessionA2AConfig agentToAgent;
    }

    @Data
    public static class SessionA2AConfig {
        private Integer maxPingPongTurns;
    }

    @Data
    public static class ToolsConfig {
        /** Agent-to-agent tool policy. */
        private AgentToAgentConfig agentToAgent;
    }

    @Data
    public static class AgentToAgentConfig {
        private Boolean enabled;
        private List<String> allow;
    }

    @Data
    public static class SkillsConfig {
        /** Per-skill config entries keyed by skill key. */
        private Map<String, Object> entries;
        /** Bundled skills allowlist (null = allow all). */
        private Object allowBundled;
        /** Load/watch settings. */
        private SkillLoadConfig load;
    }

    @Data
    public static class SkillLoadConfig {
        /** Whether to watch for skill file changes (default true). */
        private Boolean watch;
        /** Debounce ms for watch events. */
        private Long watchDebounceMs;
        /** Extra skill directories to load from. */
        private List<String> extraDirs;
    }
}

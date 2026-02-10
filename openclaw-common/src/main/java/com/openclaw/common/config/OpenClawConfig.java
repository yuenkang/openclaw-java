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

    /** Global sandbox settings. */
    private SandboxConfig sandbox;

    /** Logging settings. */
    private LoggingConfig logging;

    // --- Nested config types ---

    @Data
    public static class ModelsConfig {
        private List<ProviderConfig> providers;
        private List<ModelDefinition> definitions;
    }

    @Data
    public static class ProviderConfig {
        private String id;
        private String apiBaseUrl;
        private String apiKey;
        private List<ModelDefinition> models;
    }

    @Data
    public static class ModelDefinition {
        private String id;
        private String name;
        private String provider;
        private int contextWindow;
        private int maxTokens;
        private ModelCost cost;
        private List<String> input;
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
    }

    @Data
    public static class AgentEntry {
        private String id;
        private String name;
        private String model;
        private String systemPrompt;
        private SandboxConfig sandbox;
    }

    @Data
    public static class AgentDefaults {
        private int maxConcurrent = 3;
        private int subagentMaxConcurrent = 2;
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
        private String token;
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
}

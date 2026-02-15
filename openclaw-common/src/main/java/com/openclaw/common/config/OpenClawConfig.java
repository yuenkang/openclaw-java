package com.openclaw.common.config;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import lombok.Data;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Root configuration type for OpenClaw.
 * Corresponds to TypeScript's OpenClawConfig (types.openclaw.ts).
 */
@Data
public class OpenClawConfig {

    // ===== Top-level fields (types.openclaw.ts) =====

    private MetaConfig meta;
    private AuthConfig auth;
    private EnvConfig env;
    private WizardConfig wizard;
    private DiagnosticsConfig diagnostics;
    private LoggingConfig logging;
    private UpdateConfig update;
    private BrowserConfig browser;
    private UiConfig ui;
    private SkillsConfig skills;
    private PluginsConfig plugins;
    private ModelsConfig models;
    private NodeHostConfig nodeHost;
    private AgentsConfig agents;
    private ToolsConfig tools;
    private List<AgentBinding> bindings;
    private BroadcastConfig broadcast;
    private AudioConfig audio;
    private MessagesConfig messages;
    private CommandsConfig commands;
    private ApprovalsConfig approvals;
    private SandboxConfig sandbox;
    private SessionConfig session;
    private WebConfig web;
    private ChannelsConfig channels;
    private CronConfig cron;
    private HooksConfig hooks;
    private DiscoveryConfig discovery;
    private CanvasHostConfig canvasHost;
    private TalkConfig talk;
    private GatewayConfig gateway;
    private MemoryConfig memory;

    /** Legacy top-level model aliases (agent.modelAliases in TS). */
    private Map<String, String> modelAliases;

    /** Top-level default model shortcut (e.g. "anthropic/claude-sonnet-4-5"). */
    private String model;

    // ===== types.openclaw.ts: MetaConfig =====

    @Data
    public static class MetaConfig {
        private String lastTouchedVersion;
        private String lastTouchedAt;
    }

    // ===== types.openclaw.ts: EnvConfig =====

    @Data
    public static class EnvConfig {
        private ShellEnvConfig shellEnv;
        private Map<String, String> vars;

        @Data
        public static class ShellEnvConfig {
            private Boolean enabled;
            private Integer timeoutMs;
        }
    }

    // ===== types.openclaw.ts: WizardConfig =====

    @Data
    public static class WizardConfig {
        private String lastRunAt;
        private String lastRunVersion;
        private String lastRunCommit;
        private String lastRunCommand;
        /** "local" | "remote" */
        private String lastRunMode;
    }

    // ===== types.openclaw.ts: UpdateConfig =====

    @Data
    public static class UpdateConfig {
        /** "stable" | "beta" | "dev" */
        private String channel;
        private Boolean checkOnStart;
    }

    // ===== types.openclaw.ts: UiConfig =====

    @Data
    public static class UiConfig {
        private String seamColor;
        private UiAssistantConfig assistant;

        @Data
        public static class UiAssistantConfig {
            private String name;
            private String avatar;
        }
    }

    // ===== types.auth.ts =====

    @Data
    public static class AuthConfig {
        private Integer cooldownMs;
        private Integer maxFailures;
    }

    // ===== types.approvals.ts =====

    @Data
    public static class ApprovalsConfig {
        private Boolean enabled;
        private String mode;
        private Integer timeoutSeconds;
    }

    // ===== types.base.ts: SessionConfig =====

    @Data
    public static class SessionConfig {
        /** "per-sender" | "global" */
        private String scope;
        /** "main" | "per-peer" | "per-channel-peer" | "per-account-channel-peer" */
        private String dmScope;
        private Map<String, List<String>> identityLinks;
        private List<String> resetTriggers;
        private Integer idleMinutes;
        private SessionResetConfig reset;
        private SessionResetByTypeConfig resetByType;
        private Map<String, SessionResetConfig> resetByChannel;
        private String store;
        private Integer typingIntervalSeconds;
        /** "never" | "instant" | "thinking" | "message" */
        private String typingMode;
        private String mainKey;
        private SessionSendPolicyConfig sendPolicy;
        private SessionA2AConfig agentToAgent;
    }

    @Data
    public static class SessionResetConfig {
        /** "daily" | "idle" */
        private String mode;
        private Integer atHour;
        private Integer idleMinutes;
    }

    @Data
    public static class SessionResetByTypeConfig {
        private SessionResetConfig dm;
        private SessionResetConfig group;
        private SessionResetConfig thread;
    }

    @Data
    public static class SessionSendPolicyConfig {
        /** "allow" | "deny" */
        private String defaultAction;
        private List<SessionSendPolicyRule> rules;
    }

    @Data
    public static class SessionSendPolicyRule {
        /** "allow" | "deny" */
        private String action;
        private SessionSendPolicyMatch match;
    }

    @Data
    public static class SessionSendPolicyMatch {
        private String channel;
        private String chatType;
        private String keyPrefix;
    }

    @Data
    public static class SessionA2AConfig {
        private Integer maxPingPongTurns;
    }

    // ===== types.base.ts: LoggingConfig =====

    @Data
    public static class LoggingConfig {
        /** "silent"|"fatal"|"error"|"warn"|"info"|"debug"|"trace" */
        private String level;
        private String file;
        private String consoleLevel;
        /** "pretty" | "compact" | "json" */
        private String consoleStyle;
        /** "off" | "tools" */
        private String redactSensitive;
        private List<String> redactPatterns;
    }

    // ===== types.base.ts: DiagnosticsConfig =====

    @Data
    public static class DiagnosticsConfig {
        private Boolean enabled;
        private List<String> flags;
        private DiagnosticsOtelConfig otel;
        private DiagnosticsCacheTraceConfig cacheTrace;
    }

    @Data
    public static class DiagnosticsOtelConfig {
        private Boolean enabled;
        private String endpoint;
        /** "http/protobuf" | "grpc" */
        private String protocol;
        private Map<String, String> headers;
        private String serviceName;
        private Boolean traces;
        private Boolean metrics;
        private Boolean logs;
        private Double sampleRate;
        private Integer flushIntervalMs;
    }

    @Data
    public static class DiagnosticsCacheTraceConfig {
        private Boolean enabled;
        private String filePath;
        private Boolean includeMessages;
        private Boolean includePrompt;
        private Boolean includeSystem;
    }

    // ===== types.base.ts: WebConfig =====

    @Data
    public static class WebConfig {
        private Boolean enabled;
        private Integer heartbeatSeconds;
        private WebReconnectConfig reconnect;
    }

    @Data
    public static class WebReconnectConfig {
        private Integer initialMs;
        private Integer maxMs;
        private Double factor;
        private Double jitter;
        private Integer maxAttempts;
    }

    // ===== types.base.ts: misc types =====

    @Data
    public static class HumanDelayConfig {
        /** "off" | "natural" | "custom" */
        private String mode;
        private Integer minMs;
        private Integer maxMs;
    }

    @Data
    public static class IdentityConfig {
        private String name;
        private String theme;
        private String emoji;
        private String avatar;
    }

    @Data
    public static class OutboundRetryConfig {
        private Integer attempts;
        private Integer minDelayMs;
        private Integer maxDelayMs;
        private Double jitter;
    }

    @Data
    public static class BlockStreamingCoalesceConfig {
        private Integer minChars;
        private Integer maxChars;
        private Integer idleMs;
    }

    @Data
    public static class BlockStreamingChunkConfig {
        private Integer minChars;
        private Integer maxChars;
        /** "paragraph" | "newline" | "sentence" */
        private String breakPreference;
    }

    // ===== types.browser.ts =====

    @Data
    public static class BrowserConfig {
        private Boolean enabled;
        /** If false, disable browser act:evaluate (arbitrary JS). Default: true */
        private Boolean evaluateEnabled;
        /** Base URL of the CDP endpoint (for remote browsers). */
        private String cdpUrl;
        /** Remote CDP HTTP timeout (ms). Default: 1500. */
        private Integer remoteCdpTimeoutMs;
        /** Remote CDP WebSocket handshake timeout (ms). */
        private Integer remoteCdpHandshakeTimeoutMs;
        /** Accent color for the openclaw browser profile (hex). Default: #FF4500 */
        private String color;
        /** Override the browser executable path. */
        private String executablePath;
        /** Start Chrome headless. Default: false */
        private Boolean headless;
        /** Pass --no-sandbox to Chrome (Linux containers). Default: false */
        private Boolean noSandbox;
        /** If true: never launch; only attach to existing browser. Default: false */
        private Boolean attachOnly;
        /** Default profile when profile param omitted. Default: "chrome" */
        private String defaultProfile;
        /** Named browser profiles with explicit CDP ports or URLs. */
        private Map<String, BrowserProfileConfig> profiles;
        /** Default snapshot options. */
        private BrowserSnapshotDefaults snapshotDefaults;

        /** Legacy field (kept for backward compat). */
        private String mode;
        /** Legacy field. */
        private String profile;
        /** Legacy field. */
        private Integer cdpPort;
        /** Control server port (derived from gateway port). */
        private Integer controlPort;
    }

    @Data
    public static class BrowserProfileConfig {
        /** CDP port for this profile. */
        private Integer cdpPort;
        /** CDP URL for this profile (remote Chrome). */
        private String cdpUrl;
        /** Profile driver: "openclaw" | "extension". */
        private String driver;
        /** Profile color (hex). */
        private String color;
    }

    @Data
    public static class BrowserSnapshotDefaults {
        /** Default snapshot mode: "efficient" */
        private String mode;
    }

    // ===== types.models.ts =====

    @Data
    public static class ModelsConfig {
        /** "merge" | "replace" */
        private String mode;
        private Map<String, ProviderConfig> providers;
        private List<ModelDefinition> definitions;
        private BedrockDiscoveryConfig bedrockDiscovery;
    }

    @Data
    public static class ProviderConfig {
        private String id;
        private String baseUrl;
        /** Alias for baseUrl used by some callers */
        private String apiBaseUrl;
        private String apiKey;
        /** "api-key" | "aws-sdk" | "oauth" | "token" */
        private String auth;
        private String api;
        private Map<String, String> headers;
        private Boolean authHeader;
        private boolean enabled = true;
        private List<ModelDefinition> models;

        /** Provider name/displayName alias. */
        private String provider;
    }

    @Data
    public static class ModelDefinition {
        private String id;
        private String name;
        private String api;
        private Boolean reasoning;
        private List<String> input;
        private ModelCost cost;
        private Integer contextWindow;
        private Integer maxTokens;
        private Map<String, String> headers;
        private ModelCompatConfig compat;
    }

    @Data
    public static class ModelCost {
        private double input;
        private double output;
        private double cacheRead;
        private double cacheWrite;
    }

    @Data
    public static class ModelCompatConfig {
        private Boolean supportsStore;
        private Boolean supportsDeveloperRole;
        private Boolean supportsReasoningEffort;
        /** "max_completion_tokens" | "max_tokens" */
        private String maxTokensField;
    }

    @Data
    public static class BedrockDiscoveryConfig {
        private Boolean enabled;
        private String region;
        private List<String> providerFilter;
        private Integer refreshInterval;
        private Integer defaultContextWindow;
        private Integer defaultMaxTokens;
    }

    @Data
    public static class SandboxConfig {
        private Boolean enabled;
        private SandboxToolsConfig tools;
    }

    @Data
    public static class SandboxToolsConfig {
        private List<String> allow;
        private List<String> deny;
    }

    // ===== types.agents.ts =====

    @Data
    public static class AgentsConfig {
        private AgentDefaultsConfig defaults;
        private List<AgentEntry> list;

        /** Convenience: returns list or empty. */
        public List<AgentEntry> getEntries() {
            return list != null ? list : List.of();
        }
    }

    @Data
    public static class AgentEntry {
        private String id;
        private Boolean defaultAgent;
        private String name;
        private String description;
        private String systemPrompt;
        private String workspace;
        private String agentDir;
        /** String or object {primary, fallbacks} — Jackson maps to Object. */
        private Object model;
        private List<String> skills;
        private MemorySearchConfig memorySearch;
        private HumanDelayConfig humanDelay;
        private HeartbeatConfig heartbeat;
        private IdentityConfig identity;
        private GroupChatConfig groupChat;
        private AgentSubagentsConfig subagents;
        private AgentSandboxConfig sandbox;
        private AgentToolsConfig agentTools;
        /** Per-agent fallback model list (backward compat). */
        private List<String> modelFallbacks;

        /** Helper: extract model as String when model is a plain string. */
        public String getModelString() {
            if (model instanceof String s)
                return s;
            return null;
        }
    }

    @Data
    public static class AgentSubagentsConfig {
        private List<String> allowAgents;
        /** String or {primary, fallbacks} */
        private Object model;
    }

    @Data
    public static class AgentSandboxConfig {
        /** "off" | "non-main" | "all" */
        private String mode;
        /** "none" | "ro" | "rw" */
        private String workspaceAccess;
        /** "spawned" | "all" */
        private String sessionToolsVisibility;
        /** "session" | "agent" | "shared" */
        private String scope;
        private Boolean perSession;
        private String workspaceRoot;
        private SandboxDockerSettings docker;
        private SandboxBrowserSettings browser;
        private SandboxPruneSettings prune;
    }

    @Data
    public static class AgentBinding {
        private String agentId;
        private AgentBindingMatch match;

        @Data
        public static class AgentBindingMatch {
            private String channel;
            private String accountId;
            private AgentBindingPeer peer;
            private String guildId;
            private String teamId;
        }

        @Data
        public static class AgentBindingPeer {
            /** "dm" | "group" | "channel" */
            private String kind;
            private String id;
        }
    }

    // ===== types.agent-defaults.ts =====

    @Data
    public static class AgentDefaultsConfig {
        private AgentModelListConfig model;
        private AgentModelListConfig imageModel;
        private Map<String, AgentModelEntryConfig> models;
        private String workspace;
        private String repoRoot;
        private Boolean skipBootstrap;
        private Integer bootstrapMaxChars;
        private String userTimezone;
        /** "auto" | "12" | "24" */
        private String timeFormat;
        private String envelopeTimezone;
        /** "on" | "off" */
        private String envelopeTimestamp;
        /** "on" | "off" */
        private String envelopeElapsed;
        private Integer contextTokens;
        private Map<String, CliBackendConfig> cliBackends;
        private AgentContextPruningConfig contextPruning;
        private AgentCompactionConfig compaction;
        private MemorySearchConfig memorySearch;
        /** "off"|"minimal"|"low"|"medium"|"high"|"xhigh" */
        private String thinkingDefault;
        /** "off"|"on"|"full" */
        private String verboseDefault;
        /** "off"|"on"|"ask"|"full" */
        private String elevatedDefault;
        /** "off"|"on" */
        private String blockStreamingDefault;
        /** "text_end"|"message_end" */
        private String blockStreamingBreak;
        private BlockStreamingChunkConfig blockStreamingChunk;
        private BlockStreamingCoalesceConfig blockStreamingCoalesce;
        private HumanDelayConfig humanDelay;
        private Integer timeoutSeconds;
        private Integer mediaMaxMb;
        private Integer typingIntervalSeconds;
        /** "never"|"instant"|"thinking"|"message" */
        private String typingMode;
        private HeartbeatConfig heartbeat;
        private Integer maxConcurrent;
        private SubagentDefaultsConfig subagents;
        private AgentSandboxConfig sandbox;
        private List<String> modelFallbacks;
        private List<String> modelAllowlist;
    }

    @Data
    public static class AgentModelListConfig {
        private String primary;
        private List<String> fallbacks;
    }

    @Data
    public static class AgentModelEntryConfig {
        private String alias;
        private Map<String, Object> params;
        private Boolean streaming;
    }

    @Data
    public static class HeartbeatConfig {
        private String every;
        private HeartbeatActiveHours activeHours;
        private String model;
        private String session;
        /** "last"|"none" or channel id */
        private String target;
        private String to;
        private String accountId;
        private String prompt;
        private Integer ackMaxChars;
        private Boolean includeReasoning;

        @Data
        public static class HeartbeatActiveHours {
            private String start;
            private String end;
            private String timezone;
        }
    }

    @Data
    public static class SubagentDefaultsConfig {
        private Integer maxConcurrent;
        private Integer archiveAfterMinutes;
        /** String or {primary, fallbacks} */
        private Object model;
        private String thinking;
    }

    @Data
    public static class AgentContextPruningConfig {
        /** "off" | "cache-ttl" */
        private String mode;
        private String ttl;
        private Integer keepLastAssistants;
        private Double softTrimRatio;
        private Double hardClearRatio;
        private Integer minPrunableToolChars;
        private PruningToolPolicy tools;
        private PruningSoftTrim softTrim;
        private PruningHardClear hardClear;

        @Data
        public static class PruningToolPolicy {
            private List<String> allow;
            private List<String> deny;
        }

        @Data
        public static class PruningSoftTrim {
            private Integer maxChars;
            private Integer headChars;
            private Integer tailChars;
        }

        @Data
        public static class PruningHardClear {
            private Boolean enabled;
            private String placeholder;
        }
    }

    @Data
    public static class AgentCompactionConfig {
        /** "default" | "safeguard" */
        private String mode;
        private Integer reserveTokensFloor;
        private Double maxHistoryShare;
        private AgentCompactionMemoryFlushConfig memoryFlush;
    }

    @Data
    public static class AgentCompactionMemoryFlushConfig {
        private Boolean enabled;
        private Integer softThresholdTokens;
        private String prompt;
        private String systemPrompt;
    }

    @Data
    public static class CliBackendConfig {
        private String command;
        private List<String> args;
        /** "json"|"text"|"jsonl" */
        private String output;
        private String resumeOutput;
        /** "arg"|"stdin" */
        private String input;
        private Integer maxPromptArgChars;
        private Map<String, String> env;
        private List<String> clearEnv;
        private String modelArg;
        private Map<String, String> modelAliases;
        private String sessionArg;
        private List<String> sessionArgs;
        private List<String> resumeArgs;
        /** "always"|"existing"|"none" */
        private String sessionMode;
        private List<String> sessionIdFields;
        private String systemPromptArg;
        /** "append"|"replace" */
        private String systemPromptMode;
        /** "first"|"always"|"never" */
        private String systemPromptWhen;
        private String imageArg;
        /** "repeat"|"list" */
        private String imageMode;
        private Boolean serialize;
    }

    // ===== types.tools.ts =====

    @Data
    public static class ToolsConfig {
        /** "minimal"|"coding"|"messaging"|"full" */
        private String profile;
        private List<String> allow;
        private List<String> alsoAllow;
        private List<String> deny;
        private Map<String, ToolPolicyConfig> byProvider;
        private WebToolsConfig web;
        private MediaToolsConfig media;
        private LinkToolsConfig links;
        private MessageToolConfig message;
        private AgentToAgentToolConfig agentToAgent;
        private ElevatedConfig elevated;
        private ExecToolConfig exec;
        private SubagentToolConfig subagents;
        private SandboxToolPolicyConfig sandbox;
    }

    @Data
    public static class ToolPolicyConfig {
        private List<String> allow;
        private List<String> alsoAllow;
        private List<String> deny;
        /** "minimal"|"coding"|"messaging"|"full" */
        private String profile;
    }

    @Data
    public static class AgentToolsConfig {
        private String profile;
        private List<String> allow;
        private List<String> alsoAllow;
        private List<String> deny;
        private Map<String, ToolPolicyConfig> byProvider;
        private AgentElevatedConfig elevated;
        private ExecToolConfig exec;
        private SandboxToolPolicyConfig sandbox;
    }

    @Data
    public static class AgentElevatedConfig {
        private Boolean enabled;
        private Map<String, List<Object>> allowFrom;
    }

    @Data
    public static class ElevatedConfig {
        private Boolean enabled;
        private Map<String, List<Object>> allowFrom;
    }

    @Data
    public static class ExecToolConfig {
        /** "sandbox"|"gateway"|"node" */
        private String host;
        /** "deny"|"allowlist"|"full" */
        private String security;
        /** "off"|"on-miss"|"always" */
        private String ask;
        private String node;
        private List<String> pathPrepend;
        private List<String> safeBins;
        private Integer backgroundMs;
        private Integer timeoutSec;
        private Integer approvalRunningNoticeMs;
        private Integer cleanupMs;
        private Boolean notifyOnExit;
        private ExecApplyPatchConfig applyPatch;

        @Data
        public static class ExecApplyPatchConfig {
            private Boolean enabled;
            private List<String> allowModels;
        }
    }

    @Data
    public static class WebToolsConfig {
        private WebSearchConfig search;
        private WebFetchConfig fetch;
    }

    @Data
    public static class WebSearchConfig {
        private Boolean enabled;
        /** "brave"|"perplexity" */
        private String provider;
        private String apiKey;
        private Integer maxResults;
        private Integer timeoutSeconds;
        private Integer cacheTtlMinutes;
        private PerplexityConfig perplexity;

        @Data
        public static class PerplexityConfig {
            private String apiKey;
            private String baseUrl;
            private String model;
        }
    }

    @Data
    public static class WebFetchConfig {
        private Boolean enabled;
        private Integer maxChars;
        private Integer maxCharsCap;
        private Integer timeoutSeconds;
        private Integer cacheTtlMinutes;
        private Integer maxRedirects;
        private String userAgent;
        private Boolean readability;
        private FirecrawlConfig firecrawl;

        @Data
        public static class FirecrawlConfig {
            private Boolean enabled;
            private String apiKey;
            private String baseUrl;
            private Boolean onlyMainContent;
            private Integer maxAgeMs;
            private Integer timeoutSeconds;
        }
    }

    @Data
    public static class MediaToolsConfig {
        private List<MediaUnderstandingModelConfig> models;
        private Integer concurrency;
        private MediaUnderstandingConfig image;
        private MediaUnderstandingConfig audio;
        private MediaUnderstandingConfig video;
    }

    @Data
    public static class MediaUnderstandingConfig {
        private Boolean enabled;
        private MediaUnderstandingScopeConfig scope;
        private Integer maxBytes;
        private Integer maxChars;
        private String prompt;
        private Integer timeoutSeconds;
        private String language;
        private Map<String, Map<String, Object>> providerOptions;
        private String baseUrl;
        private Map<String, String> headers;
        private MediaUnderstandingAttachmentsConfig attachments;
        private List<MediaUnderstandingModelConfig> models;
    }

    @Data
    public static class MediaUnderstandingScopeConfig {
        /** "allow"|"deny" */
        private String defaultAction;
        private List<MediaUnderstandingScopeRule> rules;
    }

    @Data
    public static class MediaUnderstandingScopeRule {
        private String action;
        private MediaUnderstandingScopeMatch match;
    }

    @Data
    public static class MediaUnderstandingScopeMatch {
        private String channel;
        private String chatType;
        private String keyPrefix;
    }

    @Data
    public static class MediaUnderstandingAttachmentsConfig {
        /** "first"|"all" */
        private String mode;
        private Integer maxAttachments;
        /** "first"|"last"|"path"|"url" */
        private String prefer;
    }

    @Data
    public static class MediaUnderstandingModelConfig {
        private String provider;
        private String model;
        private List<String> capabilities;
        /** "provider"|"cli" */
        private String type;
        private String command;
        private List<String> args;
        private String prompt;
        private Integer maxChars;
        private Integer maxBytes;
        private Integer timeoutSeconds;
        private String language;
        private Map<String, Map<String, Object>> providerOptions;
        private String baseUrl;
        private Map<String, String> headers;
        private String profile;
        private String preferredProfile;
    }

    @Data
    public static class LinkToolsConfig {
        private Boolean enabled;
        private MediaUnderstandingScopeConfig scope;
        private Integer maxLinks;
        private Integer timeoutSeconds;
        private List<LinkModelConfig> models;
    }

    @Data
    public static class LinkModelConfig {
        private String type;
        private String command;
        private List<String> args;
        private Integer timeoutSeconds;
    }

    @Data
    public static class MessageToolConfig {
        private Boolean allowCrossContextSend;
        private CrossContextConfig crossContext;
        private MessageBroadcastConfig broadcast;

        @Data
        public static class CrossContextConfig {
            private Boolean allowWithinProvider;
            private Boolean allowAcrossProviders;
            private CrossContextMarkerConfig marker;

            @Data
            public static class CrossContextMarkerConfig {
                private Boolean enabled;
                private String prefix;
                private String suffix;
            }
        }

        @Data
        public static class MessageBroadcastConfig {
            private Boolean enabled;
        }
    }

    @Data
    public static class AgentToAgentToolConfig {
        private Boolean enabled;
        private List<String> allow;
    }

    @Data
    public static class SubagentToolConfig {
        /** String or {primary, fallbacks} */
        private Object model;
        private SandboxToolPolicy tools;
    }

    @Data
    public static class SandboxToolPolicyConfig {
        private SandboxToolPolicy tools;
    }

    @Data
    public static class SandboxToolPolicy {
        private List<String> allow;
        private List<String> deny;
    }

    // ===== types.tools.ts: MemorySearchConfig =====

    @Data
    public static class MemorySearchConfig {
        private Boolean enabled;
        private List<String> sources;
        private List<String> extraPaths;
        private MemorySearchExperimentalConfig experimental;
        /** "openai"|"gemini"|"local" */
        private String provider;
        private MemorySearchRemoteConfig remote;
        /** "openai"|"gemini"|"local"|"none" */
        private String fallback;
        private String model;
        private MemorySearchLocalConfig local;
        private MemorySearchStoreConfig store;
        private MemorySearchChunkingConfig chunking;
        private MemorySearchSyncConfig sync;
        private MemorySearchQueryConfig query;
        private MemorySearchCacheConfig cache;
    }

    @Data
    public static class MemorySearchExperimentalConfig {
        private Boolean sessionMemory;
    }

    @Data
    public static class MemorySearchRemoteConfig {
        private String baseUrl;
        private String apiKey;
        private Map<String, String> headers;
        private MemorySearchBatchConfig batch;
    }

    @Data
    public static class MemorySearchBatchConfig {
        private Boolean enabled;
        private Boolean wait;
        private Integer concurrency;
        private Integer pollIntervalMs;
        private Integer timeoutMinutes;
    }

    @Data
    public static class MemorySearchLocalConfig {
        private String modelPath;
        private String modelCacheDir;
    }

    @Data
    public static class MemorySearchStoreConfig {
        private String driver;
        private String path;
        private MemorySearchVectorConfig vector;
        private MemorySearchStoreCacheConfig cache;
    }

    @Data
    public static class MemorySearchVectorConfig {
        private Boolean enabled;
        private String extensionPath;
    }

    @Data
    public static class MemorySearchStoreCacheConfig {
        private Boolean enabled;
        private Integer maxEntries;
    }

    @Data
    public static class MemorySearchChunkingConfig {
        private Integer tokens;
        private Integer overlap;
    }

    @Data
    public static class MemorySearchSyncConfig {
        private Boolean onSessionStart;
        private Boolean onSearch;
        private Boolean watch;
        private Integer watchDebounceMs;
        private Integer intervalMinutes;
        private MemorySearchSessionSyncConfig sessions;
    }

    @Data
    public static class MemorySearchSessionSyncConfig {
        private Integer deltaBytes;
        private Integer deltaMessages;
    }

    @Data
    public static class MemorySearchQueryConfig {
        private Integer maxResults;
        private Double minScore;
        private MemorySearchHybridConfig hybrid;
    }

    @Data
    public static class MemorySearchHybridConfig {
        private Boolean enabled;
        private Double vectorWeight;
        private Double textWeight;
        private Integer candidateMultiplier;
    }

    @Data
    public static class MemorySearchCacheConfig {
        private Boolean enabled;
        private Integer maxEntries;
    }

    // ===== types.messages.ts =====

    @Data
    public static class MessagesConfig {
        private String messagePrefix;
        private String responsePrefix;
        private GroupChatConfig groupChat;
        private QueueConfig queue;
        private InboundDebounceConfig inbound;
        private String ackReaction;
        /** "group-mentions"|"group-all"|"direct"|"all" */
        private String ackReactionScope;
        private Boolean removeAckAfterReply;
        private TtsConfig tts;
    }

    @Data
    public static class GroupChatConfig {
        private List<String> mentionPatterns;
        private Integer historyLimit;
    }

    @Data
    public static class QueueConfig {
        /**
         * "steer"|"followup"|"collect"|"steer-backlog"|"steer+backlog"|"queue"|"interrupt"
         */
        private String mode;
        private Map<String, String> byChannel;
        private Integer debounceMs;
        private Map<String, Integer> debounceMsByChannel;
        private Integer cap;
        /** "old"|"new"|"summarize" */
        private String drop;
    }

    @Data
    public static class InboundDebounceConfig {
        private Integer debounceMs;
        private Map<String, Integer> byChannel;
    }

    @Data
    public static class BroadcastConfig {
        /** "parallel"|"sequential" */
        private String strategy;
        /** Additional peer→agentIds mappings. */
        private Map<String, List<String>> peers;

        @JsonAnySetter
        public void addPeer(String key, Object value) {
            if ("strategy".equals(key))
                return;
            if (peers == null)
                peers = new HashMap<>();
            if (value instanceof List) {
                @SuppressWarnings("unchecked")
                List<String> list = (List<String>) value;
                peers.put(key, list);
            }
        }

        @JsonAnyGetter
        public Map<String, List<String>> getPeers() {
            return peers;
        }
    }

    @Data
    public static class AudioConfig {
        private AudioTranscriptionConfig transcription;

        @Data
        public static class AudioTranscriptionConfig {
            private List<String> command;
            private Integer timeoutSeconds;
        }
    }

    @Data
    public static class CommandsConfig {
        /** Boolean or "auto" → Object */
        private Object nativeCommands;
        private Object nativeSkills;
        private Boolean text;
        private Boolean bash;
        private Integer bashForegroundMs;
        private Boolean config;
        private Boolean debug;
        private Boolean restart;
        private Boolean useAccessGroups;
        private List<Object> ownerAllowFrom;
    }

    // ===== types.gateway.ts =====

    @Data
    public static class GatewayConfig {
        private Integer port;
        /** "local"|"remote" */
        private String mode;
        /** "auto"|"lan"|"loopback"|"custom"|"tailnet" */
        private String bind;
        private String customBindHost;
        private GatewayControlUiConfig controlUi;
        private GatewayAuthConfig auth;
        private GatewayTailscaleConfig tailscale;
        private GatewayRemoteConfig remote;
        private GatewayReloadConfig reload;
        private GatewayTlsConfig tls;
        private GatewayHttpConfig http;
        private GatewayNodesConfig nodes;
        private List<String> trustedProxies;
    }

    @Data
    public static class GatewayAuthConfig {
        /** "token"|"password" */
        private String mode;
        private String token;
        private String password;
        private Boolean allowTailscale;
    }

    @Data
    public static class GatewayTlsConfig {
        private Boolean enabled;
        private Boolean autoGenerate;
        private String certPath;
        private String keyPath;
        private String caPath;
    }

    @Data
    public static class GatewayControlUiConfig {
        private Boolean enabled;
        private String basePath;
        private String root;
        private List<String> allowedOrigins;
        private Boolean allowInsecureAuth;
        private Boolean dangerouslyDisableDeviceAuth;
    }

    @Data
    public static class GatewayTailscaleConfig {
        /** "off"|"serve"|"funnel" */
        private String mode;
        private Boolean resetOnExit;
    }

    @Data
    public static class GatewayRemoteConfig {
        private String url;
        /** "ssh"|"direct" */
        private String transport;
        private String token;
        private String password;
        private String tlsFingerprint;
        private String sshTarget;
        private String sshIdentity;
    }

    @Data
    public static class GatewayReloadConfig {
        /** "off"|"restart"|"hot"|"hybrid" */
        private String mode;
        private Integer debounceMs;
    }

    @Data
    public static class GatewayHttpConfig {
        private GatewayHttpEndpointsConfig endpoints;
    }

    @Data
    public static class GatewayHttpEndpointsConfig {
        private GatewayHttpChatCompletionsConfig chatCompletions;
        private GatewayHttpResponsesConfig responses;
    }

    @Data
    public static class GatewayHttpChatCompletionsConfig {
        private Boolean enabled;
    }

    @Data
    public static class GatewayHttpResponsesConfig {
        private Boolean enabled;
        private Integer maxBodyBytes;
        private GatewayHttpResponsesFilesConfig files;
        private GatewayHttpResponsesImagesConfig images;
    }

    @Data
    public static class GatewayHttpResponsesFilesConfig {
        private Boolean allowUrl;
        private List<String> allowedMimes;
        private Integer maxBytes;
        private Integer maxChars;
        private Integer maxRedirects;
        private Integer timeoutMs;
        private GatewayHttpResponsesPdfConfig pdf;
    }

    @Data
    public static class GatewayHttpResponsesPdfConfig {
        private Integer maxPages;
        private Integer maxPixels;
        private Integer minTextChars;
    }

    @Data
    public static class GatewayHttpResponsesImagesConfig {
        private Boolean allowUrl;
        private List<String> allowedMimes;
        private Integer maxBytes;
        private Integer maxRedirects;
        private Integer timeoutMs;
    }

    @Data
    public static class GatewayNodesConfig {
        private GatewayNodesBrowserConfig browser;
        private List<String> allowCommands;
        private List<String> denyCommands;

        @Data
        public static class GatewayNodesBrowserConfig {
            /** "auto"|"manual"|"off" */
            private String mode;
            private String node;
        }
    }

    // ===== types.gateway.ts: DiscoveryConfig, CanvasHostConfig, TalkConfig =====

    @Data
    public static class DiscoveryConfig {
        private WideAreaDiscoveryConfig wideArea;
        private MdnsDiscoveryConfig mdns;
    }

    @Data
    public static class WideAreaDiscoveryConfig {
        private Boolean enabled;
        private String domain;
    }

    @Data
    public static class MdnsDiscoveryConfig {
        /** "off"|"minimal"|"full" */
        private String mode;
    }

    @Data
    public static class CanvasHostConfig {
        private Boolean enabled;
        private String root;
        private Integer port;
        private Boolean liveReload;
    }

    @Data
    public static class TalkConfig {
        private String voiceId;
        private Map<String, String> voiceAliases;
        private String modelId;
        private String outputFormat;
        private String apiKey;
        private Boolean interruptOnSpeech;
    }

    // ===== types.hooks.ts =====

    @Data
    public static class HooksConfig {
        private Boolean enabled;
        private String path;
        private String token;
        private Integer maxBodyBytes;
        private List<String> presets;
        private String transformsDir;
        private List<HookMappingConfig> mappings;
        private HooksGmailConfig gmail;
        private InternalHooksConfig internal;
    }

    @Data
    public static class HookMappingConfig {
        private String id;
        private HookMappingMatch match;
        /** "wake"|"agent" */
        private String action;
        /** "now"|"next-heartbeat" */
        private String wakeMode;
        private String name;
        private String sessionKey;
        private String messageTemplate;
        private String textTemplate;
        private Boolean deliver;
        private Boolean allowUnsafeExternalContent;
        private String channel;
        private String to;
        private String model;
        private String thinking;
        private Integer timeoutSeconds;
        private HookMappingTransform transform;
    }

    @Data
    public static class HookMappingMatch {
        private String path;
        private String source;
    }

    @Data
    public static class HookMappingTransform {
        private String module;
        private String exportName;
    }

    @Data
    public static class HooksGmailConfig {
        private String account;
        private String label;
        private String topic;
        private String subscription;
        private String pushToken;
        private String hookUrl;
        private Boolean includeBody;
        private Integer maxBytes;
        private Integer renewEveryMinutes;
        private Boolean allowUnsafeExternalContent;
        private HooksGmailServeConfig serve;
        private HooksGmailTailscaleConfig tailscale;
        private String model;
        private String thinking;
    }

    @Data
    public static class HooksGmailServeConfig {
        private String bind;
        private Integer port;
        private String path;
    }

    @Data
    public static class HooksGmailTailscaleConfig {
        /** "off"|"serve"|"funnel" */
        private String mode;
        private String path;
        private String target;
    }

    @Data
    public static class InternalHooksConfig {
        private Boolean enabled;
        private List<InternalHookHandlerConfig> handlers;
        private Map<String, HookConfig> entries;
        private InternalHooksLoadConfig load;
        private Map<String, HookInstallRecord> installs;
    }

    @Data
    public static class InternalHookHandlerConfig {
        private String event;
        private String module;
        private String exportName;
    }

    @Data
    public static class HookConfig {
        private Boolean enabled;
        private Map<String, String> env;
    }

    @Data
    public static class HookInstallRecord {
        /** "npm"|"archive"|"path" */
        private String source;
        private String spec;
        private String sourcePath;
        private String installPath;
        private String version;
        private String installedAt;
        private List<String> hooks;
    }

    @Data
    public static class InternalHooksLoadConfig {
        private List<String> extraDirs;
    }

    // ===== types.channels.ts =====

    @Data
    public static class ChannelsConfig {
        private ChannelDefaultsConfig defaults;
        /** Channel providers added dynamically. */
        private Map<String, Object> providers;

        @JsonAnySetter
        public void addChannel(String key, Object value) {
            if ("defaults".equals(key))
                return;
            if (providers == null)
                providers = new HashMap<>();
            providers.put(key, value);
        }

        @JsonAnyGetter
        public Map<String, Object> getProviders() {
            return providers;
        }
    }

    @Data
    public static class ChannelDefaultsConfig {
        /** "open"|"disabled"|"allowlist" */
        private String groupPolicy;
        private ChannelHeartbeatVisibilityConfig heartbeat;
    }

    @Data
    public static class ChannelHeartbeatVisibilityConfig {
        private Boolean showOk;
        private Boolean showAlerts;
        private Boolean useIndicator;
    }

    // ===== types.cron.ts =====

    @Data
    public static class CronConfig {
        private Boolean enabled;
        private String store;
        private Integer maxConcurrentRuns;
    }

    // ===== types.memory.ts =====

    @Data
    public static class MemoryConfig {
        private Boolean enabled;
        private String store;
        private String path;
        /** "builtin" or "qmd" — memory indexing backend. */
        private String backend;
        /** "auto", "always", or "never" — citation mode. */
        private String citations;
    }

    // ===== types.plugins.ts =====

    @Data
    public static class PluginsConfig {
        private Map<String, PluginEntryConfig> entries;
    }

    @Data
    public static class PluginEntryConfig {
        private Boolean enabled;
        private Map<String, Object> config;
    }

    // ===== types.skills.ts =====

    @Data
    public static class SkillsConfig {
        private Map<String, Object> entries;
        private Object allowBundled;
        private SkillLoadConfig load;
    }

    @Data
    public static class SkillLoadConfig {
        private Boolean watch;
        private Long watchDebounceMs;
        private List<String> extraDirs;
    }

    // ===== types.sandbox.ts =====

    @Data
    public static class SandboxDockerSettings {
        private String image;
        private String containerPrefix;
        private String workdir;
        private Boolean readOnlyRoot;
        private List<String> tmpfs;
        private String network;
        private String user;
        private List<String> capDrop;
        private Map<String, String> env;
        private String setupCommand;
        private Integer pidsLimit;
        private Object memory;
        private Object memorySwap;
        private Double cpus;
        private Map<String, Object> ulimits;
        private String seccompProfile;
        private String apparmorProfile;
        private List<String> dns;
        private List<String> extraHosts;
        private List<String> binds;
    }

    @Data
    public static class SandboxBrowserSettings {
        private Boolean enabled;
        private String image;
        private String containerPrefix;
        private Integer cdpPort;
        private Integer vncPort;
        private Integer noVncPort;
        private Boolean headless;
        private Boolean enableNoVnc;
        private Boolean allowHostControl;
        private Boolean autoStart;
        private Integer autoStartTimeoutMs;
    }

    @Data
    public static class SandboxPruneSettings {
        private Integer idleHours;
        private Integer maxAgeDays;
    }

    // ===== types.node-host.ts =====

    @Data
    public static class NodeHostConfig {
        private NodeHostBrowserProxyConfig browserProxy;
    }

    @Data
    public static class NodeHostBrowserProxyConfig {
        private Boolean enabled;
        private List<String> allowProfiles;
    }

    // ===== types.tts.ts =====

    @Data
    public static class TtsConfig {
        /** "off"|"always"|"inbound"|"tagged" */
        private String auto;
        private Boolean enabled;
        /** "final"|"all" */
        private String mode;
        /** "elevenlabs"|"openai"|"edge" */
        private String provider;
        private String summaryModel;
        private TtsModelOverrideConfig modelOverrides;
        private TtsElevenLabsConfig elevenlabs;
        private TtsOpenAiConfig openai;
        private TtsEdgeConfig edge;
        private String prefsPath;
        private Integer maxTextLength;
        private Integer timeoutMs;
    }

    @Data
    public static class TtsModelOverrideConfig {
        private Boolean enabled;
        private Boolean allowText;
        private Boolean allowProvider;
        private Boolean allowVoice;
        private Boolean allowModelId;
        private Boolean allowVoiceSettings;
        private Boolean allowNormalization;
        private Boolean allowSeed;
    }

    @Data
    public static class TtsElevenLabsConfig {
        private String apiKey;
        private String baseUrl;
        private String voiceId;
        private String modelId;
        private Integer seed;
        /** "auto"|"on"|"off" */
        private String applyTextNormalization;
        private String languageCode;
        private TtsVoiceSettings voiceSettings;
    }

    @Data
    public static class TtsVoiceSettings {
        private Double stability;
        private Double similarityBoost;
        private Double style;
        private Boolean useSpeakerBoost;
        private Double speed;
    }

    @Data
    public static class TtsOpenAiConfig {
        private String apiKey;
        private String model;
        private String voice;
    }

    @Data
    public static class TtsEdgeConfig {
        private Boolean enabled;
        private String voice;
        private String lang;
        private String outputFormat;
        private String pitch;
        private String rate;
        private String volume;
        private Boolean saveSubtitles;
        private String proxy;
        private Integer timeoutMs;
    }

    // ===== types.openclaw.ts: ConfigFileSnapshot =====

    @Data
    public static class ConfigFileSnapshot {
        private String path;
        private boolean exists;
        private String raw;
        private Object parsed;
        private boolean valid;
        private OpenClawConfig config;
        private String hash;
        private List<ConfigValidationIssue> issues;
        private List<ConfigValidationIssue> warnings;
        private List<LegacyConfigIssue> legacyIssues;
    }

    @Data
    public static class ConfigValidationIssue {
        private String path;
        private String message;
    }

    @Data
    public static class LegacyConfigIssue {
        private String path;
        private String message;
    }

    // ===== types.base.ts: MarkdownConfig =====

    @Data
    public static class MarkdownConfig {
        /** "off"|"bullets"|"code" */
        private String tables;
    }
}

package com.openclaw.common.config;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Default value application for OpenClaw config.
 * Corresponds to TypeScript's defaults.ts.
 */
public final class ConfigDefaults {

    private ConfigDefaults() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    /** Default model aliases (short name → full provider/model). */
    public static final Map<String, String> DEFAULT_MODEL_ALIASES = Map.of(
            "opus", "anthropic/claude-opus-4-6",
            "sonnet", "anthropic/claude-sonnet-4-5",
            "gpt", "openai/gpt-5.2",
            "gpt-mini", "openai/gpt-5-mini",
            "gemini", "google/gemini-3-pro-preview",
            "gemini-flash", "google/gemini-3-flash-preview");

    /** Default context window size. */
    public static final int DEFAULT_CONTEXT_TOKENS = 200_000;

    /** Default max output tokens. */
    public static final int DEFAULT_MODEL_MAX_TOKENS = 8192;

    /** Default max concurrent agents. */
    public static final int DEFAULT_AGENT_MAX_CONCURRENT = 4;

    /** Default max concurrent subagents. */
    public static final int DEFAULT_SUBAGENT_MAX_CONCURRENT = 2;

    // =========================================================================
    // apply*Defaults — immutable transformation pipeline
    // =========================================================================

    /**
     * Apply all defaults in the standard order.
     * Returns a new config with defaults applied (does not mutate input).
     */
    public static OpenClawConfig applyAllDefaults(OpenClawConfig cfg) {
        if (cfg == null)
            cfg = new OpenClawConfig();
        cfg = applyMessageDefaults(cfg);
        cfg = applySessionDefaults(cfg);
        cfg = applyModelDefaults(cfg);
        cfg = applyAgentDefaults(cfg);
        cfg = applyLoggingDefaults(cfg);
        cfg = applyCompactionDefaults(cfg);
        return cfg;
    }

    // =========================================================================
    // Message defaults
    // =========================================================================

    /**
     * Set default ackReactionScope to "group-mentions" if not set.
     */
    public static OpenClawConfig applyMessageDefaults(OpenClawConfig cfg) {
        var messages = cfg.getMessages();
        if (messages != null && messages.getAckReactionScope() != null) {
            return cfg;
        }
        OpenClawConfig next = shallowCopy(cfg);
        var nextMessages = messages != null ? copyMessages(messages) : new OpenClawConfig.MessagesConfig();
        nextMessages.setAckReactionScope("group-mentions");
        next.setMessages(nextMessages);
        return next;
    }

    // =========================================================================
    // Session defaults
    // =========================================================================

    /**
     * Normalize session.mainKey to "main" (the only supported value).
     */
    public static OpenClawConfig applySessionDefaults(OpenClawConfig cfg) {
        var session = cfg.getSession();
        if (session == null || session.getMainKey() == null) {
            return cfg;
        }
        OpenClawConfig next = shallowCopy(cfg);
        var nextSession = copySession(session);
        nextSession.setMainKey("main");
        next.setSession(nextSession);
        return next;
    }

    // =========================================================================
    // Model defaults
    // =========================================================================

    /**
     * Apply model definition defaults: reasoning=false, input=["text"],
     * cost zeros, contextWindow, maxTokens.
     */
    public static OpenClawConfig applyModelDefaults(OpenClawConfig cfg) {
        var modelsConfig = cfg.getModels();
        if (modelsConfig == null || modelsConfig.getProviders() == null) {
            return cfg;
        }

        boolean mutated = false;
        Map<String, OpenClawConfig.ProviderConfig> nextProviders = new LinkedHashMap<>(modelsConfig.getProviders());

        for (var provEntry : modelsConfig.getProviders().entrySet()) {
            var provider = provEntry.getValue();
            if (provider.getModels() == null || provider.getModels().isEmpty()) {
                continue;
            }

            boolean providerMutated = false;
            List<OpenClawConfig.ModelDefinition> nextModels = new java.util.ArrayList<>(provider.getModels());

            for (int i = 0; i < nextModels.size(); i++) {
                var model = nextModels.get(i);
                boolean modelMutated = false;

                // reasoning default
                Boolean reasoning = model.getReasoning();
                if (reasoning == null) {
                    reasoning = false;
                    modelMutated = true;
                }

                // input default
                List<String> input = model.getInput();
                if (input == null) {
                    input = List.of("text");
                    modelMutated = true;
                }

                // cost default
                var cost = resolveModelCost(model.getCost());
                if (model.getCost() == null || !costEquals(model.getCost(), cost)) {
                    modelMutated = true;
                }

                // contextWindow default
                Integer contextWindow = model.getContextWindow();
                if (contextWindow == null || contextWindow <= 0) {
                    contextWindow = DEFAULT_CONTEXT_TOKENS;
                    modelMutated = true;
                }

                // maxTokens default
                Integer maxTokens = model.getMaxTokens();
                int defaultMax = Math.min(DEFAULT_MODEL_MAX_TOKENS, contextWindow);
                if (maxTokens == null || maxTokens <= 0) {
                    maxTokens = defaultMax;
                    modelMutated = true;
                }

                if (modelMutated) {
                    var nextModel = new OpenClawConfig.ModelDefinition();
                    nextModel.setId(model.getId());
                    nextModel.setName(model.getName());
                    nextModel.setApi(model.getApi());
                    nextModel.setReasoning(reasoning);
                    nextModel.setInput(input);
                    nextModel.setCost(cost);
                    nextModel.setContextWindow(contextWindow);
                    nextModel.setMaxTokens(maxTokens);
                    nextModel.setHeaders(model.getHeaders());
                    nextModel.setCompat(model.getCompat());
                    nextModels.set(i, nextModel);
                    providerMutated = true;
                }
            }

            if (providerMutated) {
                var nextProvider = new OpenClawConfig.ProviderConfig();
                nextProvider.setBaseUrl(provider.getBaseUrl());
                nextProvider.setApiKey(provider.getApiKey());
                nextProvider.setAuth(provider.getAuth());
                nextProvider.setApi(provider.getApi());
                nextProvider.setHeaders(provider.getHeaders());
                nextProvider.setAuthHeader(provider.getAuthHeader());
                nextProvider.setModels(nextModels);
                nextProviders.put(provEntry.getKey(), nextProvider);
                mutated = true;
            }
        }

        if (!mutated) {
            return cfg;
        }

        OpenClawConfig next = shallowCopy(cfg);
        var nextModelsConfig = new OpenClawConfig.ModelsConfig();
        nextModelsConfig.setMode(modelsConfig.getMode());
        nextModelsConfig.setProviders(nextProviders);
        nextModelsConfig.setBedrockDiscovery(modelsConfig.getBedrockDiscovery());
        next.setModels(nextModelsConfig);
        return next;
    }

    // =========================================================================
    // Agent defaults
    // =========================================================================

    /**
     * Ensure agents.defaults.maxConcurrent and subagents.maxConcurrent are set.
     */
    public static OpenClawConfig applyAgentDefaults(OpenClawConfig cfg) {
        var agents = cfg.getAgents();
        var defaults = agents != null ? agents.getDefaults() : null;
        boolean hasMax = defaults != null && defaults.getMaxConcurrent() != null;
        boolean hasSubMax = defaults != null && defaults.getSubagents() != null
                && defaults.getSubagents().getMaxConcurrent() != null;
        if (hasMax && hasSubMax) {
            return cfg;
        }

        boolean mutated = false;
        var nextDefaults = defaults != null ? copyAgentDefaults(defaults) : new OpenClawConfig.AgentDefaultsConfig();

        if (!hasMax) {
            nextDefaults.setMaxConcurrent(DEFAULT_AGENT_MAX_CONCURRENT);
            mutated = true;
        }

        var subagents = nextDefaults.getSubagents();
        var nextSubagents = subagents != null ? copySubagentDefaults(subagents)
                : new OpenClawConfig.SubagentDefaultsConfig();
        if (!hasSubMax) {
            nextSubagents.setMaxConcurrent(DEFAULT_SUBAGENT_MAX_CONCURRENT);
            mutated = true;
        }

        if (!mutated)
            return cfg;

        nextDefaults.setSubagents(nextSubagents);
        OpenClawConfig next = shallowCopy(cfg);
        var nextAgents = agents != null ? copyAgentsConfig(agents) : new OpenClawConfig.AgentsConfig();
        nextAgents.setDefaults(nextDefaults);
        next.setAgents(nextAgents);
        return next;
    }

    // =========================================================================
    // Logging defaults
    // =========================================================================

    /**
     * Set default logging.redactSensitive to "tools" if not set.
     */
    public static OpenClawConfig applyLoggingDefaults(OpenClawConfig cfg) {
        var logging = cfg.getLogging();
        if (logging == null || logging.getRedactSensitive() != null) {
            return cfg;
        }
        OpenClawConfig next = shallowCopy(cfg);
        var nextLogging = copyLogging(logging);
        nextLogging.setRedactSensitive("tools");
        next.setLogging(nextLogging);
        return next;
    }

    // =========================================================================
    // Compaction defaults
    // =========================================================================

    /**
     * Set default compaction.mode to "safeguard" if not set.
     */
    public static OpenClawConfig applyCompactionDefaults(OpenClawConfig cfg) {
        var defaults = cfg.getAgents() != null ? cfg.getAgents().getDefaults() : null;
        if (defaults == null)
            return cfg;

        var compaction = defaults.getCompaction();
        if (compaction != null && compaction.getMode() != null) {
            return cfg;
        }

        OpenClawConfig next = shallowCopy(cfg);
        var nextAgents = copyAgentsConfig(cfg.getAgents());
        var nextDefaults = copyAgentDefaults(defaults);
        var nextCompaction = compaction != null ? copyCompaction(compaction)
                : new OpenClawConfig.AgentCompactionConfig();
        nextCompaction.setMode("safeguard");
        nextDefaults.setCompaction(nextCompaction);
        nextAgents.setDefaults(nextDefaults);
        next.setAgents(nextAgents);
        return next;
    }

    // =========================================================================
    // Cost helpers
    // =========================================================================

    private static OpenClawConfig.ModelCost resolveModelCost(OpenClawConfig.ModelCost raw) {
        var cost = new OpenClawConfig.ModelCost();
        cost.setInput(raw != null ? raw.getInput() : 0);
        cost.setOutput(raw != null ? raw.getOutput() : 0);
        cost.setCacheRead(raw != null ? raw.getCacheRead() : 0);
        cost.setCacheWrite(raw != null ? raw.getCacheWrite() : 0);
        return cost;
    }

    private static boolean costEquals(OpenClawConfig.ModelCost a, OpenClawConfig.ModelCost b) {
        return a.getInput() == b.getInput()
                && a.getOutput() == b.getOutput()
                && a.getCacheRead() == b.getCacheRead()
                && a.getCacheWrite() == b.getCacheWrite();
    }

    // =========================================================================
    // Shallow copy helpers (Lombok @Data gives us setters)
    // =========================================================================

    private static OpenClawConfig shallowCopy(OpenClawConfig src) {
        var dest = new OpenClawConfig();
        dest.setMeta(src.getMeta());
        dest.setAuth(src.getAuth());
        dest.setEnv(src.getEnv());
        dest.setWizard(src.getWizard());
        dest.setDiagnostics(src.getDiagnostics());
        dest.setLogging(src.getLogging());
        dest.setUpdate(src.getUpdate());
        dest.setBrowser(src.getBrowser());
        dest.setUi(src.getUi());
        dest.setSkills(src.getSkills());
        dest.setPlugins(src.getPlugins());
        dest.setModels(src.getModels());
        dest.setNodeHost(src.getNodeHost());
        dest.setAgents(src.getAgents());
        dest.setTools(src.getTools());
        dest.setBindings(src.getBindings());
        dest.setBroadcast(src.getBroadcast());
        dest.setAudio(src.getAudio());
        dest.setMessages(src.getMessages());
        dest.setCommands(src.getCommands());
        dest.setApprovals(src.getApprovals());
        dest.setSession(src.getSession());
        dest.setWeb(src.getWeb());
        dest.setChannels(src.getChannels());
        dest.setCron(src.getCron());
        dest.setHooks(src.getHooks());
        dest.setDiscovery(src.getDiscovery());
        dest.setCanvasHost(src.getCanvasHost());
        dest.setTalk(src.getTalk());
        dest.setGateway(src.getGateway());
        dest.setMemory(src.getMemory());
        return dest;
    }

    private static OpenClawConfig.MessagesConfig copyMessages(OpenClawConfig.MessagesConfig src) {
        var dest = new OpenClawConfig.MessagesConfig();
        dest.setMessagePrefix(src.getMessagePrefix());
        dest.setResponsePrefix(src.getResponsePrefix());
        dest.setGroupChat(src.getGroupChat());
        dest.setQueue(src.getQueue());
        dest.setInbound(src.getInbound());
        dest.setAckReaction(src.getAckReaction());
        dest.setAckReactionScope(src.getAckReactionScope());
        dest.setRemoveAckAfterReply(src.getRemoveAckAfterReply());
        dest.setTts(src.getTts());
        return dest;
    }

    private static OpenClawConfig.SessionConfig copySession(OpenClawConfig.SessionConfig src) {
        var dest = new OpenClawConfig.SessionConfig();
        dest.setScope(src.getScope());
        dest.setDmScope(src.getDmScope());
        dest.setIdentityLinks(src.getIdentityLinks());
        dest.setResetTriggers(src.getResetTriggers());
        dest.setIdleMinutes(src.getIdleMinutes());
        dest.setReset(src.getReset());
        dest.setResetByType(src.getResetByType());
        dest.setResetByChannel(src.getResetByChannel());
        dest.setStore(src.getStore());
        dest.setTypingIntervalSeconds(src.getTypingIntervalSeconds());
        dest.setTypingMode(src.getTypingMode());
        dest.setMainKey(src.getMainKey());
        dest.setSendPolicy(src.getSendPolicy());
        dest.setAgentToAgent(src.getAgentToAgent());
        return dest;
    }

    private static OpenClawConfig.AgentsConfig copyAgentsConfig(OpenClawConfig.AgentsConfig src) {
        var dest = new OpenClawConfig.AgentsConfig();
        dest.setDefaults(src.getDefaults());
        dest.setList(src.getList());
        return dest;
    }

    private static OpenClawConfig.AgentDefaultsConfig copyAgentDefaults(OpenClawConfig.AgentDefaultsConfig src) {
        var dest = new OpenClawConfig.AgentDefaultsConfig();
        dest.setModel(src.getModel());
        dest.setImageModel(src.getImageModel());
        dest.setModels(src.getModels());
        dest.setWorkspace(src.getWorkspace());
        dest.setRepoRoot(src.getRepoRoot());
        dest.setSkipBootstrap(src.getSkipBootstrap());
        dest.setBootstrapMaxChars(src.getBootstrapMaxChars());
        dest.setUserTimezone(src.getUserTimezone());
        dest.setTimeFormat(src.getTimeFormat());
        dest.setEnvelopeTimezone(src.getEnvelopeTimezone());
        dest.setEnvelopeTimestamp(src.getEnvelopeTimestamp());
        dest.setEnvelopeElapsed(src.getEnvelopeElapsed());
        dest.setContextTokens(src.getContextTokens());
        dest.setCliBackends(src.getCliBackends());
        dest.setContextPruning(src.getContextPruning());
        dest.setCompaction(src.getCompaction());
        dest.setMemorySearch(src.getMemorySearch());
        dest.setThinkingDefault(src.getThinkingDefault());
        dest.setVerboseDefault(src.getVerboseDefault());
        dest.setElevatedDefault(src.getElevatedDefault());
        dest.setBlockStreamingDefault(src.getBlockStreamingDefault());
        dest.setBlockStreamingBreak(src.getBlockStreamingBreak());
        dest.setBlockStreamingChunk(src.getBlockStreamingChunk());
        dest.setBlockStreamingCoalesce(src.getBlockStreamingCoalesce());
        dest.setHumanDelay(src.getHumanDelay());
        dest.setTimeoutSeconds(src.getTimeoutSeconds());
        dest.setMediaMaxMb(src.getMediaMaxMb());
        dest.setTypingIntervalSeconds(src.getTypingIntervalSeconds());
        dest.setTypingMode(src.getTypingMode());
        dest.setHeartbeat(src.getHeartbeat());
        dest.setMaxConcurrent(src.getMaxConcurrent());
        dest.setSubagents(src.getSubagents());
        dest.setSandbox(src.getSandbox());
        return dest;
    }

    private static OpenClawConfig.SubagentDefaultsConfig copySubagentDefaults(
            OpenClawConfig.SubagentDefaultsConfig src) {
        var dest = new OpenClawConfig.SubagentDefaultsConfig();
        dest.setMaxConcurrent(src.getMaxConcurrent());
        dest.setArchiveAfterMinutes(src.getArchiveAfterMinutes());
        dest.setModel(src.getModel());
        dest.setThinking(src.getThinking());
        return dest;
    }

    private static OpenClawConfig.LoggingConfig copyLogging(OpenClawConfig.LoggingConfig src) {
        var dest = new OpenClawConfig.LoggingConfig();
        dest.setLevel(src.getLevel());
        dest.setFile(src.getFile());
        dest.setConsoleLevel(src.getConsoleLevel());
        dest.setConsoleStyle(src.getConsoleStyle());
        dest.setRedactSensitive(src.getRedactSensitive());
        dest.setRedactPatterns(src.getRedactPatterns());
        return dest;
    }

    private static OpenClawConfig.AgentCompactionConfig copyCompaction(OpenClawConfig.AgentCompactionConfig src) {
        var dest = new OpenClawConfig.AgentCompactionConfig();
        dest.setMode(src.getMode());
        dest.setReserveTokensFloor(src.getReserveTokensFloor());
        dest.setMaxHistoryShare(src.getMaxHistoryShare());
        dest.setMemoryFlush(src.getMemoryFlush());
        return dest;
    }
}

package com.openclaw.agent;

import com.openclaw.agent.models.AnthropicProvider;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.models.OpenAICompatibleProvider;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.tools.OpenClawToolFactory;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.agent.tools.builtin.ExecTool;
import com.openclaw.agent.tools.builtin.FileTools;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Spring configuration for Agent runtime beans.
 */
@Slf4j
@Configuration
public class AgentBeanConfig {

    private final ConfigService configService;

    public AgentBeanConfig(ConfigService configService) {
        this.configService = configService;
    }

    @Bean
    public ToolRegistry toolRegistry(ModelProviderRegistry modelProviderRegistry) {
        ToolRegistry registry = new ToolRegistry();

        // Register built-in coding tools
        registry.register(new ExecTool());
        registry.register(FileTools.readFile());
        registry.register(FileTools.writeFile());
        registry.register(FileTools.listDir());
        registry.register(FileTools.grepSearch());

        // Register OpenClaw extension tools (browser, web, memory, message, etc.)
        try {
            OpenClawConfig config = configService.loadConfig();
            var extensionTools = OpenClawToolFactory.createTools(
                    OpenClawToolFactory.OpenClawToolOptions.builder()
                            .config(config)
                            .modelProviderRegistry(modelProviderRegistry)
                            .build());
            registry.registerAll(extensionTools);
            log.info("Registered {} extension tools", extensionTools.size());
        } catch (Exception e) {
            log.warn("Failed to register extension tools: {}", e.getMessage());
        }

        log.info("Registered {} total tools", registry.size());
        return registry;
    }

    @Bean
    public ModelProviderRegistry modelProviderRegistry() {
        ModelProviderRegistry registry = new ModelProviderRegistry();

        // Load aliases and providers from config
        try {
            OpenClawConfig config = configService.loadConfig();
            registry.loadAliasesFromConfig(config);
            registerProvidersFromConfig(registry, config);
        } catch (Exception e) {
            log.debug("Config not loaded for model aliases: {}", e.getMessage());
        }

        // Register providers from environment variables (may override config-based ones)
        registerProvidersFromEnv(registry);

        log.info("Registered {} model providers, {} aliases",
                registry.size(), registry.getAliases().size());
        return registry;
    }

    @Bean
    public AgentRunner agentRunner(ModelProviderRegistry modelProviderRegistry, ToolRegistry toolRegistry) {
        return new AgentRunner(modelProviderRegistry, toolRegistry);
    }

    private void registerProvidersFromConfig(ModelProviderRegistry registry, OpenClawConfig config) {
        if (config.getModels() == null || config.getModels().getProviders() == null) {
            return;
        }
        config.getModels().getProviders().forEach((id, pc) -> {
            if (!pc.isEnabled()) return;
            String apiKey = pc.getApiKey();
            if (apiKey == null || apiKey.isBlank()) return;

            String baseUrl = resolveBaseUrl(pc);
            ModelProvider provider = createProvider(id, apiKey, baseUrl);
            registry.register(provider);
            log.info("Registered provider from config: {} (baseUrl={})", id, provider.getApiBaseUrl());
        });
    }

    private void registerProvidersFromEnv(ModelProviderRegistry registry) {
        registerEnvProvider(registry, "anthropic", "ANTHROPIC_API_KEY", "ANTHROPIC_BASE_URL");
        registerEnvProvider(registry, "openai", "OPENAI_API_KEY", "OPENAI_BASE_URL");

        // Ollama (no API key needed)
        String ollamaUrl = System.getenv().getOrDefault("OLLAMA_BASE_URL", "http://127.0.0.1:11434/v1");
        registry.register(new OpenAICompatibleProvider("ollama", null, ollamaUrl));
    }

    private void registerEnvProvider(ModelProviderRegistry registry, String id, String keyEnv, String urlEnv) {
        String apiKey = System.getenv(keyEnv);
        if (apiKey == null || apiKey.isBlank()) return;

        ModelProvider provider = createProvider(id, apiKey, System.getenv(urlEnv));
        registry.register(provider);
        log.info("Registered provider from env: {} (baseUrl={})", id, provider.getApiBaseUrl());
    }

    private static String resolveBaseUrl(OpenClawConfig.ProviderConfig pc) {
        String url = pc.getBaseUrl();
        return (url != null && !url.isBlank()) ? url : pc.getApiBaseUrl();
    }

    private static ModelProvider createProvider(String id, String apiKey, String baseUrl) {
        return "anthropic".equals(id)
                ? new AnthropicProvider(apiKey, baseUrl)
                : new OpenAICompatibleProvider(id, apiKey, baseUrl);
    }
}

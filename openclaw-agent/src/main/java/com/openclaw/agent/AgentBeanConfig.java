package com.openclaw.agent;

import com.openclaw.agent.models.AnthropicProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.models.OpenAICompatibleProvider;
import com.openclaw.agent.runtime.AgentRunner;
import com.openclaw.agent.tools.ToolRegistry;
import com.openclaw.agent.tools.builtin.ExecTool;
import com.openclaw.agent.tools.builtin.FileTools;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import jakarta.annotation.PostConstruct;
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
    public ToolRegistry toolRegistry() {
        ToolRegistry registry = new ToolRegistry();

        // Register built-in tools
        registry.register(new ExecTool());
        registry.register(FileTools.readFile());
        registry.register(FileTools.writeFile());
        registry.register(FileTools.listDir());

        log.info("Registered {} built-in tools", registry.size());
        return registry;
    }

    @Bean
    public ModelProviderRegistry modelProviderRegistry() {
        ModelProviderRegistry registry = new ModelProviderRegistry();

        // Load aliases from config
        try {
            OpenClawConfig config = configService.loadConfig();
            registry.loadAliasesFromConfig(config);
        } catch (Exception e) {
            log.debug("Config not loaded for model aliases: {}", e.getMessage());
        }

        // Register providers from environment variables
        registerProvidersFromEnv(registry);

        log.info("Registered {} model providers, {} aliases",
                registry.size(), registry.getAliases().size());
        return registry;
    }

    @Bean
    public AgentRunner agentRunner(ModelProviderRegistry modelProviderRegistry, ToolRegistry toolRegistry) {
        return new AgentRunner(modelProviderRegistry, toolRegistry);
    }

    private void registerProvidersFromEnv(ModelProviderRegistry registry) {
        String anthropicKey = System.getenv("ANTHROPIC_API_KEY");
        if (anthropicKey != null && !anthropicKey.isBlank()) {
            String anthropicBaseUrl = System.getenv("ANTHROPIC_BASE_URL");
            if (anthropicBaseUrl != null && !anthropicBaseUrl.isBlank()) {
                registry.register(new AnthropicProvider(anthropicKey, anthropicBaseUrl));
                log.info("Anthropic provider registered with custom base URL: {}", anthropicBaseUrl);
            } else {
                registry.register(new AnthropicProvider(anthropicKey));
                log.info("Anthropic provider registered with default base URL");
            }
        }

        String openaiKey = System.getenv("OPENAI_API_KEY");
        if (openaiKey != null && !openaiKey.isBlank()) {
            registry.register(new OpenAICompatibleProvider(openaiKey));
        }

        // Ollama (no API key needed)
        String ollamaUrl = System.getenv().getOrDefault("OLLAMA_BASE_URL", "http://127.0.0.1:11434/v1");
        registry.register(new OpenAICompatibleProvider("ollama", null, ollamaUrl));
    }
}

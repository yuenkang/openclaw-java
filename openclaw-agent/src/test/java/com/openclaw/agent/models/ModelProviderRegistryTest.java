package com.openclaw.agent.models;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ModelProviderRegistry}.
 */
class ModelProviderRegistryTest {

    private ModelProviderRegistry registry;

    /**
     * Minimal ModelProvider stub for testing registration.
     */
    private static ModelProvider stubProvider(String id) {
        return new ModelProvider() {
            @Override
            public String getId() {
                return id;
            }

            @Override
            public String getApiBaseUrl() {
                return "https://api." + id + ".com";
            }

            @Override
            public List<ModelInfo> listModels() {
                var info = new ModelInfo();
                info.setId(id + "/test-model");
                info.setName(id + "/test-model");
                info.setContextWindow(128000);
                info.setMaxTokens(4096);
                return List.of(info);
            }

            @Override
            public CompletableFuture<ChatResponse> chat(ChatRequest request) {
                return CompletableFuture.completedFuture(null);
            }
        };
    }

    @BeforeEach
    void setUp() {
        registry = new ModelProviderRegistry();
    }

    // =========================================================================
    // Registration
    // =========================================================================

    @Test
    void register_addsProvider() {
        registry.register(stubProvider("test"));
        assertTrue(registry.hasProvider("test"));
        assertEquals(1, registry.size());
    }

    @Test
    void register_multipleProviders() {
        registry.register(stubProvider("a"));
        registry.register(stubProvider("b"));
        assertEquals(2, registry.size());
        assertTrue(registry.hasProvider("a"));
        assertTrue(registry.hasProvider("b"));
    }

    @Test
    void getProvider_returnsRegistered() {
        registry.register(stubProvider("openai"));
        var provider = registry.getProvider("openai");
        assertNotNull(provider);
        assertEquals("openai", provider.getId());
    }

    @Test
    void getProvider_unknownReturnsNull() {
        assertNull(registry.getProvider("nonexistent"));
    }

    @Test
    void hasProvider_falseWhenEmpty() {
        assertFalse(registry.hasProvider("anything"));
    }

    // =========================================================================
    // Aliases
    // =========================================================================

    @Test
    void defaultAliases_exist() {
        var aliases = registry.getAliases();
        assertTrue(aliases.containsKey("sonnet"));
        assertTrue(aliases.containsKey("opus"));
        assertTrue(aliases.containsKey("gpt"));
        assertTrue(aliases.containsKey("gemini"));
    }

    @Test
    void addAlias_resolvesCorrectly() {
        registry.addAlias("my-model", "openai/gpt-5");
        assertEquals("openai/gpt-5", registry.resolveModelId("my-model"));
    }

    @Test
    void resolveModelId_unknownAlias_returnsOriginal() {
        assertEquals("some-model", registry.resolveModelId("some-model"));
    }

    @Test
    void resolve_withProvider_returnsProvider() {
        registry.register(stubProvider("anthropic"));
        // "sonnet" is a default alias → "anthropic/claude-sonnet-4-5"
        var provider = registry.resolve("sonnet");
        assertNotNull(provider);
        assertEquals("anthropic", provider.getId());
    }

    @Test
    void resolve_directProviderId() {
        registry.register(stubProvider("openai"));
        var provider = registry.resolve("openai/gpt-5");
        assertNotNull(provider);
        assertEquals("openai", provider.getId());
    }

    // =========================================================================
    // Provider IDs
    // =========================================================================

    @Test
    void getProviderIds_returnsAllRegistered() {
        registry.register(stubProvider("a"));
        registry.register(stubProvider("b"));
        var ids = registry.getProviderIds();
        assertEquals(2, ids.size());
        assertTrue(ids.contains("a"));
        assertTrue(ids.contains("b"));
    }

    // =========================================================================
    // listAllModels
    // =========================================================================

    @Test
    void listAllModels_aggregatesFromAllProviders() {
        registry.register(stubProvider("a"));
        registry.register(stubProvider("b"));
        var models = registry.listAllModels();
        assertEquals(2, models.size());
    }

    @Test
    void listAllModels_emptyWhenNoProviders() {
        assertTrue(registry.listAllModels().isEmpty());
    }
}

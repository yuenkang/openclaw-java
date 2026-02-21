package com.openclaw.plugin.tools;

import com.openclaw.plugin.registry.PluginRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for PluginToolResolver.
 */
class PluginToolResolverTest {

    PluginRegistry registry;

    @BeforeEach
    void setUp() {
        registry = new PluginRegistry();
    }

    @Test
    void resolveEmptyRegistry() {
        var tools = PluginToolResolver.resolvePluginTools(registry, Set.of(), null);
        assertTrue(tools.isEmpty());
    }

    @Test
    void resolveRegisteredTools() {
        registry.registerTool(PluginRegistry.PluginToolRegistration.builder()
                .pluginId("my-plugin")
                .names(List.of("custom_search", "custom_fetch"))
                .optional(false)
                .build());

        var tools = PluginToolResolver.resolvePluginTools(registry, Set.of(), null);
        assertEquals(2, tools.size());
        assertEquals("custom_search", tools.get(0).getName());
        assertEquals("custom_fetch", tools.get(1).getName());
        assertEquals("my-plugin", tools.get(0).getPluginId());
    }

    @Test
    void conflictWithExistingToolName() {
        registry.registerTool(PluginRegistry.PluginToolRegistration.builder()
                .pluginId("my-plugin")
                .names(List.of("bash"))
                .optional(false)
                .build());

        var tools = PluginToolResolver.resolvePluginTools(registry, Set.of("bash"), null);
        assertTrue(tools.isEmpty()); // conflict, not added
    }

    @Test
    void optionalToolNotInAllowlist() {
        registry.registerTool(PluginRegistry.PluginToolRegistration.builder()
                .pluginId("my-plugin")
                .names(List.of("optional_tool"))
                .optional(true)
                .build());

        // No allowlist → optional tools not included
        var tools = PluginToolResolver.resolvePluginTools(registry, Set.of(), null);
        assertTrue(tools.isEmpty());
    }

    @Test
    void optionalToolInAllowlist() {
        registry.registerTool(PluginRegistry.PluginToolRegistration.builder()
                .pluginId("my-plugin")
                .names(List.of("optional_tool"))
                .optional(true)
                .build());

        var tools = PluginToolResolver.resolvePluginTools(
                registry, Set.of(), List.of("optional_tool"));
        assertEquals(1, tools.size());
        assertTrue(tools.get(0).isOptional());
    }

    @Test
    void normalizeToolName() {
        assertEquals("hello_world", PluginToolResolver.normalizeToolName("Hello-World"));
        assertEquals("", PluginToolResolver.normalizeToolName(null));
    }
}

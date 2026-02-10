package com.openclaw.agent.tools;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Registry for agent tools.
 * Corresponds to TypeScript's tool resolution in pi-tools.ts /
 * openclaw-tools.ts.
 */
@Slf4j
public class ToolRegistry {

    private final Map<String, AgentTool> tools = new ConcurrentHashMap<>();

    /**
     * Register a tool. Overwrites any existing tool with the same name.
     */
    public void register(AgentTool tool) {
        tools.put(tool.getName(), tool);
        log.debug("Registered tool: {}", tool.getName());
    }

    /**
     * Register multiple tools, skipping names that already exist.
     */
    public void registerAll(Collection<AgentTool> toolList) {
        for (AgentTool tool : toolList) {
            tools.putIfAbsent(tool.getName(), tool);
        }
    }

    /**
     * Get a tool by name.
     */
    public Optional<AgentTool> get(String name) {
        return Optional.ofNullable(tools.get(name));
    }

    /**
     * List all registered tool names.
     */
    public Set<String> getToolNames() {
        return Collections.unmodifiableSet(tools.keySet());
    }

    /**
     * List all registered tools.
     */
    public List<AgentTool> listAll() {
        return new ArrayList<>(tools.values());
    }

    /**
     * Convert all tools to LLM-compatible definitions (name + description +
     * schema).
     */
    public List<Map<String, Object>> toDefinitions() {
        return tools.values().stream()
                .map(tool -> {
                    Map<String, Object> def = new LinkedHashMap<>();
                    def.put("name", tool.getName());
                    def.put("description", tool.getDescription());
                    def.put("input_schema", tool.getParameterSchema());
                    return def;
                })
                .collect(Collectors.toList());
    }

    /**
     * Returns the number of registered tools.
     */
    public int size() {
        return tools.size();
    }
}

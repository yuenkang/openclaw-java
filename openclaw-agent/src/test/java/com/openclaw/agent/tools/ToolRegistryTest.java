package com.openclaw.agent.tools;

import com.openclaw.agent.tools.builtin.ExecTool;
import com.openclaw.agent.tools.builtin.FileTools;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class ToolRegistryTest {

    private ToolRegistry registry;
    private final ObjectMapper mapper = new ObjectMapper();

    @BeforeEach
    void setUp() {
        registry = new ToolRegistry();
    }

    @Test
    void register_addsToolToRegistry() {
        registry.register(new ExecTool());

        assertEquals(1, registry.size());
        assertTrue(registry.get("exec").isPresent());
    }

    @Test
    void register_multipleTools() {
        registry.register(new ExecTool());
        registry.register(FileTools.readFile());
        registry.register(FileTools.writeFile());
        registry.register(FileTools.listDir());
        registry.register(FileTools.grepSearch());

        assertEquals(5, registry.size());
        assertTrue(registry.get("grep_search").isPresent());
    }

    @Test
    void get_unknownTool_returnsEmpty() {
        Optional<AgentTool> tool = registry.get("nonexistent");
        assertTrue(tool.isEmpty());
    }

    @Test
    void toDefinitions_returnsCorrectStructure() {
        registry.register(new ExecTool());

        List<Map<String, Object>> defs = registry.toDefinitions();
        assertEquals(1, defs.size());

        Map<String, Object> def = defs.get(0);
        assertEquals("exec", def.get("name"));
        assertNotNull(def.get("description"));
        assertNotNull(def.get("input_schema"));
    }

    @Test
    void execTool_executesCommand() throws Exception {
        ExecTool exec = new ExecTool();

        ObjectNode params = mapper.createObjectNode();
        params.put("command", "echo hello");

        AgentTool.ToolContext ctx = AgentTool.ToolContext.builder()
                .parameters(params)
                .cwd(System.getProperty("user.dir"))
                .build();

        AgentTool.ToolResult result = exec.execute(ctx).get();

        assertTrue(result.isSuccess());
        assertTrue(result.getOutput().contains("hello"));
    }

    @Test
    void execTool_failingCommand_returnsError() throws Exception {
        ExecTool exec = new ExecTool();

        ObjectNode params = mapper.createObjectNode();
        params.put("command", "exit 1");

        AgentTool.ToolContext ctx = AgentTool.ToolContext.builder()
                .parameters(params)
                .cwd(System.getProperty("user.dir"))
                .build();

        AgentTool.ToolResult result = exec.execute(ctx).get();

        assertFalse(result.isSuccess());
    }
}

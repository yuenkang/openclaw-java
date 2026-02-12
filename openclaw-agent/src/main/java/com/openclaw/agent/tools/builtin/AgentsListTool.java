package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Agents list tool — list available agent IDs for sub-agent spawning.
 * Corresponds to TypeScript's tools/agents-list-tool.ts.
 */
@Slf4j
public class AgentsListTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Override
    public String getName() {
        return "agents_list";
    }

    @Override
    public String getDescription() {
        return "List agent ids you can target with sessions_spawn (based on allowlists).";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        schema.putObject("properties");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            OpenClawConfig config = context.getConfig();
            List<AgentInfo> agents = new ArrayList<>();
            agents.add(new AgentInfo("main", null));

            if (config != null && config.getAgents() != null) {
                for (OpenClawConfig.AgentEntry entry : config.getAgents().getEntries()) {
                    String id = entry.getId();
                    if (id != null && !id.isBlank() && !id.equals("main")) {
                        agents.add(new AgentInfo(id, entry.getName()));
                    }
                }
            }

            agents.sort((a, b) -> {
                if ("main".equals(a.id))
                    return -1;
                if ("main".equals(b.id))
                    return 1;
                return a.id.compareTo(b.id);
            });

            ObjectNode result = MAPPER.createObjectNode();
            result.put("requester", "main");
            ArrayNode arr = result.putArray("agents");
            for (AgentInfo e : agents) {
                ObjectNode n = arr.addObject();
                n.put("id", e.id);
                if (e.name != null)
                    n.put("name", e.name);
                n.put("configured", true);
            }

            return ToolResult.ok(ToolParamUtils.toJsonString(result), result);

        } catch (Exception e) {
            log.error("agents_list error: {}", e.getMessage(), e);
            return ToolResult.fail("Agents list error: " + e.getMessage());
        }
    }

    private static class AgentInfo {
        final String id;
        final String name;

        AgentInfo(String id, String name) {
            this.id = id;
            this.name = name;
        }
    }
}

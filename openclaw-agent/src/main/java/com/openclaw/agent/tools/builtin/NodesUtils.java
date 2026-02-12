package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

/**
 * Node resolution utilities for the nodes tool.
 * Corresponds to TypeScript agents/tools/nodes-utils.ts.
 */
@Slf4j
public final class NodesUtils {

    private NodesUtils() {
    }

    /**
     * A node list entry.
     */
    public record NodeInfo(
            String nodeId,
            String displayName,
            String platform,
            String version,
            String coreVersion,
            String uiVersion,
            String remoteIp,
            String deviceFamily,
            String modelIdentifier,
            List<String> caps,
            List<String> commands,
            boolean paired,
            boolean connected) {
    }

    /**
     * Parse a node list from a gateway response.
     */
    public static List<NodeInfo> parseNodeList(JsonNode value) {
        if (value == null || !value.isObject())
            return List.of();
        JsonNode nodesArr = value.get("nodes");
        if (nodesArr == null || !nodesArr.isArray())
            return List.of();

        List<NodeInfo> result = new ArrayList<>();
        for (JsonNode n : nodesArr) {
            if (!n.isObject())
                continue;
            result.add(new NodeInfo(
                    n.path("nodeId").asText(""),
                    n.path("displayName").asText(null),
                    n.path("platform").asText(null),
                    n.path("version").asText(null),
                    n.path("coreVersion").asText(null),
                    n.path("uiVersion").asText(null),
                    n.path("remoteIp").asText(null),
                    n.path("deviceFamily").asText(null),
                    n.path("modelIdentifier").asText(null),
                    parseStringArray(n.get("caps")),
                    parseStringArray(n.get("commands")),
                    n.path("paired").asBoolean(false),
                    n.path("connected").asBoolean(false)));
        }
        return result;
    }

    /**
     * Normalize a node key for matching.
     */
    public static String normalizeNodeKey(String value) {
        return value.toLowerCase().replaceAll("[^a-z0-9]+", "-")
                .replaceAll("^-+", "").replaceAll("-+$", "");
    }

    /**
     * Pick the default node from a node list.
     * Prefers connected nodes with canvas capability, then local Mac nodes.
     */
    public static NodeInfo pickDefaultNode(List<NodeInfo> nodes) {
        List<NodeInfo> withCanvas = nodes.stream()
                .filter(n -> n.caps == null || n.caps.isEmpty() || n.caps.contains("canvas"))
                .toList();
        if (withCanvas.isEmpty())
            return null;

        List<NodeInfo> connected = withCanvas.stream().filter(NodeInfo::connected).toList();
        List<NodeInfo> candidates = connected.isEmpty() ? withCanvas : connected;
        if (candidates.size() == 1)
            return candidates.get(0);

        List<NodeInfo> local = candidates.stream()
                .filter(n -> n.platform != null && n.platform.toLowerCase().startsWith("mac")
                        && n.nodeId.startsWith("mac-"))
                .toList();
        if (local.size() == 1)
            return local.get(0);

        return null;
    }

    /**
     * Resolve a node ID from a list by query string.
     *
     * @param nodes        Available nodes
     * @param query        Node query (ID, name, IP, or prefix)
     * @param allowDefault If true and query is blank, pick default
     * @return Resolved node ID
     * @throws IllegalArgumentException if node not found or ambiguous
     */
    public static String resolveNodeIdFromList(List<NodeInfo> nodes, String query, boolean allowDefault) {
        String q = (query == null ? "" : query).trim();
        if (q.isEmpty()) {
            if (allowDefault) {
                NodeInfo picked = pickDefaultNode(nodes);
                if (picked != null)
                    return picked.nodeId;
            }
            throw new IllegalArgumentException("node required");
        }

        String qNorm = normalizeNodeKey(q);
        List<NodeInfo> matches = nodes.stream().filter(n -> {
            if (n.nodeId.equals(q))
                return true;
            if (n.remoteIp != null && n.remoteIp.equals(q))
                return true;
            String name = n.displayName != null ? n.displayName : "";
            if (!name.isEmpty() && normalizeNodeKey(name).equals(qNorm))
                return true;
            if (q.length() >= 6 && n.nodeId.startsWith(q))
                return true;
            return false;
        }).toList();

        if (matches.size() == 1)
            return matches.get(0).nodeId;
        if (matches.isEmpty()) {
            String known = nodes.stream()
                    .map(n -> n.displayName != null ? n.displayName : (n.remoteIp != null ? n.remoteIp : n.nodeId))
                    .reduce((a, b) -> a + ", " + b).orElse("");
            throw new IllegalArgumentException("unknown node: " + q +
                    (known.isEmpty() ? "" : " (known: " + known + ")"));
        }
        String matchNames = matches.stream()
                .map(n -> n.displayName != null ? n.displayName : (n.remoteIp != null ? n.remoteIp : n.nodeId))
                .reduce((a, b) -> a + ", " + b).orElse("");
        throw new IllegalArgumentException("ambiguous node: " + q + " (matches: " + matchNames + ")");
    }

    private static List<String> parseStringArray(JsonNode node) {
        if (node == null || !node.isArray())
            return List.of();
        List<String> result = new ArrayList<>();
        for (JsonNode item : node) {
            if (item.isTextual())
                result.add(item.asText());
        }
        return result;
    }
}

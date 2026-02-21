package com.openclaw.autoreply.reply;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Parse and handle /ptt (push-to-talk) chat commands.
 * Mirrors {@code auto-reply/reply/commands-ptt.ts}.
 */
public final class CommandsPtt {

    private CommandsPtt() {
    }

    /** Node summary from the gateway. */
    public record NodeSummary(
            String nodeId,
            String displayName,
            String platform,
            String deviceFamily,
            String remoteIp,
            boolean connected) {
    }

    /** PTT command mapping. */
    private static final Map<String, String> PTT_COMMANDS = Map.of(
            "start", "talk.ptt.start",
            "stop", "talk.ptt.stop",
            "once", "talk.ptt.once",
            "cancel", "talk.ptt.cancel");

    /** Parsed PTT args. */
    public record PttArgs(String action, String node) {
    }

    /* ── helpers ────────────────────────────────────────────── */

    static String normalizeNodeKey(String value) {
        return value.toLowerCase()
                .replaceAll("[^a-z0-9]+", "-")
                .replaceAll("^-+", "")
                .replaceAll("-+$", "");
    }

    /**
     * Check if a node is an iOS device.
     */
    public static boolean isIOSNode(NodeSummary node) {
        String platform = node.platform() != null ? node.platform().toLowerCase() : "";
        String family = node.deviceFamily() != null ? node.deviceFamily().toLowerCase() : "";
        return platform.startsWith("ios")
                || family.contains("iphone")
                || family.contains("ipad")
                || family.contains("ios");
    }

    /**
     * Describe a list of nodes as a comma-separated string.
     */
    public static String describeNodes(List<NodeSummary> nodes) {
        return nodes.stream()
                .map(n -> {
                    if (n.displayName() != null && !n.displayName().isEmpty())
                        return n.displayName();
                    if (n.remoteIp() != null && !n.remoteIp().isEmpty())
                        return n.remoteIp();
                    return n.nodeId();
                })
                .filter(s -> s != null && !s.isEmpty())
                .collect(Collectors.joining(", "));
    }

    /**
     * Resolve a node ID from a query string against a list of known nodes.
     *
     * @throws IllegalArgumentException if the node is unknown or ambiguous
     */
    public static String resolveNodeId(List<NodeSummary> nodes, String query) {
        String trimmed = query != null ? query.trim() : "";
        if (!trimmed.isEmpty()) {
            String qNorm = normalizeNodeKey(trimmed);
            List<NodeSummary> matches = nodes.stream().filter(node -> {
                if (node.nodeId().equals(trimmed))
                    return true;
                if (node.remoteIp() != null && node.remoteIp().equals(trimmed))
                    return true;
                String name = node.displayName() != null ? node.displayName() : "";
                if (!name.isEmpty() && normalizeNodeKey(name).equals(qNorm))
                    return true;
                return trimmed.length() >= 6 && node.nodeId().startsWith(trimmed);
            }).toList();

            if (matches.size() == 1)
                return matches.get(0).nodeId();
            String known = describeNodes(nodes);
            if (matches.isEmpty()) {
                throw new IllegalArgumentException(
                        "unknown node: " + trimmed + (known.isEmpty() ? "" : " (known: " + known + ")"));
            }
            throw new IllegalArgumentException(
                    "ambiguous node: " + trimmed + " (matches: " + describeNodes(matches) + ")");
        }

        // Auto-resolve: prefer iOS → connected → any
        List<NodeSummary> ios = nodes.stream().filter(CommandsPtt::isIOSNode).toList();
        List<NodeSummary> iosConnected = ios.stream().filter(NodeSummary::connected).toList();
        List<NodeSummary> iosCandidates = !iosConnected.isEmpty() ? iosConnected : ios;
        if (iosCandidates.size() == 1)
            return iosCandidates.get(0).nodeId();
        if (iosCandidates.size() > 1) {
            throw new IllegalArgumentException(
                    "multiple iOS nodes found (" + describeNodes(iosCandidates) + "); specify node=<id>");
        }

        List<NodeSummary> connected = nodes.stream().filter(NodeSummary::connected).toList();
        List<NodeSummary> fallback = !connected.isEmpty() ? connected : nodes;
        if (fallback.size() == 1)
            return fallback.get(0).nodeId();

        String known = describeNodes(nodes);
        throw new IllegalArgumentException("node required" + (known.isEmpty() ? "" : " (known: " + known + ")"));
    }

    /**
     * Parse /ptt command arguments.
     */
    public static PttArgs parsePTTArgs(String commandBody) {
        String[] tokens = commandBody.trim().split("\\s+");
        String action = null;
        String node = null;
        for (int i = 1; i < tokens.length; i++) {
            String token = tokens[i];
            if (token.isEmpty())
                continue;
            if (token.toLowerCase().startsWith("node=")) {
                node = token.substring("node=".length());
                continue;
            }
            if (action == null)
                action = token;
        }
        return new PttArgs(action, node);
    }

    /**
     * Get the gateway method for a PTT action.
     */
    public static String resolvePttCommand(String action) {
        if (action == null)
            return null;
        return PTT_COMMANDS.get(action.trim().toLowerCase());
    }

    /**
     * Build PTT help text.
     */
    public static String buildPTTHelpText() {
        return "Usage: /ptt <start|stop|once|cancel> [node=<id>]\nExample: /ptt once node=iphone";
    }
}

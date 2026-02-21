package com.openclaw.autoreply.reply;

import java.text.NumberFormat;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Agent runner utility functions — threading tool context, Bun fetch error
 * formatting, usage line formatting, and followup tag resolution.
 * Mirrors {@code auto-reply/reply/agent-runner-utils.ts}.
 */
public final class AgentRunnerUtils {

    private AgentRunnerUtils() {
    }

    private static final Pattern BUN_FETCH_SOCKET_ERROR_RE = Pattern
            .compile("socket connection was closed unexpectedly", Pattern.CASE_INSENSITIVE);

    // --- Threading tool context ---

    /**
     * Build provider-specific threading context for tool auto-injection.
     */
    public static Map<String, Object> buildThreadingToolContext(
            Map<String, Object> sessionCtx,
            Map<String, Object> config) {
        if (config == null)
            return Map.of();
        String rawProvider = sessionCtx.get("Provider") != null
                ? sessionCtx.get("Provider").toString().trim().toLowerCase()
                : null;
        if (rawProvider == null || rawProvider.isEmpty())
            return Map.of();

        Map<String, Object> ctx = new LinkedHashMap<>();
        String channelId = sessionCtx.get("To") != null ? sessionCtx.get("To").toString().trim() : null;
        ctx.put("currentChannelId", channelId);
        ctx.put("currentChannelProvider", rawProvider);
        return ctx;
    }

    // --- Bun fetch socket error ---

    public static boolean isBunFetchSocketError(String message) {
        return message != null && BUN_FETCH_SOCKET_ERROR_RE.matcher(message).find();
    }

    public static String formatBunFetchSocketError(String message) {
        String trimmed = message != null ? message.trim() : "Unknown error";
        return String.join("\n",
                "⚠️ LLM connection failed. This could be due to server issues, network problems, "
                        + "or context length exceeded (e.g., with local LLMs like LM Studio). Original error:",
                "```",
                trimmed,
                "```");
    }

    // --- Usage formatting ---

    /**
     * Format a response usage line for display.
     *
     * @param inputTokens  input token count (nullable)
     * @param outputTokens output token count (nullable)
     * @param showCost     whether to show cost estimate
     * @param costConfig   per-token cost rates (nullable)
     * @return formatted line or null if no usage data
     */
    public static String formatResponseUsageLine(
            Integer inputTokens, Integer outputTokens,
            boolean showCost, Map<String, Double> costConfig) {
        if (inputTokens == null && outputTokens == null)
            return null;
        String inputLabel = inputTokens != null ? formatTokenCount(inputTokens) : "?";
        String outputLabel = outputTokens != null ? formatTokenCount(outputTokens) : "?";

        String suffix = "";
        if (showCost && inputTokens != null && outputTokens != null && costConfig != null) {
            double cost = estimateUsageCost(inputTokens, outputTokens, costConfig);
            suffix = " · est " + formatUsd(cost);
        }
        return "Usage: " + inputLabel + " in / " + outputLabel + " out" + suffix;
    }

    static String formatTokenCount(int tokens) {
        if (tokens < 1000)
            return String.valueOf(tokens);
        if (tokens < 1_000_000)
            return String.format("%.1fk", tokens / 1000.0);
        return String.format("%.2fM", tokens / 1_000_000.0);
    }

    static double estimateUsageCost(int input, int output, Map<String, Double> costConfig) {
        double inputCost = costConfig.getOrDefault("input", 0.0);
        double outputCost = costConfig.getOrDefault("output", 0.0);
        return (input * inputCost + output * outputCost) / 1_000_000.0;
    }

    static String formatUsd(double value) {
        if (value < 0.01)
            return String.format("$%.4f", value);
        return String.format("$%.2f", value);
    }

    // --- Append usage line to payloads ---

    /**
     * Append a usage line to the last text-bearing payload.
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> appendUsageLine(
            List<Map<String, Object>> payloads, String line) {
        int index = -1;
        for (int i = payloads.size() - 1; i >= 0; i--) {
            if (payloads.get(i).get("text") != null) {
                index = i;
                break;
            }
        }
        if (index == -1) {
            return new ArrayList<>(payloads) {
                {
                    add(Map.of("text", line));
                }
            };
        }
        List<Map<String, Object>> updated = new ArrayList<>(payloads);
        Map<String, Object> existing = new LinkedHashMap<>(updated.get(index));
        String existingText = existing.get("text").toString();
        String separator = existingText.endsWith("\n") ? "" : "\n";
        existing.put("text", existingText + separator + line);
        updated.set(index, existing);
        return updated;
    }

    // --- Enforce final tag ---

    /**
     * Whether to enforce a final tag for this provider.
     */
    public static boolean resolveEnforceFinalTag(Map<String, Object> run, String provider) {
        boolean enforce = Boolean.TRUE.equals(run.get("enforceFinalTag"));
        return enforce || isReasoningTagProvider(provider);
    }

    private static boolean isReasoningTagProvider(String provider) {
        if (provider == null)
            return false;
        String lower = provider.trim().toLowerCase();
        return "deepseek".equals(lower) || lower.startsWith("deepseek-");
    }
}

package com.openclaw.gateway.session;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.*;

/**
 * Per-session usage tracking — records token usage and estimated cost
 * for each LLM call. Stored as JSONL alongside transcript files.
 *
 * <p>
 * File layout:
 * </p>
 * 
 * <pre>
 *   ~/.openclaw/state/agents/{agentId}/sessions/{sessionId}-usage.jsonl
 * </pre>
 *
 * <p>
 * Each line:
 * </p>
 * 
 * <pre>
 *   {"timestamp":"...","model":"...","inputTokens":100,"outputTokens":50,...}
 * </pre>
 */
@Slf4j
public class UsageTracker {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * A single usage record for one LLM call.
     */
    public record UsageRecord(
            String timestamp,
            String sessionKey,
            String model,
            int inputTokens,
            int outputTokens,
            int cacheReadTokens,
            int cacheWriteTokens,
            int totalTokens,
            double estimatedCost) {
    }

    /**
     * Aggregated usage summary for a session.
     */
    public record UsageSummary(
            int totalInputTokens,
            int totalOutputTokens,
            int totalCacheReadTokens,
            int totalCacheWriteTokens,
            int totalTokens,
            double totalEstimatedCost,
            int callCount,
            String lastModel) {
    }

    // =========================================================================
    // Cost estimation (rough per-model pricing)
    // =========================================================================

    /**
     * Estimate cost in USD for a single LLM call.
     * Uses approximate pricing per 1M tokens.
     */
    public static double estimateCost(String model, int inputTokens, int outputTokens,
            int cacheReadTokens) {
        if (model == null)
            model = "";
        String m = model.toLowerCase();

        // Per 1M token pricing (USD)
        double inputPer1M;
        double outputPer1M;
        double cacheReadPer1M;

        if (m.contains("claude-sonnet-4") || m.contains("claude-3-5-sonnet") || m.contains("claude-3.5-sonnet")) {
            inputPer1M = 3.0;
            outputPer1M = 15.0;
            cacheReadPer1M = 0.30;
        } else if (m.contains("claude-opus") || m.contains("claude-3-opus")) {
            inputPer1M = 15.0;
            outputPer1M = 75.0;
            cacheReadPer1M = 1.50;
        } else if (m.contains("claude-haiku") || m.contains("claude-3-haiku") || m.contains("claude-3.5-haiku")
                || m.contains("claude-3-5-haiku")) {
            inputPer1M = 0.80;
            outputPer1M = 4.0;
            cacheReadPer1M = 0.08;
        } else if (m.contains("gpt-4o-mini")) {
            inputPer1M = 0.15;
            outputPer1M = 0.60;
            cacheReadPer1M = 0.075;
        } else if (m.contains("gpt-4o")) {
            inputPer1M = 2.50;
            outputPer1M = 10.0;
            cacheReadPer1M = 1.25;
        } else if (m.contains("gpt-4-turbo") || m.contains("gpt-4-1")) {
            inputPer1M = 10.0;
            outputPer1M = 30.0;
            cacheReadPer1M = 2.50;
        } else if (m.contains("o1-mini") || m.contains("o3-mini") || m.contains("o4-mini")) {
            inputPer1M = 1.10;
            outputPer1M = 4.40;
            cacheReadPer1M = 0.275;
        } else if (m.contains("deepseek")) {
            inputPer1M = 0.27;
            outputPer1M = 1.10;
            cacheReadPer1M = 0.07;
        } else if (m.contains("gemini-2.0-flash") || m.contains("gemini-2.5-flash")) {
            inputPer1M = 0.10;
            outputPer1M = 0.40;
            cacheReadPer1M = 0.025;
        } else if (m.contains("gemini-2.5-pro") || m.contains("gemini-pro")) {
            inputPer1M = 1.25;
            outputPer1M = 10.0;
            cacheReadPer1M = 0.3125;
        } else {
            // Fallback: rough average of mid-tier models
            inputPer1M = 3.0;
            outputPer1M = 15.0;
            cacheReadPer1M = 0.30;
        }

        return (inputTokens * inputPer1M + outputTokens * outputPer1M
                + cacheReadTokens * cacheReadPer1M) / 1_000_000.0;
    }

    // =========================================================================
    // Write
    // =========================================================================

    /**
     * Record a usage entry for a single LLM call.
     */
    public static void recordUsage(Path usagePath, String sessionKey, String model,
            int inputTokens, int outputTokens,
            int cacheReadTokens, int cacheWriteTokens) {
        try {
            Files.createDirectories(usagePath.getParent());
            double cost = estimateCost(model, inputTokens, outputTokens, cacheReadTokens);
            int totalTokens = inputTokens + outputTokens;

            UsageRecord record = new UsageRecord(
                    Instant.now().toString(), sessionKey, model,
                    inputTokens, outputTokens, cacheReadTokens, cacheWriteTokens,
                    totalTokens, cost);

            String line = MAPPER.writeValueAsString(record) + "\n";
            Files.writeString(usagePath, line, StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
            log.debug("Recorded usage: model={} in={} out={} cost=${}", model,
                    inputTokens, outputTokens, String.format("%.6f", cost));
        } catch (IOException e) {
            log.error("Failed to record usage: {}", e.getMessage());
        }
    }

    // =========================================================================
    // Read / Summarize
    // =========================================================================

    /**
     * Resolve the usage file path for a session.
     */
    public static Path resolveUsagePath(Path transcriptPath) {
        String fileName = transcriptPath.getFileName().toString();
        String usageFileName = fileName.replace(".jsonl", "-usage.jsonl");
        return transcriptPath.resolveSibling(usageFileName);
    }

    /**
     * Read and summarize all usage records for a session.
     */
    @SuppressWarnings("unchecked")
    public static UsageSummary summarizeUsage(Path usagePath) {
        if (!Files.exists(usagePath)) {
            return new UsageSummary(0, 0, 0, 0, 0, 0.0, 0, null);
        }

        int totalIn = 0, totalOut = 0, totalCacheRead = 0, totalCacheWrite = 0;
        double totalCost = 0.0;
        int count = 0;
        String lastModel = null;

        try (BufferedReader reader = Files.newBufferedReader(usagePath, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty())
                    continue;
                try {
                    Map<String, Object> entry = MAPPER.readValue(line, Map.class);
                    totalIn += toInt(entry.get("inputTokens"));
                    totalOut += toInt(entry.get("outputTokens"));
                    totalCacheRead += toInt(entry.get("cacheReadTokens"));
                    totalCacheWrite += toInt(entry.get("cacheWriteTokens"));
                    totalCost += toDouble(entry.get("estimatedCost"));
                    count++;
                    Object m = entry.get("model");
                    if (m instanceof String s && !s.isEmpty())
                        lastModel = s;
                } catch (Exception ignored) {
                }
            }
        } catch (IOException e) {
            log.error("Failed to read usage: {}", e.getMessage());
        }

        return new UsageSummary(totalIn, totalOut, totalCacheRead, totalCacheWrite,
                totalIn + totalOut, totalCost, count, lastModel);
    }

    private static int toInt(Object v) {
        if (v instanceof Number n)
            return n.intValue();
        return 0;
    }

    private static double toDouble(Object v) {
        if (v instanceof Number n)
            return n.doubleValue();
        return 0.0;
    }
}

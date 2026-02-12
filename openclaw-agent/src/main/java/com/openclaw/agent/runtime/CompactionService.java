package com.openclaw.agent.runtime;

import com.openclaw.agent.models.ModelProvider;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Session history compaction — prunes and summarises conversation history
 * when it approaches context window limits.
 * Corresponds to TypeScript's compaction.ts.
 */
@Slf4j
public class CompactionService {

    /** Rough average: 1 token ≈ 4 characters (English). */
    private static final double CHARS_PER_TOKEN = 4.0;
    private static final double MAX_HISTORY_SHARE = 0.6;
    private static final String DEFAULT_SUMMARY_FALLBACK = "No prior history.";

    // =========================================================================
    // Token estimation
    // =========================================================================

    /**
     * Rough token estimation based on character count.
     */
    public static int estimateTokens(String text) {
        if (text == null || text.isEmpty())
            return 0;
        return Math.max(1, (int) Math.ceil(text.length() / CHARS_PER_TOKEN));
    }

    /**
     * Estimate total tokens across a list of messages.
     */
    public static int estimateMessagesTokens(List<ModelProvider.ChatMessage> messages) {
        int total = 0;
        for (var msg : messages) {
            if (msg.getContent() != null) {
                total += estimateTokens(msg.getContent());
            }
        }
        return total;
    }

    // =========================================================================
    // History pruning
    // =========================================================================

    /**
     * Result of pruning history.
     */
    public record PruneResult(
            List<ModelProvider.ChatMessage> messages,
            List<ModelProvider.ChatMessage> droppedMessages,
            int droppedTokens,
            int keptTokens,
            int budgetTokens) {
    }

    /**
     * Prune history to fit within a context-window share.
     * Keeps the most recent messages within the token budget.
     *
     * @param messages         full message history
     * @param maxContextTokens total context window tokens
     * @param maxHistoryShare  fraction of context reserved for history (default
     *                         0.6)
     */
    public static PruneResult pruneHistoryForContextShare(
            List<ModelProvider.ChatMessage> messages,
            int maxContextTokens,
            double maxHistoryShare) {

        double share = maxHistoryShare > 0 && maxHistoryShare <= 1.0
                ? maxHistoryShare
                : MAX_HISTORY_SHARE;
        int budgetTokens = (int) (maxContextTokens * share);

        if (messages == null || messages.isEmpty()) {
            return new PruneResult(List.of(), List.of(), 0, 0, budgetTokens);
        }

        // Walk backwards, keeping messages that fit budget
        int keptTokens = 0;
        int splitIndex = 0;

        for (int i = messages.size() - 1; i >= 0; i--) {
            int msgTokens = estimateTokens(messages.get(i).getContent());
            if (keptTokens + msgTokens > budgetTokens) {
                splitIndex = i + 1;
                break;
            }
            keptTokens += msgTokens;
        }

        List<ModelProvider.ChatMessage> kept = messages.subList(splitIndex, messages.size());
        List<ModelProvider.ChatMessage> dropped = messages.subList(0, splitIndex);
        int droppedTokens = estimateMessagesTokens(dropped);

        return new PruneResult(
                new ArrayList<>(kept),
                new ArrayList<>(dropped),
                droppedTokens,
                keptTokens,
                budgetTokens);
    }

    public static PruneResult pruneHistoryForContextShare(
            List<ModelProvider.ChatMessage> messages, int maxContextTokens) {
        return pruneHistoryForContextShare(messages, maxContextTokens, MAX_HISTORY_SHARE);
    }

    // =========================================================================
    // Message chunking
    // =========================================================================

    /**
     * Split messages into chunks where each chunk is ≤ maxTokens.
     */
    public static List<List<ModelProvider.ChatMessage>> chunkMessagesByMaxTokens(
            List<ModelProvider.ChatMessage> messages, int maxTokens) {

        List<List<ModelProvider.ChatMessage>> chunks = new ArrayList<>();
        List<ModelProvider.ChatMessage> current = new ArrayList<>();
        int currentTokens = 0;

        for (var msg : messages) {
            int msgTokens = estimateTokens(msg.getContent());
            if (!current.isEmpty() && currentTokens + msgTokens > maxTokens) {
                chunks.add(current);
                current = new ArrayList<>();
                currentTokens = 0;
            }
            current.add(msg);
            currentTokens += msgTokens;
        }
        if (!current.isEmpty()) {
            chunks.add(current);
        }

        return chunks;
    }

    /**
     * Split messages into roughly equal parts by token share.
     */
    public static List<List<ModelProvider.ChatMessage>> splitMessagesByTokenShare(
            List<ModelProvider.ChatMessage> messages, int parts) {
        int actualParts = Math.max(1, Math.min(parts, messages.size()));
        int totalTokens = estimateMessagesTokens(messages);
        int targetPerPart = Math.max(1, totalTokens / actualParts);

        List<List<ModelProvider.ChatMessage>> result = new ArrayList<>();
        List<ModelProvider.ChatMessage> current = new ArrayList<>();
        int currentTokens = 0;

        for (var msg : messages) {
            current.add(msg);
            currentTokens += estimateTokens(msg.getContent());

            if (currentTokens >= targetPerPart && result.size() < actualParts - 1) {
                result.add(current);
                current = new ArrayList<>();
                currentTokens = 0;
            }
        }
        if (!current.isEmpty()) {
            result.add(current);
        }

        return result;
    }

    // =========================================================================
    // LLM-based summarisation
    // =========================================================================

    /**
     * Summarise a list of message chunks using an LLM provider.
     *
     * @param provider        model provider to use for summarisation
     * @param modelId         model to use
     * @param chunks          message chunks to summarise
     * @param instructions    custom instructions for summarisation (nullable)
     * @param previousSummary previous summary to merge with (nullable)
     * @return CompletableFuture with the summary text
     */
    public static CompletableFuture<String> summarizeChunks(
            ModelProvider provider,
            String modelId,
            List<List<ModelProvider.ChatMessage>> chunks,
            String instructions,
            String previousSummary) {

        if (chunks == null || chunks.isEmpty()) {
            return CompletableFuture.completedFuture(DEFAULT_SUMMARY_FALLBACK);
        }

        // Build prompts for each chunk
        List<CompletableFuture<String>> chunkSummaries = new ArrayList<>();
        for (int i = 0; i < chunks.size(); i++) {
            var chunk = chunks.get(i);
            String prompt = buildSummarizationPrompt(chunk, instructions, previousSummary, i, chunks.size());
            chunkSummaries.add(callForSummary(provider, modelId, prompt));
        }

        // Collect all chunk summaries then merge if >1
        return CompletableFuture.allOf(chunkSummaries.toArray(new CompletableFuture[0]))
                .thenCompose(v -> {
                    List<String> summaries = chunkSummaries.stream()
                            .map(CompletableFuture::join)
                            .toList();

                    if (summaries.size() == 1) {
                        return CompletableFuture.completedFuture(summaries.get(0));
                    }

                    // Merge multiple summaries
                    String mergePrompt = buildMergePrompt(summaries);
                    return callForSummary(provider, modelId, mergePrompt);
                });
    }

    /**
     * High-level compaction: prune + summarise.
     */
    public static CompletableFuture<CompactionResult> compact(
            ModelProvider provider,
            String modelId,
            List<ModelProvider.ChatMessage> messages,
            int contextWindowTokens,
            int maxChunkTokens,
            String instructions) {

        PruneResult pruned = pruneHistoryForContextShare(messages, contextWindowTokens);

        if (pruned.droppedMessages().isEmpty()) {
            return CompletableFuture.completedFuture(new CompactionResult(
                    messages, null, 0, estimateMessagesTokens(messages)));
        }

        var chunks = chunkMessagesByMaxTokens(pruned.droppedMessages(), maxChunkTokens);

        return summarizeChunks(provider, modelId, chunks, instructions, null)
                .thenApply(summary -> {
                    // Prepend summary as a system message
                    ModelProvider.ChatMessage summaryMsg = ModelProvider.ChatMessage.builder()
                            .role("system")
                            .content("[Previous conversation summary]\n" + summary)
                            .build();

                    List<ModelProvider.ChatMessage> compacted = new ArrayList<>();
                    compacted.add(summaryMsg);
                    compacted.addAll(pruned.messages());

                    return new CompactionResult(
                            compacted,
                            summary,
                            pruned.droppedTokens(),
                            estimateMessagesTokens(compacted));
                });
    }

    public record CompactionResult(
            List<ModelProvider.ChatMessage> messages,
            String summary,
            int compactedTokens,
            int remainingTokens) {
    }

    // =========================================================================
    // Private helpers
    // =========================================================================

    private static String buildSummarizationPrompt(
            List<ModelProvider.ChatMessage> chunk,
            String instructions,
            String previousSummary,
            int partIndex,
            int totalParts) {

        StringBuilder sb = new StringBuilder();
        sb.append("Summarize the following conversation history concisely. ");
        sb.append("Preserve all important decisions, TODOs, open questions, ");
        sb.append("and any technical constraints mentioned.\n\n");

        if (instructions != null && !instructions.isBlank()) {
            sb.append("Additional instructions: ").append(instructions).append("\n\n");
        }
        if (previousSummary != null && !previousSummary.isBlank()) {
            sb.append("Previous context summary:\n").append(previousSummary).append("\n\n");
        }
        if (totalParts > 1) {
            sb.append("(Part ").append(partIndex + 1).append(" of ").append(totalParts).append(")\n\n");
        }

        sb.append("Conversation:\n");
        for (var msg : chunk) {
            sb.append(msg.getRole()).append(": ");
            String content = msg.getContent();
            if (content != null && content.length() > 2000) {
                sb.append(content, 0, 2000).append("... [truncated]");
            } else {
                sb.append(content != null ? content : "");
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    private static String buildMergePrompt(List<String> summaries) {
        StringBuilder sb = new StringBuilder();
        sb.append("Merge these partial summaries into a single cohesive summary. ");
        sb.append("Preserve decisions, TODOs, open questions, and any constraints.\n\n");
        for (int i = 0; i < summaries.size(); i++) {
            sb.append("--- Summary ").append(i + 1).append(" ---\n");
            sb.append(summaries.get(i)).append("\n\n");
        }
        return sb.toString();
    }

    private static CompletableFuture<String> callForSummary(
            ModelProvider provider, String modelId, String prompt) {

        ModelProvider.ChatRequest request = ModelProvider.ChatRequest.builder()
                .model(modelId)
                .messages(List.of(
                        ModelProvider.ChatMessage.builder()
                                .role("user")
                                .content(prompt)
                                .build()))
                .maxTokens(1024)
                .build();

        return provider.chat(request)
                .thenApply(response -> {
                    String content = response.getMessage() != null
                            ? response.getMessage().getContent()
                            : null;
                    return (content != null && !content.isBlank())
                            ? content.trim()
                            : DEFAULT_SUMMARY_FALLBACK;
                })
                .exceptionally(ex -> {
                    log.warn("Summarization failed: {}", ex.getMessage());
                    return DEFAULT_SUMMARY_FALLBACK;
                });
    }
}

package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

/**
 * Build reply payloads from the LLM assistant response.
 * Corresponds to TypeScript pi-embedded-runner/run/payloads.ts.
 *
 * <p>
 * This module processes assistant texts, tool metadata, error text, and
 * reasoning
 * to produce a list of reply items that should be delivered to the channel.
 * </p>
 */
@Slf4j
public final class RunPayloads {

    /** Token used to suppress reply delivery. */
    public static final String SILENT_REPLY_TOKEN = "[SILENT]";

    private RunPayloads() {
    }

    /**
     * A single reply payload to be delivered.
     */
    public record ReplyPayload(
            String text,
            List<String> mediaUrls,
            String mediaUrl,
            boolean isError,
            boolean audioAsVoice,
            String replyToId,
            boolean replyToTag,
            boolean replyToCurrent) {
    }

    /**
     * Tool metadata entry.
     */
    public record ToolMetaEntry(String toolName, String meta) {
    }

    /**
     * Parameters for building payloads.
     */
    public record PayloadParams(
            List<String> assistantTexts,
            List<ToolMetaEntry> toolMetas,
            String errorText,
            String rawErrorMessage,
            String reasoningText,
            String fallbackAnswerText,
            ToolMetaEntry lastToolError,
            String verboseLevel,
            String reasoningLevel,
            String toolResultFormat,
            boolean inlineToolResultsAllowed,
            boolean lastAssistantHasToolCalls,
            boolean lastAssistantWasToolUse) {
    }

    /**
     * Build reply payloads from assistant response data.
     *
     * @param params All the data needed to construct reply items
     * @return List of reply payloads (filtered for empty/silent entries)
     */
    public static List<ReplyPayload> buildPayloads(PayloadParams params) {
        List<MutableReplyItem> replyItems = new ArrayList<>();

        // 1. Error text
        if (params.errorText != null && !params.errorText.isBlank()) {
            replyItems.add(new MutableReplyItem(params.errorText, true));
        }

        // 2. Inline tool results
        boolean inlineToolResults = params.inlineToolResultsAllowed
                && !"off".equals(params.verboseLevel)
                && params.toolMetas != null
                && !params.toolMetas.isEmpty();
        if (inlineToolResults) {
            for (ToolMetaEntry entry : params.toolMetas) {
                String formatted = formatToolAggregate(entry.toolName, entry.meta);
                if (formatted != null && !formatted.isBlank()) {
                    replyItems.add(new MutableReplyItem(formatted, false));
                }
            }
        }

        // 3. Reasoning text
        if (params.reasoningText != null && !params.reasoningText.isBlank()
                && "on".equals(params.reasoningLevel)) {
            replyItems.add(new MutableReplyItem(params.reasoningText, false));
        }

        // 4. Assistant answer texts
        List<String> answerTexts;
        if (params.assistantTexts != null && !params.assistantTexts.isEmpty()) {
            answerTexts = params.assistantTexts;
        } else if (params.fallbackAnswerText != null && !params.fallbackAnswerText.isBlank()) {
            answerTexts = List.of(params.fallbackAnswerText);
        } else {
            answerTexts = List.of();
        }

        // Filter out duplicate error texts
        String normalizedErrorText = normalizeForComparison(params.errorText);
        String normalizedRawError = normalizeForComparison(params.rawErrorMessage);

        for (String text : answerTexts) {
            if (shouldSuppressErrorText(text, normalizedErrorText, normalizedRawError,
                    params.errorText != null)) {
                continue;
            }
            if (text != null && !text.isBlank()) {
                replyItems.add(new MutableReplyItem(text.trim(), false));
            }
        }

        // 5. Last tool error (only when no user-facing reply exists)
        if (params.lastToolError != null) {
            boolean hasUserFacingReply = !replyItems.isEmpty()
                    && !params.lastAssistantHasToolCalls
                    && !params.lastAssistantWasToolUse;
            String errorLower = params.lastToolError.meta != null
                    ? params.lastToolError.meta.toLowerCase()
                    : "";
            boolean isRecoverable = errorLower.contains("required")
                    || errorLower.contains("missing")
                    || errorLower.contains("invalid")
                    || errorLower.contains("must be")
                    || errorLower.contains("must have")
                    || errorLower.contains("needs")
                    || errorLower.contains("requires");

            if (!hasUserFacingReply && !isRecoverable) {
                String toolSummary = formatToolAggregate(
                        params.lastToolError.toolName, params.lastToolError.meta);
                String errorSuffix = (params.lastToolError.meta != null && !params.lastToolError.meta.isBlank())
                        ? ": " + params.lastToolError.meta
                        : "";
                replyItems.add(new MutableReplyItem(
                        "⚠️ " + toolSummary + " failed" + errorSuffix, true));
            }
        }

        // 6. Convert to final payloads, filter empty/silent
        return replyItems.stream()
                .filter(item -> item.text != null && !item.text.isBlank())
                .filter(item -> !isSilentReply(item.text))
                .map(item -> new ReplyPayload(
                        item.text.trim(),
                        item.mediaUrls,
                        item.mediaUrls != null && !item.mediaUrls.isEmpty()
                                ? item.mediaUrls.get(0)
                                : null,
                        item.isError,
                        item.audioAsVoice,
                        item.replyToId,
                        item.replyToTag,
                        item.replyToCurrent))
                .toList();
    }

    // --- Internal helpers ---

    private static boolean isSilentReply(String text) {
        return text != null && text.trim().startsWith(SILENT_REPLY_TOKEN);
    }

    private static String normalizeForComparison(String text) {
        if (text == null || text.isBlank())
            return null;
        return text.trim().toLowerCase().replaceAll("\\s+", " ");
    }

    private static boolean shouldSuppressErrorText(
            String text, String normalizedError, String normalizedRawError,
            boolean hasErrorText) {
        if (!hasErrorText)
            return false;
        String trimmed = text != null ? text.trim() : "";
        if (trimmed.isEmpty())
            return false;

        String normalized = normalizeForComparison(trimmed);
        if (normalized == null)
            return false;

        if (normalizedError != null && normalized.equals(normalizedError))
            return true;
        if (normalizedRawError != null && normalized.equals(normalizedRawError))
            return true;
        if ("the ai service returned an error. please try again.".equals(normalized))
            return true;

        return false;
    }

    private static String formatToolAggregate(String toolName, String meta) {
        if (toolName == null)
            return "";
        StringBuilder sb = new StringBuilder("🔧 ").append(toolName);
        if (meta != null && !meta.isBlank()) {
            sb.append(": ").append(meta);
        }
        return sb.toString();
    }

    private static class MutableReplyItem {
        String text;
        boolean isError;
        List<String> mediaUrls;
        boolean audioAsVoice;
        String replyToId;
        boolean replyToTag;
        boolean replyToCurrent;

        MutableReplyItem(String text, boolean isError) {
            this.text = text;
            this.isError = isError;
        }
    }
}

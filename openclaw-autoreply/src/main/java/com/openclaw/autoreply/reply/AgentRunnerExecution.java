package com.openclaw.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

/**
 * Agent run-loop execution — run agent turn with model fallback,
 * handle streaming text normalization, block reply pipeline integration,
 * error recovery (context overflow, compaction failure, role ordering,
 * session corruption), and tool result delivery.
 * Mirrors {@code auto-reply/reply/agent-runner-execution.ts}.
 */
public final class AgentRunnerExecution {

    private static final Logger log = LoggerFactory.getLogger(AgentRunnerExecution.class);

    private AgentRunnerExecution() {
    }

    // --- Result types ---

    /** Outcome of a single agent turn with fallback. */
    public sealed interface AgentRunLoopResult {
        record Success(
                Map<String, Object> runResult,
                String fallbackProvider,
                String fallbackModel,
                boolean didLogHeartbeatStrip,
                boolean autoCompactionCompleted,
                Set<String> directlySentBlockKeys) implements AgentRunLoopResult {
        }

        record Final(Map<String, Object> payload) implements AgentRunLoopResult {
        }
    }

    // --- Error detection patterns ---

    private static final Pattern FUNCTION_CALL_CORRUPTION = Pattern
            .compile("function call turn comes immediately after", Pattern.CASE_INSENSITIVE);
    private static final Pattern ROLE_ORDERING_ERROR = Pattern
            .compile("incorrect role information|roles must alternate", Pattern.CASE_INSENSITIVE);

    // --- Streaming text normalization ---

    /** Result of normalizing streaming text. */
    public record NormalizedStreamingText(String text, boolean skip) {
    }

    /**
     * Normalize streaming text payload: strip heartbeat tokens, detect
     * silent replies, sanitize user-facing text.
     */
    static NormalizedStreamingText normalizeStreamingText(
            Map<String, Object> payload,
            boolean isHeartbeat,
            boolean[] didLogHeartbeatStripRef) {
        String text = payload != null ? (String) payload.get("text") : null;

        if (!isHeartbeat && text != null && text.contains("HEARTBEAT_OK")) {
            // Strip stray HEARTBEAT_OK token
            String stripped = text.replace("HEARTBEAT_OK", "").trim();
            if (!didLogHeartbeatStripRef[0]) {
                didLogHeartbeatStripRef[0] = true;
                log.debug("Stripped stray HEARTBEAT_OK token from reply");
            }
            @SuppressWarnings("unchecked")
            List<String> mediaUrls = (List<String>) payload.get("mediaUrls");
            if (stripped.isEmpty() && (mediaUrls == null || mediaUrls.isEmpty())) {
                return new NormalizedStreamingText(null, true);
            }
            text = stripped;
        }

        if (text != null && com.openclaw.autoreply.ReplyTokens.isSilentReplyText(text)) {
            return new NormalizedStreamingText(null, true);
        }
        if (text == null || text.isEmpty()) {
            return new NormalizedStreamingText(null, true);
        }

        String sanitized = sanitizeUserFacingText(text);
        if (sanitized.trim().isEmpty()) {
            return new NormalizedStreamingText(null, true);
        }
        return new NormalizedStreamingText(sanitized, false);
    }

    private static String sanitizeUserFacingText(String text) {
        if (text == null)
            return "";
        return text;
    }

    // --- Run agent turn with fallback ---

    /**
     * Execute one agent turn with model fallback, error recovery,
     * streaming, and block reply pipeline.
     *
     * @param params execution parameters
     * @return agent run loop result
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<AgentRunLoopResult> runAgentTurnWithFallback(
            Map<String, Object> params) {

        boolean[] didLogHeartbeatStrip = { false };
        boolean[] autoCompactionCompleted = { false };
        Set<String> directlySentBlockKeys = new HashSet<>();

        String runId = params.get("runId") != null
                ? params.get("runId").toString()
                : UUID.randomUUID().toString();

        // Notify run start
        Consumer<String> onAgentRunStart = (Consumer<String>) params.get("onAgentRunStart");
        if (onAgentRunStart != null)
            onAgentRunStart.accept(runId);

        // Register run context
        String sessionKey = (String) params.get("sessionKey");
        if (sessionKey != null) {
            log.debug("Registered agent run context: runId={}, sessionKey={}", runId, sessionKey);
        }

        Map<String, Object> followupRun = (Map<String, Object>) params.get("followupRun");
        Map<String, Object> run = followupRun != null
                ? (Map<String, Object>) followupRun.get("run")
                : Map.of();
        String provider = (String) run.getOrDefault("provider", "");
        String model = (String) run.getOrDefault("model", "");
        boolean isHeartbeat = Boolean.TRUE.equals(params.get("isHeartbeat"));
        boolean didResetAfterCompactionFailure = false;

        try {
            // Full embedded PI agent execution deferred — placeholder loop
            Map<String, Object> runResult = executeAgentRun(params, run, provider, model,
                    isHeartbeat, didLogHeartbeatStrip, autoCompactionCompleted, directlySentBlockKeys);

            // Check for embedded error (context overflow, role ordering)
            Map<String, Object> meta = runResult.get("meta") instanceof Map<?, ?>
                    ? (Map<String, Object>) runResult.get("meta")
                    : Map.of();
            Map<String, Object> embeddedError = meta.get("error") instanceof Map<?, ?>
                    ? (Map<String, Object>) meta.get("error")
                    : null;

            if (embeddedError != null) {
                String errorMessage = String.valueOf(embeddedError.getOrDefault("message", ""));
                if (isContextOverflowError(errorMessage) && !didResetAfterCompactionFailure) {
                    return CompletableFuture.completedFuture(
                            new AgentRunLoopResult.Final(Map.of(
                                    "text",
                                    "⚠️ Context limit exceeded. I've reset our conversation to start fresh - please try again.\n\nTo prevent this, increase your compaction buffer by setting `agents.defaults.compaction.reserveTokensFloor` to 4000 or higher in your config.")));
                }
                String errorKind = String.valueOf(embeddedError.getOrDefault("kind", ""));
                if ("role_ordering".equals(errorKind)) {
                    return CompletableFuture.completedFuture(
                            new AgentRunLoopResult.Final(Map.of(
                                    "text",
                                    "⚠️ Message ordering conflict. I've reset the conversation - please try again.")));
                }
            }

            return CompletableFuture.completedFuture(
                    new AgentRunLoopResult.Success(
                            runResult, provider, model,
                            didLogHeartbeatStrip[0], autoCompactionCompleted[0],
                            directlySentBlockKeys.isEmpty() ? null : directlySentBlockKeys));

        } catch (Exception err) {
            String message = err.getMessage() != null ? err.getMessage() : err.toString();
            boolean isContextOverflow = isLikelyContextOverflowError(message);
            boolean isCompactionFailure = isCompactionFailureError(message);
            boolean isSessionCorruption = FUNCTION_CALL_CORRUPTION.matcher(message).find();
            boolean isRoleOrdering = ROLE_ORDERING_ERROR.matcher(message).find();

            if (isCompactionFailure) {
                return CompletableFuture.completedFuture(
                        new AgentRunLoopResult.Final(Map.of(
                                "text",
                                "⚠️ Context limit exceeded during compaction. I've reset our conversation to start fresh - please try again.\n\nTo prevent this, increase your compaction buffer by setting `agents.defaults.compaction.reserveTokensFloor` to 4000 or higher in your config.")));
            }
            if (isRoleOrdering) {
                return CompletableFuture.completedFuture(
                        new AgentRunLoopResult.Final(Map.of(
                                "text",
                                "⚠️ Message ordering conflict. I've reset the conversation - please try again.")));
            }
            if (isSessionCorruption && sessionKey != null) {
                log.error("Session history corrupted (Gemini function call ordering). Resetting session: {}",
                        sessionKey);
                return CompletableFuture.completedFuture(
                        new AgentRunLoopResult.Final(Map.of(
                                "text",
                                "⚠️ Session history was corrupted. I've reset the conversation - please try again!")));
            }

            log.error("Embedded agent failed before reply: {}", message);
            String trimmedMessage = message.replaceAll("\\.\\s*$", "");
            String fallbackText = isContextOverflow
                    ? "⚠️ Context overflow — prompt too large for this model. Try a shorter message or a larger-context model."
                    : "⚠️ Agent failed before reply: " + trimmedMessage + ".\nLogs: openclaw logs --follow";

            return CompletableFuture.completedFuture(
                    new AgentRunLoopResult.Final(Map.of("text", fallbackText)));
        }
    }

    // --- Agent execution (placeholder) ---

    @SuppressWarnings("unchecked")
    private static Map<String, Object> executeAgentRun(
            Map<String, Object> params,
            Map<String, Object> run,
            String provider, String model,
            boolean isHeartbeat,
            boolean[] didLogHeartbeatStripRef,
            boolean[] autoCompactionCompletedRef,
            Set<String> directlySentBlockKeys) {

        // Full PI agent / CLI agent execution integration deferred.
        // Returns empty result for now.
        log.debug("Agent run execution placeholder: provider={}, model={}", provider, model);
        return Map.of(
                "payloads", List.of(),
                "meta", Map.of(
                        "agentMeta", Map.of(
                                "provider", provider,
                                "model", model)));
    }

    // --- Error classification ---

    static boolean isContextOverflowError(String message) {
        if (message == null)
            return false;
        String lower = message.toLowerCase();
        return lower.contains("context") && lower.contains("overflow")
                || lower.contains("maximum context length")
                || lower.contains("token limit");
    }

    static boolean isLikelyContextOverflowError(String message) {
        if (message == null)
            return false;
        return isContextOverflowError(message)
                || message.toLowerCase().contains("too many tokens");
    }

    static boolean isCompactionFailureError(String message) {
        if (message == null)
            return false;
        String lower = message.toLowerCase();
        return lower.contains("compaction") && lower.contains("fail");
    }
}

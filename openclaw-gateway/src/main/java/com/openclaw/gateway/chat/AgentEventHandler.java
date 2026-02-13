package com.openclaw.gateway.chat;

import com.openclaw.gateway.websocket.EventBroadcaster;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Processes agent event payloads and emits chat deltas / finals to connected
 * WebSocket clients and node subscriptions.
 * <p>
 * Corresponds to TypeScript's {@code createAgentEventHandler()} in
 * server-chat.ts.
 */
@Slf4j
public class AgentEventHandler implements Consumer<AgentEventPayload> {

    /** Throttle interval: skip chat deltas sent less than 150 ms apart. */
    private static final long DELTA_THROTTLE_MS = 150;

    private final EventBroadcaster broadcaster;
    private final NodeSessionSender nodeSender;
    private final ChatRunState chatRunState;
    private final Map<String, Integer> agentRunSeq;
    private final Function<String, String> resolveSessionKeyForRun;
    private final Consumer<String> clearAgentRunContext;
    private final ToolEventRecipientRegistry toolEventRecipients;

    // Verbose level resolver: runId → "off" | "minimal" | "full"
    private final Function<String, String> resolveToolVerboseLevel;

    // Heartbeat suppression: runId → true if should suppress
    private final Function<String, Boolean> shouldSuppressHeartbeat;

    public AgentEventHandler(Builder builder) {
        this.broadcaster = builder.broadcaster;
        this.nodeSender = builder.nodeSender;
        this.chatRunState = builder.chatRunState;
        this.agentRunSeq = builder.agentRunSeq;
        this.resolveSessionKeyForRun = builder.resolveSessionKeyForRun;
        this.clearAgentRunContext = builder.clearAgentRunContext;
        this.toolEventRecipients = builder.toolEventRecipients;
        this.resolveToolVerboseLevel = builder.resolveToolVerboseLevel;
        this.shouldSuppressHeartbeat = builder.shouldSuppressHeartbeat;
    }

    @Override
    public void accept(AgentEventPayload evt) {
        var chatLink = chatRunState.getRegistry().peek(evt.runId());
        String sessionKey = chatLink != null ? chatLink.sessionKey()
                : resolveSessionKeyForRun.apply(evt.runId());
        String clientRunId = chatLink != null ? chatLink.clientRunId() : evt.runId();

        boolean isAborted = chatRunState.getAbortedRuns().containsKey(clientRunId)
                || chatRunState.getAbortedRuns().containsKey(evt.runId());

        // Enrich payload with sessionKey for Control UI filtering
        var agentPayload = sessionKey != null
                ? evt.withSessionKey(sessionKey)
                : evt;

        int last = agentRunSeq.getOrDefault(evt.runId(), 0);
        boolean isToolEvent = "tool".equals(evt.stream());
        String toolVerbose = isToolEvent
                ? resolveToolVerboseLevel.apply(evt.runId())
                : "off";

        // Skip tool events when verbose is off
        if (isToolEvent && "off".equals(toolVerbose)) {
            agentRunSeq.put(evt.runId(), evt.seq());
            return;
        }

        // Build tool payload (strip result/partialResult unless verbose=full)
        var toolPayload = isToolEvent && !"full".equals(toolVerbose)
                ? stripToolResults(agentPayload, sessionKey)
                : agentPayload;

        // Detect sequence gap
        if (evt.seq() != last + 1) {
            var gapPayload = AgentEventPayload.of(
                    evt.runId(), "error", evt.seq(),
                    Map.of("reason", "seq gap",
                            "expected", last + 1,
                            "received", evt.seq()));
            if (sessionKey != null) {
                gapPayload = gapPayload.withSessionKey(sessionKey);
            }
            broadcaster.broadcastToAll("agent", gapPayload.toMap());
        }
        agentRunSeq.put(evt.runId(), evt.seq());

        // Broadcast
        if (isToolEvent) {
            var recipients = toolEventRecipients.get(evt.runId());
            if (recipients != null && !recipients.isEmpty()) {
                broadcaster.sendToConnections(recipients, "agent", toolPayload.toMap());
            }
        } else {
            broadcaster.broadcastToAll("agent", agentPayload.toMap());
        }

        // Determine lifecycle phase
        String lifecyclePhase = "lifecycle".equals(evt.stream())
                && evt.dataString("phase") != null
                        ? evt.dataString("phase")
                        : null;

        // Session-scoped logic
        if (sessionKey != null) {
            nodeSender.send(sessionKey, "agent",
                    isToolEvent ? toolPayload.toMap() : agentPayload.toMap());

            if (!isAborted && "assistant".equals(evt.stream())
                    && evt.dataString("text") != null) {
                emitChatDelta(sessionKey, clientRunId, evt.seq(), evt.dataString("text"));

            } else if (!isAborted && ("end".equals(lifecyclePhase) || "error".equals(lifecyclePhase))) {
                if (chatLink != null) {
                    var finished = chatRunState.getRegistry().shift(evt.runId());
                    if (finished == null) {
                        clearAgentRunContext.accept(evt.runId());
                        return;
                    }
                    emitChatFinal(finished.sessionKey(), finished.clientRunId(), evt.seq(),
                            "error".equals(lifecyclePhase) ? "error" : "done",
                            evt.dataString("error"));
                } else {
                    emitChatFinal(sessionKey, evt.runId(), evt.seq(),
                            "error".equals(lifecyclePhase) ? "error" : "done",
                            evt.dataString("error"));
                }

            } else if (isAborted && ("end".equals(lifecyclePhase) || "error".equals(lifecyclePhase))) {
                chatRunState.getAbortedRuns().remove(clientRunId);
                chatRunState.getAbortedRuns().remove(evt.runId());
                chatRunState.getBuffers().remove(clientRunId);
                chatRunState.getDeltaSentAt().remove(clientRunId);
                if (chatLink != null) {
                    chatRunState.getRegistry().remove(evt.runId(), clientRunId, sessionKey);
                }
            }
        }

        // Lifecycle cleanup
        if ("end".equals(lifecyclePhase) || "error".equals(lifecyclePhase)) {
            toolEventRecipients.markFinal(evt.runId());
            clearAgentRunContext.accept(evt.runId());
        }
    }

    // ─── Chat delta / final emission ────────────────────────────────────

    private void emitChatDelta(String sessionKey, String clientRunId, int seq, String text) {
        chatRunState.getBuffers().put(clientRunId, text);
        long now = System.currentTimeMillis();
        long lastSent = chatRunState.getDeltaSentAt().getOrDefault(clientRunId, 0L);
        if (now - lastSent < DELTA_THROTTLE_MS) {
            return;
        }
        chatRunState.getDeltaSentAt().put(clientRunId, now);

        Map<String, Object> payload = buildChatPayload(clientRunId, sessionKey, seq, "delta", text, null);

        if (!shouldSuppressHeartbeat.apply(clientRunId)) {
            broadcaster.broadcastToAll("chat", payload);
        }
        nodeSender.send(sessionKey, "chat", payload);
    }

    private void emitChatFinal(String sessionKey, String clientRunId, int seq,
            String jobState, String error) {
        String text = chatRunState.getBuffers().getOrDefault(clientRunId, "").trim();
        chatRunState.getBuffers().remove(clientRunId);
        chatRunState.getDeltaSentAt().remove(clientRunId);

        if ("done".equals(jobState)) {
            Map<String, Object> payload = buildChatPayload(clientRunId, sessionKey, seq,
                    "final", text.isEmpty() ? null : text, null);
            if (!shouldSuppressHeartbeat.apply(clientRunId)) {
                broadcaster.broadcastToAll("chat", payload);
            }
            nodeSender.send(sessionKey, "chat", payload);
        } else {
            Map<String, Object> payload = new LinkedHashMap<>();
            payload.put("runId", clientRunId);
            payload.put("sessionKey", sessionKey);
            payload.put("seq", seq);
            payload.put("state", "error");
            if (error != null) {
                payload.put("errorMessage", error);
            }
            broadcaster.broadcastToAll("chat", payload);
            nodeSender.send(sessionKey, "chat", payload);
        }
    }

    private Map<String, Object> buildChatPayload(String runId, String sessionKey, int seq,
            String state, String text, String error) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("runId", runId);
        payload.put("sessionKey", sessionKey);
        payload.put("seq", seq);
        payload.put("state", state);
        if (text != null) {
            Map<String, Object> message = new LinkedHashMap<>();
            message.put("role", "assistant");
            message.put("content", List.of(Map.of("type", "text", "text", text)));
            message.put("timestamp", System.currentTimeMillis());
            payload.put("message", message);
        }
        if (error != null) {
            payload.put("errorMessage", error);
        }
        return payload;
    }

    private AgentEventPayload stripToolResults(AgentEventPayload payload, String sessionKey) {
        Map<String, Object> strippedData = payload.data() != null
                ? new LinkedHashMap<>(payload.data())
                : new LinkedHashMap<>();
        strippedData.remove("result");
        strippedData.remove("partialResult");
        return AgentEventPayload.of(payload.runId(), payload.stream(), payload.seq(), strippedData)
                .withSessionKey(sessionKey);
    }

    // ─── Builder ────────────────────────────────────────────────────────

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private EventBroadcaster broadcaster;
        private NodeSessionSender nodeSender;
        private ChatRunState chatRunState;
        private Map<String, Integer> agentRunSeq = new ConcurrentHashMap<>();
        private Function<String, String> resolveSessionKeyForRun = id -> null;
        private Consumer<String> clearAgentRunContext = id -> {
        };
        private ToolEventRecipientRegistry toolEventRecipients = new ToolEventRecipientRegistry();
        private Function<String, String> resolveToolVerboseLevel = id -> "off";
        private Function<String, Boolean> shouldSuppressHeartbeat = id -> false;

        public Builder broadcaster(EventBroadcaster broadcaster) {
            this.broadcaster = broadcaster;
            return this;
        }

        public Builder nodeSender(NodeSessionSender nodeSender) {
            this.nodeSender = nodeSender;
            return this;
        }

        public Builder chatRunState(ChatRunState chatRunState) {
            this.chatRunState = chatRunState;
            return this;
        }

        public Builder agentRunSeq(Map<String, Integer> agentRunSeq) {
            this.agentRunSeq = agentRunSeq;
            return this;
        }

        public Builder resolveSessionKeyForRun(Function<String, String> fn) {
            this.resolveSessionKeyForRun = fn;
            return this;
        }

        public Builder clearAgentRunContext(Consumer<String> fn) {
            this.clearAgentRunContext = fn;
            return this;
        }

        public Builder toolEventRecipients(ToolEventRecipientRegistry registry) {
            this.toolEventRecipients = registry;
            return this;
        }

        public Builder resolveToolVerboseLevel(Function<String, String> fn) {
            this.resolveToolVerboseLevel = fn;
            return this;
        }

        public Builder shouldSuppressHeartbeat(Function<String, Boolean> fn) {
            this.shouldSuppressHeartbeat = fn;
            return this;
        }

        public AgentEventHandler build() {
            Objects.requireNonNull(broadcaster, "broadcaster is required");
            Objects.requireNonNull(nodeSender, "nodeSender is required");
            Objects.requireNonNull(chatRunState, "chatRunState is required");
            return new AgentEventHandler(this);
        }
    }

    /**
     * Functional interface for sending events to node subscriptions.
     */
    @FunctionalInterface
    public interface NodeSessionSender {
        void send(String sessionKey, String event, Object payload);
    }
}

package com.openclaw.plugin.hooks;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.PluginTypes.*;
import com.openclaw.plugin.registry.PluginRegistry;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.BiFunction;

/**
 * Plugin hook runner — executes plugin lifecycle hooks with error handling
 * and priority ordering.
 * Corresponds to TypeScript's plugins/hooks.ts createHookRunner().
 */
@Slf4j
public class PluginHookRunner {

    private final PluginRegistry registry;
    private final boolean catchErrors;

    /** No-op runner that never fires any hooks. */
    public static final PluginHookRunner NOOP = new PluginHookRunner(new PluginRegistry(), true);

    public PluginHookRunner(PluginRegistry registry) {
        this(registry, true);
    }

    public PluginHookRunner(PluginRegistry registry, boolean catchErrors) {
        this.registry = registry;
        this.catchErrors = catchErrors;
    }

    // =========================================================================
    // Core execution primitives
    // =========================================================================

    /**
     * Fire-and-forget hook — all handlers run in parallel.
     * Used for: agent_end, message_received, message_sent, after_tool_call,
     * session_start/end, gateway_start/stop
     */
    public void runVoidHook(String hookName, Object event, Object ctx) {
        List<PluginRegistry.PluginHookRegistration> hooks = getHooksForName(hookName);
        if (hooks.isEmpty())
            return;

        log.debug("[hooks] running {} ({} handlers)", hookName, hooks.size());

        List<CompletableFuture<Void>> futures = new ArrayList<>();
        for (var hook : hooks) {
            futures.add(CompletableFuture.runAsync(() -> {
                try {
                    invokeHandler(hook, event, ctx);
                } catch (Exception e) {
                    String msg = String.format("[hooks] %s handler from %s failed: %s",
                            hookName, hook.getPluginId(), e.getMessage());
                    if (catchErrors) {
                        log.error(msg, e);
                    } else {
                        throw new RuntimeException(msg, e);
                    }
                }
            }));
        }

        try {
            CompletableFuture.allOf(futures.toArray(CompletableFuture[]::new)).join();
        } catch (CompletionException e) {
            if (!catchErrors)
                throw e;
            log.error("[hooks] {} parallel execution had errors", hookName);
        }
    }

    /**
     * Modifying hook — handlers run sequentially, results are merged.
     * Used for: before_agent_start, message_sending, before_tool_call
     */
    @SuppressWarnings("unchecked")
    public <T> T runModifyingHook(String hookName, Object event, Object ctx,
            BiFunction<T, T, T> mergeResults) {
        List<PluginRegistry.PluginHookRegistration> hooks = getHooksForName(hookName);
        if (hooks.isEmpty())
            return null;

        log.debug("[hooks] running {} ({} handlers, sequential)", hookName, hooks.size());

        T result = null;
        for (var hook : hooks) {
            try {
                Object handlerResult = invokeHandler(hook, event, ctx);
                if (handlerResult != null) {
                    T typed = (T) handlerResult;
                    if (mergeResults != null && result != null) {
                        result = mergeResults.apply(result, typed);
                    } else {
                        result = typed;
                    }
                }
            } catch (Exception e) {
                String msg = String.format("[hooks] %s handler from %s failed: %s",
                        hookName, hook.getPluginId(), e.getMessage());
                if (catchErrors) {
                    log.error(msg, e);
                } else {
                    throw new RuntimeException(msg, e);
                }
            }
        }
        return result;
    }

    // =========================================================================
    // Agent hooks
    // =========================================================================

    /**
     * Run before_agent_start — allows plugins to inject context into prompt.
     * Runs sequentially, merging systemPrompt and prependContext.
     */
    public BeforeAgentStartResult runBeforeAgentStart(
            BeforeAgentStartEvent event, AgentContext ctx) {
        return runModifyingHook(
                PluginHookName.BEFORE_AGENT_START.key(), event, ctx,
                (acc, next) -> BeforeAgentStartResult.builder()
                        .systemPrompt(next.getSystemPrompt() != null
                                ? next.getSystemPrompt()
                                : acc.getSystemPrompt())
                        .prependContext(mergeContext(
                                acc.getPrependContext(), next.getPrependContext()))
                        .build());
    }

    /**
     * Run agent_end — allows plugins to analyze completed conversations.
     * Runs in parallel (fire-and-forget).
     */
    public void runAgentEnd(AgentEndEvent event, AgentContext ctx) {
        runVoidHook(PluginHookName.AGENT_END.key(), event, ctx);
    }

    public void runBeforeCompaction(BeforeCompactionEvent event, AgentContext ctx) {
        runVoidHook(PluginHookName.BEFORE_COMPACTION.key(), event, ctx);
    }

    public void runAfterCompaction(AfterCompactionEvent event, AgentContext ctx) {
        runVoidHook(PluginHookName.AFTER_COMPACTION.key(), event, ctx);
    }

    // =========================================================================
    // Message hooks
    // =========================================================================

    public void runMessageReceived(MessageReceivedEvent event, MessageContext ctx) {
        runVoidHook(PluginHookName.MESSAGE_RECEIVED.key(), event, ctx);
    }

    /**
     * Run message_sending — allows plugins to modify or cancel outgoing messages.
     * Runs sequentially.
     */
    public MessageSendingResult runMessageSending(
            MessageSendingEvent event, MessageContext ctx) {
        return runModifyingHook(
                PluginHookName.MESSAGE_SENDING.key(), event, ctx,
                (acc, next) -> MessageSendingResult.builder()
                        .content(next.getContent() != null
                                ? next.getContent()
                                : acc.getContent())
                        .cancel(next.getCancel() != null
                                ? next.getCancel()
                                : acc.getCancel())
                        .build());
    }

    public void runMessageSent(MessageSentEvent event, MessageContext ctx) {
        runVoidHook(PluginHookName.MESSAGE_SENT.key(), event, ctx);
    }

    // =========================================================================
    // Tool hooks
    // =========================================================================

    /**
     * Run before_tool_call — allows plugins to modify or block tool calls.
     * Runs sequentially.
     */
    public BeforeToolCallResult runBeforeToolCall(
            BeforeToolCallEvent event, AgentContext ctx) {
        return runModifyingHook(
                PluginHookName.BEFORE_TOOL_CALL.key(), event, ctx,
                (acc, next) -> BeforeToolCallResult.builder()
                        .params(next.getParams() != null
                                ? next.getParams()
                                : acc.getParams())
                        .block(next.getBlock() != null
                                ? next.getBlock()
                                : acc.getBlock())
                        .blockReason(next.getBlockReason() != null
                                ? next.getBlockReason()
                                : acc.getBlockReason())
                        .build());
    }

    public void runAfterToolCall(AfterToolCallEvent event, AgentContext ctx) {
        runVoidHook(PluginHookName.AFTER_TOOL_CALL.key(), event, ctx);
    }

    // =========================================================================
    // Session hooks
    // =========================================================================

    public void runSessionStart(SessionStartEvent event) {
        runVoidHook(PluginHookName.SESSION_START.key(), event, null);
    }

    public void runSessionEnd(SessionEndEvent event) {
        runVoidHook(PluginHookName.SESSION_END.key(), event, null);
    }

    // =========================================================================
    // Gateway hooks
    // =========================================================================

    public void runGatewayStart(GatewayStartEvent event) {
        runVoidHook(PluginHookName.GATEWAY_START.key(), event, null);
    }

    public void runGatewayStop(GatewayStopEvent event) {
        runVoidHook(PluginHookName.GATEWAY_STOP.key(), event, null);
    }

    // =========================================================================
    // Utility
    // =========================================================================

    public boolean hasHooks(PluginHookName hookName) {
        return !getHooksForName(hookName.key()).isEmpty();
    }

    public int getHookCount(PluginHookName hookName) {
        return getHooksForName(hookName.key()).size();
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    private List<PluginRegistry.PluginHookRegistration> getHooksForName(String hookName) {
        return registry.getHooks().stream()
                .filter(h -> h.getEvents() != null && h.getEvents().contains(hookName))
                .toList();
    }

    private Object invokeHandler(PluginRegistry.PluginHookRegistration hook,
            Object event, Object ctx) {
        // Hook handlers are registered as event-name → handler mappings.
        // For the Java SPI-based system, handlers would implement a functional
        // interface.
        // Currently this is a placeholder; actual handler invocation depends on
        // how plugins register handlers through OpenClawPluginApi.
        log.debug("[hooks] invoking hook from plugin {}", hook.getPluginId());
        return null;
    }

    private String mergeContext(String acc, String next) {
        if (acc != null && next != null) {
            return acc + "\n\n" + next;
        }
        return next != null ? next : acc;
    }
}

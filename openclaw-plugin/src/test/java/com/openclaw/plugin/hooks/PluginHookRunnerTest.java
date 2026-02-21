package com.openclaw.plugin.hooks;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.registry.PluginRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for PluginHookRunner.
 */
class PluginHookRunnerTest {

    PluginRegistry registry;
    PluginHookRunner runner;

    @BeforeEach
    void setUp() {
        registry = new PluginRegistry();
        runner = new PluginHookRunner(registry);
    }

    @Test
    void noopRunnerNeverFails() {
        PluginHookRunner noop = PluginHookRunner.NOOP;
        // Should not throw
        noop.runAgentEnd(
                PluginTypes.AgentEndEvent.builder().success(true).build(),
                PluginTypes.AgentContext.builder().agentId("test").build());
        assertNotNull(noop);
    }

    @Test
    void beforeAgentStartReturnsNullWhenNoHooks() {
        PluginTypes.BeforeAgentStartResult result = runner.runBeforeAgentStart(
                PluginTypes.BeforeAgentStartEvent.builder().prompt("test").build(),
                PluginTypes.AgentContext.builder().agentId("test").build());
        assertNull(result);
    }

    @Test
    void voidHookRunsWithoutException() {
        // Fire all void hooks with empty registry — should not throw
        assertDoesNotThrow(() -> {
            runner.runAgentEnd(
                    PluginTypes.AgentEndEvent.builder().success(true).build(),
                    PluginTypes.AgentContext.builder().build());
            runner.runBeforeCompaction(
                    PluginTypes.BeforeCompactionEvent.builder().messageCount(10).build(),
                    PluginTypes.AgentContext.builder().build());
            runner.runAfterCompaction(
                    PluginTypes.AfterCompactionEvent.builder().messageCount(5).compactedCount(5).build(),
                    PluginTypes.AgentContext.builder().build());
            runner.runMessageReceived(
                    PluginTypes.MessageReceivedEvent.builder().content("hi").build(),
                    PluginTypes.MessageContext.builder().channelId("ch1").build());
            runner.runMessageSent(
                    PluginTypes.MessageSentEvent.builder().content("hi").success(true).build(),
                    PluginTypes.MessageContext.builder().build());
            runner.runAfterToolCall(
                    PluginTypes.AfterToolCallEvent.builder().toolName("bash").build(),
                    PluginTypes.AgentContext.builder().build());
            runner.runSessionStart(PluginTypes.SessionStartEvent.builder().sessionId("s1").build());
            runner.runSessionEnd(PluginTypes.SessionEndEvent.builder().sessionId("s1").build());
            runner.runGatewayStart(PluginTypes.GatewayStartEvent.builder().port(8080).build());
            runner.runGatewayStop(PluginTypes.GatewayStopEvent.builder().reason("test").build());
        });
    }

    @Test
    void beforeToolCallReturnsNullWhenNoHooks() {
        PluginTypes.BeforeToolCallResult result = runner.runBeforeToolCall(
                PluginTypes.BeforeToolCallEvent.builder().toolName("bash").build(),
                PluginTypes.AgentContext.builder().build());
        assertNull(result);
    }

    @Test
    void messageSendingReturnsNullWhenNoHooks() {
        PluginTypes.MessageSendingResult result = runner.runMessageSending(
                PluginTypes.MessageSendingEvent.builder().content("test").build(),
                PluginTypes.MessageContext.builder().build());
        assertNull(result);
    }

    @Test
    void hasHooksReturnsFalseForEmptyRegistry() {
        assertFalse(runner.hasHooks(PluginTypes.PluginHookName.BEFORE_AGENT_START));
        assertFalse(runner.hasHooks(PluginTypes.PluginHookName.AGENT_END));
        assertEquals(0, runner.getHookCount(PluginTypes.PluginHookName.BEFORE_TOOL_CALL));
    }

    @Test
    void hasHooksReturnsTrueWhenRegistered() {
        registry.registerHook(PluginRegistry.PluginHookRegistration.builder()
                .pluginId("test-plugin")
                .events(List.of("before_agent_start"))
                .build());

        assertTrue(runner.hasHooks(PluginTypes.PluginHookName.BEFORE_AGENT_START));
        assertEquals(1, runner.getHookCount(PluginTypes.PluginHookName.BEFORE_AGENT_START));
        assertFalse(runner.hasHooks(PluginTypes.PluginHookName.AGENT_END));
    }
}

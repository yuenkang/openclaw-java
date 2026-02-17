package com.openclaw.agent.hooks;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

class InternalHookRegistryTest {

    private InternalHookRegistry registry;

    @BeforeEach
    void setUp() {
        registry = new InternalHookRegistry();
    }

    @Nested
    class Registration {
        @Test
        void registerAndTrigger_typeLevel() {
            var counter = new AtomicInteger();
            registry.register("agent", event -> counter.incrementAndGet());

            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.AGENT, "bootstrap", "s1");
            registry.trigger(event);

            assertEquals(1, counter.get());
        }

        @Test
        void registerAndTrigger_specificAction() {
            var counter = new AtomicInteger();
            registry.register("agent:bootstrap", event -> counter.incrementAndGet());

            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.AGENT, "bootstrap", "s1");
            registry.trigger(event);
            assertEquals(1, counter.get());

            // Different action should NOT trigger
            var event2 = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.AGENT, "shutdown", "s1");
            registry.trigger(event2);
            assertEquals(1, counter.get());
        }

        @Test
        void bothTypeLevelAndSpecificFire() {
            var typeCounter = new AtomicInteger();
            var specificCounter = new AtomicInteger();
            registry.register("command", event -> typeCounter.incrementAndGet());
            registry.register("command:new", event -> specificCounter.incrementAndGet());

            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.COMMAND, "new", "s1");
            registry.trigger(event);

            assertEquals(1, typeCounter.get());
            assertEquals(1, specificCounter.get());
        }
    }

    @Nested
    class Unregister {
        @Test
        void unregisterRemovesHandler() {
            var counter = new AtomicInteger();
            InternalHookRegistry.HookHandler handler = event -> counter.incrementAndGet();
            registry.register("agent", handler);

            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.AGENT, "bootstrap", "s1");
            registry.trigger(event);
            assertEquals(1, counter.get());

            registry.unregister("agent", handler);
            registry.trigger(event);
            assertEquals(1, counter.get()); // should not increment
        }

        @Test
        void clearAll_removesEverything() {
            registry.register("agent", event -> {
            });
            registry.register("command:new", event -> {
            });
            assertFalse(registry.getRegisteredEventKeys().isEmpty());

            registry.clearAll();
            assertTrue(registry.getRegisteredEventKeys().isEmpty());
        }
    }

    @Nested
    class ErrorIsolation {
        @Test
        void handlerError_doesNotBlockOthers() {
            var counter = new AtomicInteger();
            registry.register("agent", event -> {
                throw new RuntimeException("boom");
            });
            registry.register("agent", event -> counter.incrementAndGet());

            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.AGENT, "bootstrap", "s1");
            registry.trigger(event);

            assertEquals(1, counter.get()); // second handler still runs
        }
    }

    @Nested
    class EventFactory {
        @Test
        void createEvent_withContext() {
            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.SESSION, "start", "s1",
                    Map.of("key", "value"));

            assertEquals("session", event.getType().key());
            assertEquals("start", event.getAction());
            assertEquals("s1", event.getSessionKey());
            assertEquals("value", event.getContext().get("key"));
            assertEquals("session:start", event.eventKey());
            assertNotNull(event.getTimestamp());
            assertTrue(event.getMessages().isEmpty());
        }

        @Test
        void createAgentBootstrapEvent() {
            var event = InternalHookRegistry.createAgentBootstrapEvent(
                    "s1", "/workspace", "main");

            assertTrue(InternalHookRegistry.isAgentBootstrapEvent(event));
            assertEquals("/workspace", event.getContext().get("workspaceDir"));
            assertEquals("main", event.getContext().get("agentId"));
        }

        @Test
        void isAgentBootstrapEvent_falseForOther() {
            var event = InternalHookRegistry.createEvent(
                    InternalHookRegistry.HookEventType.COMMAND, "new", "s1");
            assertFalse(InternalHookRegistry.isAgentBootstrapEvent(event));
        }
    }

    @Test
    void getRegisteredEventKeys_returnsAllKeys() {
        registry.register("agent", event -> {
        });
        registry.register("command:new", event -> {
        });
        registry.register("session:end", event -> {
        });

        var keys = registry.getRegisteredEventKeys();
        assertEquals(3, keys.size());
        assertTrue(keys.contains("agent"));
        assertTrue(keys.contains("command:new"));
        assertTrue(keys.contains("session:end"));
    }
}

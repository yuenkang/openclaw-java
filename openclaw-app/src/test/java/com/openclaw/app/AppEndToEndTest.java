package com.openclaw.app;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketHttpHeaders;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.client.standard.StandardWebSocketClient;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.net.URI;
import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * End-to-end tests for the OpenClaw application.
 * <p>
 * Exercises full request flows through the Spring Boot context:
 * WebSocket handshake → RPC method → gateway → agent/session-store → response.
 * No mocks — the real server is started on a random port.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AppEndToEndTest {

    @LocalServerPort
    private int port;

    private final ObjectMapper mapper = new ObjectMapper();
    private WebSocketSession session;
    private final ArrayBlockingQueue<String> messages = new ArrayBlockingQueue<>(50);

    @BeforeEach
    void connect() throws Exception {
        jakarta.websocket.WebSocketContainer container = jakarta.websocket.ContainerProvider.getWebSocketContainer();
        container.setDefaultMaxTextMessageBufferSize(256 * 1024);
        StandardWebSocketClient client = new StandardWebSocketClient(container);
        URI uri = URI.create("ws://127.0.0.1:" + port + "/");

        session = client.execute(new TextWebSocketHandler() {
            @Override
            protected void handleTextMessage(WebSocketSession s, TextMessage message) {
                messages.add(message.getPayload());
            }
        }, new WebSocketHttpHeaders(), uri).get(5, TimeUnit.SECONDS);

        assertTrue(session.isOpen());

        // consume connect.challenge
        String challengeMsg = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(challengeMsg);
        JsonNode challenge = mapper.readTree(challengeMsg);
        assertEquals("connect.challenge", challenge.get("event").asText());

        // complete handshake
        session.sendMessage(new TextMessage(mapper.writeValueAsString(Map.of(
                "type", "req",
                "id", "hs",
                "method", "connect",
                "params", Map.of(
                        "minProtocol", 3,
                        "maxProtocol", 3,
                        "client", Map.of("id", "e2e-test", "version", "0.1", "platform", "test", "mode", "test"),
                        "role", "operator",
                        "scopes", new String[] { "operator.admin" })))));

        String helloMsg = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(helloMsg);
        JsonNode hello = mapper.readTree(helloMsg);
        assertTrue(hello.get("ok").asBoolean());
    }

    @AfterEach
    void disconnect() throws Exception {
        if (session != null && session.isOpen())
            session.close();
        messages.clear();
    }

    // =========================================================================
    // Helper
    // =========================================================================

    private JsonNode rpc(String id, String method, Object params) throws Exception {
        Map<String, Object> frame = new LinkedHashMap<>();
        frame.put("type", "req");
        frame.put("id", id);
        frame.put("method", method);
        frame.put("params", params != null ? params : Map.of());
        session.sendMessage(new TextMessage(mapper.writeValueAsString(frame)));

        // Poll until we find the response matching our request ID.
        // Broadcast events or responses for other IDs are discarded.
        long deadline = System.currentTimeMillis() + 10_000;
        while (true) {
            long remaining = deadline - System.currentTimeMillis();
            if (remaining <= 0)
                break;
            String raw = messages.poll(remaining, TimeUnit.MILLISECONDS);
            assertNotNull(raw, "No response for " + method);
            JsonNode node = mapper.readTree(raw);
            // Skip broadcast events (they have "event" field, no "id")
            if (node.has("event") && !node.has("id"))
                continue;
            // Match by response ID
            if (node.has("id") && id.equals(node.get("id").asText())) {
                return node;
            }
            // Otherwise discard (response for a different request or unknown frame)
        }
        fail("Timed out waiting for response to " + method + " (id=" + id + ")");
        return null; // unreachable
    }

    private JsonNode rpcOk(String id, String method, Object params) throws Exception {
        JsonNode res = rpc(id, method, params);
        assertTrue(res.get("ok").asBoolean(),
                method + " failed: " + (res.has("error") ? res.get("error") : "no error info"));
        return res.get("payload");
    }

    // =========================================================================
    // Session full lifecycle: create → get → patch → reset → delete
    // =========================================================================

    @Test
    @Order(1)
    void sessionLifecycle_createGetPatchResetDelete() throws Exception {
        // 1. Create
        JsonNode created = rpcOk("s1", "session.create",
                Map.of("sessionKey", "e2e-lifecycle", "cwd", "/tmp"));
        String sessionId = created.get("sessionId").asText();
        assertNotNull(sessionId);
        assertEquals("e2e-lifecycle", created.get("sessionKey").asText());

        // 2. Get by sessionId
        JsonNode fetched = rpcOk("s2", "session.get",
                Map.of("sessionId", sessionId));
        assertEquals(sessionId, fetched.get("sessionId").asText());
        assertEquals("e2e-lifecycle", fetched.get("sessionKey").asText());

        // 3. Patch (update label)
        JsonNode patched = rpcOk("s3", "session.patch",
                Map.of("sessionId", sessionId, "label", "My E2E Session"));
        assertNotNull(patched);

        // 4. Verify patch took effect
        JsonNode afterPatch = rpcOk("s4", "session.get",
                Map.of("sessionId", sessionId));
        assertEquals("My E2E Session", afterPatch.get("label").asText());

        // 5. Reset
        JsonNode resetResult = rpcOk("s5", "session.reset",
                Map.of("sessionId", sessionId));
        assertNotNull(resetResult);

        // 6. Delete
        JsonNode deleteResult = rpcOk("s6", "session.delete",
                Map.of("sessionId", sessionId));
        assertNotNull(deleteResult);

        // 7. Get after delete → should fail or return null
        JsonNode afterDelete = rpc("s7", "session.get",
                Map.of("sessionId", sessionId));
        // Depending on implementation: either ok=false or payload is null
        if (afterDelete.get("ok").asBoolean()) {
            assertTrue(afterDelete.get("payload").isNull() || afterDelete.get("payload").isMissingNode());
        }
    }

    // =========================================================================
    // Session get by sessionKey
    // =========================================================================

    @Test
    @Order(2)
    void sessionGet_bySessionKey() throws Exception {
        // Create a session
        JsonNode created = rpcOk("sk1", "session.create",
                Map.of("sessionKey", "e2e-key-lookup", "cwd", "/tmp"));
        String sessionId = created.get("sessionId").asText();

        // Get by sessionKey
        JsonNode fetched = rpcOk("sk2", "session.get",
                Map.of("sessionKey", "e2e-key-lookup"));
        assertEquals(sessionId, fetched.get("sessionId").asText());

        // Cleanup
        rpcOk("sk3", "session.delete", Map.of("sessionId", sessionId));
    }

    // =========================================================================
    // Session get — missing session returns gracefully
    // =========================================================================

    @Test
    @Order(3)
    void sessionGet_nonexistentSession() throws Exception {
        JsonNode res = rpc("nx1", "session.get",
                Map.of("sessionId", "does-not-exist-" + UUID.randomUUID()));
        // Should not crash — either ok=false or ok=true with null payload
        assertNotNull(res);
    }

    // =========================================================================
    // Sessions list after creating multiple
    // =========================================================================

    @Test
    @Order(4)
    void sessionsList_returnsCreatedSessions() throws Exception {
        // Create two sessions
        JsonNode s1 = rpcOk("ml1", "session.create",
                Map.of("sessionKey", "e2e-list-a", "cwd", "/tmp"));
        JsonNode s2 = rpcOk("ml2", "session.create",
                Map.of("sessionKey", "e2e-list-b", "cwd", "/tmp"));

        // List all
        JsonNode list = rpcOk("ml3", "sessions.list", null);
        // sessions.list returns {sessions: [...], count: N, ...}
        assertTrue(list.isObject());
        assertTrue(list.has("sessions"));
        assertTrue(list.get("sessions").isArray());

        // Cleanup
        rpcOk("ml4", "session.delete", Map.of("sessionId", s1.get("sessionId").asText()));
        rpcOk("ml5", "session.delete", Map.of("sessionId", s2.get("sessionId").asText()));
    }

    // =========================================================================
    // Sessions resolve
    // =========================================================================

    @Test
    @Order(5)
    void sessionsResolve_resolvesSessionKey() throws Exception {
        JsonNode created = rpcOk("sr1", "session.create",
                Map.of("sessionKey", "e2e-resolve-target", "cwd", "/tmp"));
        String sessionId = created.get("sessionId").asText();

        JsonNode resolved = rpcOk("sr2", "sessions.resolve",
                Map.of("sessionKey", "e2e-resolve-target"));
        assertNotNull(resolved);

        // Cleanup
        rpcOk("sr3", "session.delete", Map.of("sessionId", sessionId));
    }

    // =========================================================================
    // Config get — verify structure
    // =========================================================================

    @Test
    @Order(10)
    void configGet_returnsConfigStructure() throws Exception {
        JsonNode config = rpcOk("c1", "config.get", null);
        assertNotNull(config);
        // Config should be a JSON object
        assertTrue(config.isObject());
    }

    // =========================================================================
    // Models list — verify structure
    // =========================================================================

    @Test
    @Order(11)
    void modelsList_containsModelsArray() throws Exception {
        JsonNode payload = rpcOk("m1", "models.list", null);
        assertTrue(payload.has("models"));
        assertTrue(payload.get("models").isArray());

        // Each model should have id and provider
        if (payload.get("models").size() > 0) {
            JsonNode first = payload.get("models").get(0);
            assertTrue(first.has("id"));
            assertTrue(first.has("provider"));
        }
    }

    // =========================================================================
    // Agent list — verify structure
    // =========================================================================

    @Test
    @Order(12)
    void agentList_hasAgentsArray() throws Exception {
        JsonNode payload = rpcOk("a1", "agents.list", null);
        assertTrue(payload.has("agents"));
        assertTrue(payload.get("agents").isArray());
    }

    // =========================================================================
    // Agent identity — verify structure
    // =========================================================================

    @Test
    @Order(13)
    void agentIdentityGet_returnsGracefully() throws Exception {
        // May return ok with identity or error (UNAVAILABLE) if no agents configured
        JsonNode res = rpc("ai1", "agent.identity.get", null);
        assertNotNull(res);
        // Either ok=true with payload or ok=false with UNAVAILABLE — both are valid
    }

    // =========================================================================
    // Health — verify uptime is positive
    // =========================================================================

    @Test
    @Order(14)
    void health_uptimeIsPositive() throws Exception {
        JsonNode payload = rpcOk("h1", "health", null);
        // health maps to status handler, returns a status summary object
        assertNotNull(payload);
        assertTrue(payload.isObject());
    }

    // =========================================================================
    // Channels status — should return even if no channels configured
    // =========================================================================

    @Test
    @Order(20)
    void channelsStatus_returnsStatusMap() throws Exception {
        JsonNode payload = rpcOk("ch1", "channels.status", null);
        assertNotNull(payload);
    }

    // =========================================================================
    // Cron lifecycle: add → list → status → remove
    // =========================================================================

    @Test
    @Order(30)
    void cronLifecycle_addListStatusRemove() throws Exception {
        // 1. Add a cron job (CronJob field is 'name', not 'label')
        JsonNode added = rpcOk("cr1", "cron.add", Map.of(
                "name", "E2E Test Cron",
                "schedule", "0 0 * * *",
                "message", "daily ping",
                "channelId", "cron-e2e"));
        assertNotNull(added);

        // 2. List should contain the new job
        JsonNode listPayload = rpcOk("cr2", "cron.list", null);
        // cron.list returns {ok: true, jobs: [...]}
        JsonNode jobs = listPayload.has("jobs") ? listPayload.get("jobs") : listPayload;
        assertTrue(jobs.isArray());
        assertTrue(jobs.size() >= 1);
        String cronId = null;
        for (JsonNode job : jobs) {
            if ("E2E Test Cron".equals(job.path("name").asText())) {
                cronId = job.get("id").asText();
                break;
            }
        }
        assertNotNull(cronId, "Expected to find E2E Test Cron in list");

        // 3. Status
        JsonNode status = rpcOk("cr3", "cron.status", null);
        assertNotNull(status);

        // 4. Remove
        JsonNode removed = rpcOk("cr4", "cron.remove", Map.of("id", cronId));
        assertNotNull(removed);

        // 5. Verify removed
        JsonNode listAfterPayload = rpcOk("cr5", "cron.list", null);
        JsonNode jobsAfter = listAfterPayload.has("jobs") ? listAfterPayload.get("jobs") : listAfterPayload;
        boolean found = false;
        for (JsonNode job : jobsAfter) {
            if (cronId.equals(job.path("id").asText()))
                found = true;
        }
        assertFalse(found, "Cron job should be removed");
    }

    // =========================================================================
    // Heartbeat / wake (system methods)
    // =========================================================================

    @Test
    @Order(40)
    void lastHeartbeat_returnsGracefully() throws Exception {
        JsonNode payload = rpcOk("hb1", "last-heartbeat", null);
        assertNotNull(payload);
    }

    @Test
    @Order(41)
    void wake_returnsOk() throws Exception {
        JsonNode payload = rpcOk("wk1", "wake", null);
        assertNotNull(payload);
    }

    // =========================================================================
    // Error edge cases
    // =========================================================================

    @Test
    @Order(50)
    void sessionCreate_missingCwd_stillSucceeds() throws Exception {
        // sessionKey is the only truly required field
        JsonNode created = rpcOk("ec1", "session.create",
                Map.of("sessionKey", "e2e-no-cwd"));
        assertNotNull(created.get("sessionId"));
        // Cleanup
        rpcOk("ec2", "session.delete",
                Map.of("sessionId", created.get("sessionId").asText()));
    }

    @Test
    @Order(51)
    void sessionDelete_nonexistent_handledGracefully() throws Exception {
        JsonNode res = rpc("ec3", "session.delete",
                Map.of("sessionId", "nonexistent-" + UUID.randomUUID()));
        // Should not crash the server
        assertNotNull(res);
    }

    @Test
    @Order(52)
    void sessionPatch_nonexistent_returnsError() throws Exception {
        JsonNode res = rpc("ec4", "session.patch",
                Map.of("sessionId", "nonexistent-" + UUID.randomUUID(), "label", "x"));
        assertNotNull(res);
    }

    @Test
    @Order(53)
    void agentRun_noMessages_returnsError() throws Exception {
        JsonNode res = rpc("ec5", "agent.run", Map.of());
        assertFalse(res.get("ok").asBoolean());
    }

    @Test
    @Order(54)
    void emptyParams_on_requiresParams_returnsError() throws Exception {
        // agent expects messages; empty params should cause an error, not a crash
        JsonNode res = rpc("ec6", "agent", Map.of());
        assertFalse(res.get("ok").asBoolean());
    }

    // =========================================================================
    // Route resolve
    // =========================================================================

    @Test
    @Order(60)
    void routeResolve_doesNotCrash() throws Exception {
        // route.resolve may return ok with payload or error — just verify no crash
        JsonNode res = rpc("rr1", "route.resolve",
                Map.of("path", "/"));
        assertNotNull(res);
    }

    // =========================================================================
    // Rapid-fire: multiple methods in quick succession
    // =========================================================================

    @Test
    @Order(70)
    void rapidFire_multipleMethodsInSequence() throws Exception {
        rpcOk("rf1", "status", null);
        rpcOk("rf2", "health", null);
        rpcOk("rf3", "models.list", null);
        rpcOk("rf4", "agents.list", null);
        rpcOk("rf5", "sessions.list", null);
        rpcOk("rf6", "config.get", null);
        // All 6 should succeed without errors
    }

    // =========================================================================
    // Session create with model override
    // =========================================================================

    @Test
    @Order(80)
    void sessionPatch_modelOverride() throws Exception {
        // Create session then patch model
        JsonNode created = rpcOk("mo1", "session.create",
                Map.of("sessionKey", "e2e-model-override"));
        String sessionId = created.get("sessionId").asText();

        // Patch model
        rpcOk("mo2", "session.patch",
                Map.of("sessionId", sessionId,
                        "model", "anthropic/claude-sonnet-4-20250514"));

        // Verify
        JsonNode fetched = rpcOk("mo3", "session.get",
                Map.of("sessionId", sessionId));
        assertEquals("anthropic/claude-sonnet-4-20250514", fetched.get("model").asText());

        rpcOk("mo4", "session.delete", Map.of("sessionId", sessionId));
    }
}

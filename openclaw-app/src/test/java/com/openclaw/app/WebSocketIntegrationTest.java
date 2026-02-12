package com.openclaw.app;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketHttpHeaders;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.client.standard.StandardWebSocketClient;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.net.URI;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for the Gateway WebSocket protocol.
 * Tests use the TS-aligned frame format:
 * - Request: {type:"req", id, method, params?}
 * - Response: {type:"res", id, ok, payload?, error?}
 * - Event: {type:"event", event, payload?}
 *
 * Each test performs the connect handshake before sending method requests.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@org.junit.jupiter.api.TestMethodOrder(org.junit.jupiter.api.MethodOrderer.OrderAnnotation.class)
class WebSocketIntegrationTest {

    @LocalServerPort
    private int port;

    private final ObjectMapper mapper = new ObjectMapper();
    private WebSocketSession session;
    private final ArrayBlockingQueue<String> messages = new ArrayBlockingQueue<>(20);

    @BeforeEach
    void connect() throws Exception {
        StandardWebSocketClient client = new StandardWebSocketClient();
        URI uri = URI.create("ws://127.0.0.1:" + port + "/ws");

        session = client.execute(new TextWebSocketHandler() {
            @Override
            protected void handleTextMessage(WebSocketSession s, TextMessage message) {
                messages.add(message.getPayload());
            }
        }, new WebSocketHttpHeaders(), uri).get(5, TimeUnit.SECONDS);

        assertNotNull(session);
        assertTrue(session.isOpen());

        // Receive connect.challenge event
        String challengeMsg = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(challengeMsg, "Expected connect.challenge event");
        JsonNode challenge = mapper.readTree(challengeMsg);
        assertEquals("event", challenge.get("type").asText());
        assertEquals("connect.challenge", challenge.get("event").asText());
        String nonce = challenge.get("payload").get("nonce").asText();
        assertNotNull(nonce);

        // Send connect handshake
        String connectReq = mapper.writeValueAsString(Map.of(
                "type", "req",
                "id", "connect-1",
                "method", "connect",
                "params", Map.of(
                        "minProtocol", 1,
                        "maxProtocol", 1,
                        "client", Map.of(
                                "id", "test-client",
                                "version", "0.0.1",
                                "platform", "test",
                                "mode", "test"),
                        "role", "operator",
                        "scopes", new String[] { "operator.admin" })));
        session.sendMessage(new TextMessage(connectReq));

        // Receive hello-ok response
        String helloMsg = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(helloMsg, "Expected hello-ok response");
        JsonNode hello = mapper.readTree(helloMsg);
        assertEquals("res", hello.get("type").asText());
        assertTrue(hello.get("ok").asBoolean());
        JsonNode helloOk = hello.get("payload");
        assertNotNull(helloOk);
        assertEquals("hello-ok", helloOk.get("type").asText());
        assertEquals(1, helloOk.get("protocol").asInt());
    }

    @AfterEach
    void disconnect() throws Exception {
        if (session != null && session.isOpen()) {
            session.close();
        }
        messages.clear();
    }

    // --- Helper ---

    private JsonNode sendRequest(String id, String method, Object params) throws Exception {
        String req = mapper.writeValueAsString(Map.of(
                "type", "req",
                "id", id,
                "method", method,
                "params", params != null ? params : Map.of()));
        session.sendMessage(new TextMessage(req));

        String response = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(response, "No response for method=" + method);
        return mapper.readTree(response);
    }

    // --- Tests ---

    @Test
    @org.junit.jupiter.api.Order(1)
    void rpcStatus_returnsOk() throws Exception {
        JsonNode json = sendRequest("1", "status", null);
        assertEquals("res", json.get("type").asText());
        assertEquals("1", json.get("id").asText());
        assertTrue(json.get("ok").asBoolean());
        JsonNode result = json.get("payload");
        assertNotNull(result);
        assertEquals("ok", result.get("status").asText());
    }

    @Test
    @org.junit.jupiter.api.Order(2)
    void rpcSessionList_returnsArray() throws Exception {
        JsonNode json = sendRequest("2", "sessions.list", null);
        assertEquals("2", json.get("id").asText());
        assertTrue(json.get("ok").asBoolean());
        assertTrue(json.get("payload").isArray());
    }

    @Test
    @org.junit.jupiter.api.Order(3)
    void rpcSessionCreate_returnsSession() throws Exception {
        JsonNode json = sendRequest("3", "session.create",
                Map.of("sessionKey", "test-key", "cwd", "/tmp"));
        assertEquals("3", json.get("id").asText());
        assertTrue(json.get("ok").asBoolean());
        JsonNode result = json.get("payload");
        assertNotNull(result.get("sessionId"));
        assertEquals("test-key", result.get("sessionKey").asText());
    }

    @Test
    @org.junit.jupiter.api.Order(4)
    void rpcConfigGet_returnsConfig() throws Exception {
        JsonNode json = sendRequest("4", "config.get", null);
        assertEquals("4", json.get("id").asText());
        assertTrue(json.get("ok").asBoolean());
        assertNotNull(json.get("payload"));
    }

    @Test
    @org.junit.jupiter.api.Order(5)
    void rpcUnknownMethod_returnsError() throws Exception {
        JsonNode json = sendRequest("5", "nonexistent.method", null);
        assertEquals("5", json.get("id").asText());
        assertFalse(json.get("ok").asBoolean());
        assertNotNull(json.get("error"));
        assertEquals("NOT_FOUND", json.get("error").get("code").asText());
    }

    @Test
    @org.junit.jupiter.api.Order(6)
    void agentRun_missingModelId_returnsError() throws Exception {
        JsonNode json = sendRequest("6", "agent.run",
                Map.of("messages", new Object[] { Map.of("role", "user", "content", "hello") }));
        assertEquals("6", json.get("id").asText());
        assertFalse(json.get("ok").asBoolean());
        assertNotNull(json.get("error"));
    }

    @Test
    @org.junit.jupiter.api.Order(7)
    void agentMessage_missingMessage_returnsError() throws Exception {
        JsonNode json = sendRequest("7", "agent.message", Map.of());
        assertEquals("7", json.get("id").asText());
        assertFalse(json.get("ok").asBoolean());
        assertNotNull(json.get("error"));
    }

    @Test
    @org.junit.jupiter.api.Order(8)
    void invalidHandshake_closesConnection() throws Exception {
        // Create a new connection that skips handshake
        StandardWebSocketClient client = new StandardWebSocketClient();
        URI uri = URI.create("ws://127.0.0.1:" + port + "/ws");
        ArrayBlockingQueue<String> msgs = new ArrayBlockingQueue<>(10);
        var rawSession = client.execute(new TextWebSocketHandler() {
            @Override
            protected void handleTextMessage(WebSocketSession s, TextMessage message) {
                msgs.add(message.getPayload());
            }
        }, new WebSocketHttpHeaders(), uri).get(5, TimeUnit.SECONDS);

        // Consume connect.challenge
        String challengeMsg = msgs.poll(5, TimeUnit.SECONDS);
        assertNotNull(challengeMsg);

        // Send a non-connect request before handshake
        String badReq = mapper.writeValueAsString(Map.of(
                "type", "req", "id", "bad-1", "method", "status", "params", Map.of()));
        rawSession.sendMessage(new TextMessage(badReq));

        // Should get error response
        String errorMsg = msgs.poll(5, TimeUnit.SECONDS);
        assertNotNull(errorMsg, "Expected error response for pre-handshake request");
        JsonNode error = mapper.readTree(errorMsg);
        assertFalse(error.get("ok").asBoolean());
        assertTrue(error.get("error").get("message").asText().contains("connect"));

        rawSession.close();
    }
}

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
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@org.junit.jupiter.api.TestMethodOrder(org.junit.jupiter.api.MethodOrderer.OrderAnnotation.class)
class WebSocketIntegrationTest {

    @LocalServerPort
    private int port;

    private final ObjectMapper mapper = new ObjectMapper();
    private WebSocketSession session;
    private final ArrayBlockingQueue<String> messages = new ArrayBlockingQueue<>(10);

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
    }

    @AfterEach
    void disconnect() throws Exception {
        if (session != null && session.isOpen()) {
            session.close();
        }
    }

    @Test
    @org.junit.jupiter.api.Order(1)
    void rpcStatus_returnsOk() throws Exception {
        String req = """
                {"jsonrpc":"2.0","id":1,"method":"status","params":{}}
                """;
        session.sendMessage(new TextMessage(req));

        String response = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(response, "No response received");

        JsonNode json = mapper.readTree(response);
        assertEquals("2.0", json.get("jsonrpc").asText());
        assertEquals(1, json.get("id").asInt());
        assertNotNull(json.get("result"));
        assertEquals("ok", json.get("result").get("status").asText());
    }

    @Test
    @org.junit.jupiter.api.Order(2)
    void rpcSessionList_returnsEmptyArray() throws Exception {
        String req = """
                {"jsonrpc":"2.0","id":2,"method":"session.list","params":{}}
                """;
        session.sendMessage(new TextMessage(req));

        String response = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(response, "No response received");

        JsonNode json = mapper.readTree(response);
        assertEquals(2, json.get("id").asInt());
        assertTrue(json.get("result").isArray());
    }

    @Test
    @org.junit.jupiter.api.Order(3)
    void rpcSessionCreate_returnsSession() throws Exception {
        String req = """
                {"jsonrpc":"2.0","id":3,"method":"session.create","params":{"sessionKey":"test-key","cwd":"/tmp"}}
                """;
        session.sendMessage(new TextMessage(req));

        String response = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(response, "No response received");

        JsonNode json = mapper.readTree(response);
        assertEquals(3, json.get("id").asInt());
        JsonNode result = json.get("result");
        assertNotNull(result.get("sessionId"));
        assertEquals("test-key", result.get("sessionKey").asText());
    }

    @Test
    @org.junit.jupiter.api.Order(4)
    void rpcConfigGet_returnsConfig() throws Exception {
        String req = """
                {"jsonrpc":"2.0","id":4,"method":"config.get","params":{}}
                """;
        session.sendMessage(new TextMessage(req));

        String response = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(response, "No response received");

        JsonNode json = mapper.readTree(response);
        assertEquals(4, json.get("id").asInt());
        assertNotNull(json.get("result"));
    }

    @Test
    @org.junit.jupiter.api.Order(5)
    void rpcUnknownMethod_returnsError() throws Exception {
        String req = """
                {"jsonrpc":"2.0","id":5,"method":"nonexistent.method","params":{}}
                """;
        session.sendMessage(new TextMessage(req));

        String response = messages.poll(5, TimeUnit.SECONDS);
        assertNotNull(response, "No response received");

        JsonNode json = mapper.readTree(response);
        assertEquals(5, json.get("id").asInt());
        assertNotNull(json.get("error"));
        assertEquals(-32601, json.get("error").get("code").asInt());
    }
}

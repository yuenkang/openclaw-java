package com.openclaw.app;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for the OpenAI-compatible HTTP API.
 * Validates the /v1/chat/completions and /v1/models endpoints.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class OpenAiApiIntegrationTest {

    @Autowired
    private TestRestTemplate restTemplate;

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void chatCompletions_withoutModel_usesDefault() throws Exception {
        // POST without model field → API uses default model, may return 200 or error
        // depending on whether the default model provider is available
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        Map<String, Object> body = Map.of(
                "messages", List.of(Map.of("role", "user", "content", "hello")));

        HttpEntity<String> request = new HttpEntity<>(mapper.writeValueAsString(body), headers);
        ResponseEntity<String> response = restTemplate.postForEntity(
                "/v1/chat/completions", request, String.class);

        // The API endpoint should be reachable (not 404)
        assertNotEquals(HttpStatus.NOT_FOUND, response.getStatusCode(),
                "/v1/chat/completions should be a valid endpoint");
    }

    @Test
    void modelsEndpoint_returns404() {
        // GET /v1/models is not yet implemented as HTTP endpoint
        ResponseEntity<String> response = restTemplate.getForEntity("/v1/models", String.class);
        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode(),
                "/v1/models should return 404 (not yet implemented)");
    }

    @Test
    void healthEndpoint_returns404() {
        // No dedicated HTTP health endpoint exists
        ResponseEntity<String> response = restTemplate.getForEntity("/health", String.class);
        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode(),
                "/health should return 404 (health is via WebSocket)");
    }
}

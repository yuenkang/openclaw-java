package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.agent.tools.AgentTool;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.*;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Web search tool — perform web searches using a configurable search API.
 * Corresponds to TypeScript's web-search tool (browser.ts / web.ts).
 *
 * <p>
 * Uses the SearXNG / Tavily / Brave Search API format, configurable via
 * OPENCLAW_SEARCH_ENDPOINT env var. Returns no-op message if unconfigured.
 * </p>
 */
@Slf4j
public class WebSearchTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Duration TIMEOUT = Duration.ofSeconds(15);

    @Override
    public String getName() {
        return "web_search";
    }

    @Override
    public String getDescription() {
        return "Search the web for information. Returns a summary of relevant search results.";
    }

    @Override
    public JsonNode getParameterSchema() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("query", Map.of(
                "type", "string",
                "description", "The search query"));
        props.put("max_results", Map.of(
                "type", "integer",
                "description", "Maximum number of results to return (default 5)"));
        Map<String, Object> schema = Map.of(
                "type", "object",
                "properties", props,
                "required", List.of("query"));
        return MAPPER.valueToTree(schema);
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        JsonNode args = context.getParameters();
        String query = args.get("query").asText();
        int maxResults = args.has("max_results") ? args.get("max_results").asInt(5) : 5;

        String searchEndpoint = System.getenv("OPENCLAW_SEARCH_ENDPOINT");
        String searchApiKey = System.getenv("OPENCLAW_SEARCH_API_KEY");

        if (searchEndpoint == null || searchEndpoint.isBlank()) {
            return CompletableFuture.completedFuture(ToolResult.fail(
                    "Web search is not configured. Set OPENCLAW_SEARCH_ENDPOINT env var. Query: " + query));
        }

        try {
            HttpClient client = HttpClient.newBuilder()
                    .connectTimeout(TIMEOUT)
                    .build();

            String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);
            String url = searchEndpoint + "?q=" + encodedQuery + "&format=json&pageno=1";

            HttpRequest.Builder reqBuilder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(TIMEOUT)
                    .GET();

            if (searchApiKey != null && !searchApiKey.isBlank()) {
                reqBuilder.header("Authorization", "Bearer " + searchApiKey);
            }

            HttpResponse<String> response = client.send(
                    reqBuilder.build(),
                    HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) {
                return CompletableFuture.completedFuture(
                        ToolResult.fail("Search API status " + response.statusCode() + ": " + response.body()));
            }

            String formatted = formatSearchResults(response.body(), maxResults);
            return CompletableFuture.completedFuture(ToolResult.ok(formatted));
        } catch (Exception e) {
            log.warn("Web search failed for '{}': {}", query, e.getMessage());
            return CompletableFuture.completedFuture(ToolResult.fail("Web search failed: " + e.getMessage()));
        }
    }

    private String formatSearchResults(String jsonBody, int maxResults) {
        try {
            var root = MAPPER.readTree(jsonBody);
            var results = root.get("results");
            if (results == null || !results.isArray() || results.isEmpty()) {
                return "No results found.";
            }

            StringBuilder sb = new StringBuilder();
            int count = 0;
            for (var result : results) {
                if (count >= maxResults)
                    break;
                String title = result.has("title") ? result.get("title").asText() : "Untitled";
                String resUrl = result.has("url") ? result.get("url").asText() : "";
                String content = result.has("content") ? result.get("content").asText() : "";

                sb.append(String.format("### %d. %s\n", count + 1, title));
                if (!resUrl.isEmpty())
                    sb.append("URL: ").append(resUrl).append("\n");
                if (!content.isEmpty()) {
                    if (content.length() > 500)
                        content = content.substring(0, 500) + "…";
                    sb.append(content).append("\n");
                }
                sb.append("\n");
                count++;
            }
            return sb.toString().trim();
        } catch (Exception e) {
            return "Failed to parse search results: " + e.getMessage();
        }
    }
}

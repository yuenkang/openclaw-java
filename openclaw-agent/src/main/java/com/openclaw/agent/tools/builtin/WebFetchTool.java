package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;

/**
 * Web fetch tool — fetch and extract content from URLs.
 * Corresponds to TypeScript's tools/web-fetch-utils.ts + web-fetch.ts.
 */
@Slf4j
public class WebFetchTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final int MAX_BODY_CHARS = 50_000;
    private static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(30);

    private static final Pattern PRIVATE_HOST_RE = Pattern.compile(
            "^(10\\.|172\\.(1[6-9]|2[0-9]|3[01])\\.|192\\.168\\.|" +
                    "127\\.|0\\.|169\\.254\\.|localhost|\\[::1]|\\[fc|\\[fd)",
            Pattern.CASE_INSENSITIVE);

    private final HttpClient httpClient;

    public WebFetchTool() {
        this.httpClient = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NORMAL)
                .connectTimeout(Duration.ofSeconds(10))
                .build();
    }

    @Override
    public String getName() {
        return "web_fetch";
    }

    @Override
    public String getDescription() {
        return "Fetch the content of a URL and extract readable text. " +
                "Use for reading web pages, documentation, or API responses.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        ObjectNode url = properties.putObject("url");
        url.put("type", "string");
        url.put("description", "The URL to fetch");

        ObjectNode extractMode = properties.putObject("extractMode");
        extractMode.put("type", "string");
        extractMode.put("description", "Extract mode: 'markdown' or 'text' (default: markdown)");

        ObjectNode maxChars = properties.putObject("maxChars");
        maxChars.put("type", "number");
        maxChars.put("description", "Maximum characters to return (default: 50000)");

        schema.putArray("required").add("url");
        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String url = ToolParamUtils.readStringParam(params, "url");
            if (url == null || url.isBlank())
                return ToolResult.fail("'url' is required");

            URI uri = URI.create(url);
            String host = uri.getHost();
            if (host != null && PRIVATE_HOST_RE.matcher(host).find()) {
                return ToolResult.fail("Blocked: cannot fetch private/reserved addresses");
            }

            String mode = ToolParamUtils.readStringParam(params, "extractMode");
            if (mode == null)
                mode = "markdown";

            Integer maxCharsParam = ToolParamUtils.readIntegerParam(params, "maxChars");
            int maxChars = maxCharsParam != null ? maxCharsParam : MAX_BODY_CHARS;

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(uri).timeout(REQUEST_TIMEOUT)
                    .header("User-Agent", "Mozilla/5.0 (compatible; OpenClaw/1.0)")
                    .GET().build();

            HttpResponse<String> response = httpClient.send(
                    request, HttpResponse.BodyHandlers.ofString());

            String body = response.body() != null ? response.body() : "";
            String contentType = response.headers()
                    .firstValue("content-type").orElse("text/html");

            String text;
            String title = null;
            if (contentType.contains("text/html") || contentType.contains("application/xhtml")) {
                ExtractedContent extracted = htmlToMarkdown(body);
                text = "text".equals(mode) ? markdownToText(extracted.text) : extracted.text;
                title = extracted.title;
            } else {
                text = body;
            }

            boolean truncated = text.length() > maxChars;
            if (truncated)
                text = text.substring(0, maxChars);

            ObjectNode result = MAPPER.createObjectNode();
            result.put("url", url);
            result.put("statusCode", response.statusCode());
            if (title != null)
                result.put("title", title);
            result.put("text", text);
            result.put("truncated", truncated);
            result.put("originalLength", body.length());

            return ToolResult.ok(ToolParamUtils.toJsonString(result), result);

        } catch (IOException e) {
            return ToolResult.fail("Fetch failed: " + e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return ToolResult.fail("Fetch interrupted");
        } catch (Exception e) {
            log.error("web_fetch error: {}", e.getMessage(), e);
            return ToolResult.fail("Web fetch error: " + e.getMessage());
        }
    }

    static ExtractedContent htmlToMarkdown(String html) {
        String title = null;
        var titleMatch = Pattern.compile("<title[^>]*>([\\s\\S]*?)</title>",
                Pattern.CASE_INSENSITIVE).matcher(html);
        if (titleMatch.find())
            title = stripTags(decodeEntities(titleMatch.group(1))).trim();

        String text = html;
        text = text.replaceAll("(?si)<script[\\s\\S]*?</script>", "");
        text = text.replaceAll("(?si)<style[\\s\\S]*?</style>", "");
        text = text.replaceAll("(?si)<noscript[\\s\\S]*?</noscript>", "");
        text = text.replaceAll("(?si)<a\\s+[^>]*href=[\"']([^\"']+)[\"'][^>]*>([\\s\\S]*?)</a>", "[$2]($1)");
        for (int i = 1; i <= 6; i++) {
            String prefix = "#".repeat(i);
            text = text.replaceAll("(?si)<h" + i + "[^>]*>([\\s\\S]*?)</h" + i + ">",
                    "\n" + prefix + " $1\n");
        }
        text = text.replaceAll("(?si)<li[^>]*>([\\s\\S]*?)</li>", "\n- $1");
        text = text.replaceAll("(?i)<(br|hr)\\s*/?>", "\n");
        text = text.replaceAll("(?i)</(p|div|section|article|header|footer|table|tr|ul|ol)>", "\n");
        text = stripTags(text);
        text = decodeEntities(text);
        text = normalizeWhitespace(text);
        return new ExtractedContent(text, title);
    }

    static String markdownToText(String md) {
        String text = md;
        text = text.replaceAll("!\\[[^\\]]*]\\([^)]+\\)", "");
        text = text.replaceAll("\\[([^\\]]+)]\\([^)]+\\)", "$1");
        text = text.replaceAll("(?s)```[\\s\\S]*?```", "");
        text = text.replaceAll("`([^`]+)`", "$1");
        text = text.replaceAll("(?m)^#{1,6}\\s+", "");
        return normalizeWhitespace(text);
    }

    private static String decodeEntities(String v) {
        return v.replaceAll("(?i)&nbsp;", " ").replaceAll("(?i)&amp;", "&")
                .replaceAll("(?i)&quot;", "\"").replaceAll("(?i)&#39;", "'")
                .replaceAll("(?i)&lt;", "<").replaceAll("(?i)&gt;", ">");
    }

    private static String stripTags(String v) {
        return decodeEntities(v.replaceAll("<[^>]+>", ""));
    }

    private static String normalizeWhitespace(String v) {
        return v.replaceAll("\\r", "").replaceAll("[ \\t]+\\n", "\n")
                .replaceAll("\\n{3,}", "\n\n").replaceAll("[ \\t]{2,}", " ").trim();
    }

    static class ExtractedContent {
        final String text;
        final String title;

        ExtractedContent(String text, String title) {
            this.text = text;
            this.title = title;
        }
    }
}

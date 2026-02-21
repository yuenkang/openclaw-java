package com.openclaw.agent.tools.builtin.browser;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.browser.BrowserClient;
import com.openclaw.browser.BrowserTypes;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Browser control tool for the agent.
 * Corresponds to TypeScript's agents/tools/browser-tool.ts createBrowserTool().
 *
 * <p>
 * Delegates all operations to {@link BrowserClient} via HTTP, which in turn
 * calls {@link com.openclaw.browser.BrowserControlServer}.
 * This enables multi-profile support — the LLM can pass a {@code profile}
 * parameter to target a specific browser profile.
 * </p>
 */
@Slf4j
public class BrowserTool implements AgentTool {

    private static final ObjectMapper mapper = new ObjectMapper();

    private final BrowserClient client;

    /**
     * JSON Schema for the tool parameters (built once, cached).
     */
    private static final JsonNode PARAMETER_SCHEMA = buildParameterSchema();

    public BrowserTool(BrowserClient client) {
        this.client = client;
    }

    @Override
    public String getName() {
        return "browser";
    }

    @Override
    public String getDescription() {
        return String.join(" ",
                "Control the browser (status/start/stop/tabs/open/snapshot/screenshot/act).",
                "Use snapshot+act for UI automation.",
                "act kinds: click, type, press, hover, scrollIntoView, drag, select, fill, resize, wait, evaluate, goBack, goForward, close.",
                "When using refs from snapshot, keep the same tab via targetId.",
                "screenshot supports ref/element for element-level capture.",
                "wait supports: timeMs, text, textGone, selector, url, loadState, fn.",
                "Use the 'profile' param to target a specific browser profile (default: config default).");
    }

    @Override
    public JsonNode getParameterSchema() {
        return PARAMETER_SCHEMA;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return doExecute(context.getParameters());
            } catch (Exception e) {
                log.error("Browser tool error: {}", e.getMessage(), e);
                return ToolResult.fail(e.getMessage());
            }
        });
    }

    private ToolResult doExecute(JsonNode params) throws Exception {
        String action = requireString(params, "action");
        String profile = optString(params, "profile");

        switch (action) {
            case "status": {
                BrowserTypes.BrowserStatus status = client.status(profile);
                return jsonResult(status);
            }
            case "start": {
                boolean headless = optBoolean(params, "headless", false);
                client.start(profile, headless);
                return jsonResult(Map.of("ok", true));
            }
            case "stop": {
                client.stop(profile);
                return jsonResult(Map.of("ok", true, "stopped", true));
            }
            case "profiles": {
                List<BrowserTypes.ProfileStatus> profiles = client.profiles();
                return jsonResult(Map.of("profiles", profiles));
            }
            case "tabs": {
                List<BrowserTypes.BrowserTab> tabs = client.tabs(profile);
                return jsonResult(Map.of("tabs", tabs));
            }
            case "open": {
                String targetUrl = requireString(params, "targetUrl");
                BrowserTypes.BrowserTab tab = client.openTab(targetUrl, profile);
                return jsonResult(tab);
            }
            case "focus": {
                String targetId = requireString(params, "targetId");
                client.focusTab(targetId, profile);
                return jsonResult(Map.of("ok", true));
            }
            case "close": {
                String targetId = optString(params, "targetId");
                if (targetId != null) {
                    client.closeTab(targetId, profile);
                } else {
                    // Close active tab via act
                    Map<String, Object> request = new LinkedHashMap<>();
                    request.put("kind", "close");
                    client.act(request, profile);
                }
                return jsonResult(Map.of("ok", true));
            }
            case "snapshot": {
                return handleSnapshot(params, profile);
            }
            case "screenshot": {
                return handleScreenshot(params, profile);
            }
            case "navigate": {
                String targetUrl = requireString(params, "targetUrl");
                String targetId = optString(params, "targetId");
                BrowserTypes.NavigateResult nav = client.navigate(targetUrl, targetId, profile);
                return jsonResult(nav);
            }
            case "console": {
                String targetId = optString(params, "targetId");
                String level = optString(params, "level");
                JsonNode messages = client.consoleMessages(level, targetId, profile);
                return jsonResult(messages);
            }
            case "pdf": {
                String targetId = optString(params, "targetId");
                BrowserTypes.PdfResult pdf = client.pdf(targetId, profile);
                return jsonResult(pdf);
            }
            case "upload": {
                return ToolResult.fail("File upload not yet implemented");
            }
            case "dialog": {
                return ToolResult.fail("Dialog handling not yet implemented");
            }
            case "act": {
                return handleAct(params, profile);
            }
            default:
                return ToolResult.fail("Unknown browser action: " + action);
        }
    }

    // ===== Action handlers =====

    private ToolResult handleSnapshot(JsonNode params, String profile) throws Exception {
        Map<String, Object> opts = new LinkedHashMap<>();
        String targetId = optString(params, "targetId");
        if (targetId != null)
            opts.put("targetId", targetId);
        if (profile != null)
            opts.put("profile", profile);

        BrowserTypes.SnapshotResult snap = client.snapshot(opts);

        // Return snapshot text as the primary content (LLM reads this)
        if (snap.getSnapshot() != null) {
            return ToolResult.ok(snap.getSnapshot(), snap);
        }
        return jsonResult(snap);
    }

    private ToolResult handleScreenshot(JsonNode params, String profile) throws Exception {
        Map<String, Object> body = new LinkedHashMap<>();
        String targetId = optString(params, "targetId");
        if (targetId != null)
            body.put("targetId", targetId);
        boolean fullPage = params.has("fullPage") && params.get("fullPage").asBoolean();
        if (fullPage)
            body.put("fullPage", true);
        String ref = optString(params, "ref");
        String element = optString(params, "element");
        if (ref != null)
            body.put("ref", ref);
        if (element != null)
            body.put("element", element);

        if (fullPage && (ref != null || element != null)) {
            return ToolResult.fail("fullPage is not supported for element screenshots");
        }

        BrowserTypes.ScreenshotResult shot = client.screenshot(body, profile);

        // Build multimodal result with image content part
        if (shot.getData() != null && !shot.getData().isEmpty()) {
            String mimeType = shot.getContentType() != null ? shot.getContentType() : "image/png";
            String dataUri = "data:" + mimeType + ";base64," + shot.getData();

            List<ModelProvider.ContentPart> parts = new ArrayList<>();
            parts.add(ModelProvider.ContentPart.builder()
                    .type("image_url")
                    .imageUrl(ModelProvider.ImageUrl.builder().url(dataUri).build())
                    .build());
            parts.add(ModelProvider.ContentPart.builder()
                    .type("text")
                    .text("Screenshot captured" + (shot.getUrl() != null ? " of " + shot.getUrl() : ""))
                    .build());

            return ToolResult.builder()
                    .success(true)
                    .output("Screenshot captured")
                    .contentParts(parts)
                    .build();
        }

        return jsonResult(shot);
    }

    private ToolResult handleAct(JsonNode params, String profile) throws Exception {
        JsonNode requestNode = params.get("request");
        if (requestNode == null || !requestNode.isObject()) {
            return ToolResult.fail("request required");
        }

        Map<String, Object> request = mapper.convertValue(requestNode,
                new TypeReference<LinkedHashMap<String, Object>>() {
                });

        String kind = (String) request.get("kind");
        if (kind == null) {
            return ToolResult.fail("request.kind is required");
        }

        JsonNode result = client.act(request, profile);
        return jsonResult(result);
    }

    // ===== Utility methods =====

    private ToolResult jsonResult(Object data) {
        try {
            String json = mapper.writeValueAsString(data);
            return ToolResult.ok(json, data);
        } catch (Exception e) {
            return ToolResult.fail("Failed to serialize result: " + e.getMessage());
        }
    }

    private static String requireString(JsonNode params, String field) {
        JsonNode node = params.get(field);
        if (node == null || node.isNull() || node.asText().isBlank()) {
            throw new IllegalArgumentException(field + " is required");
        }
        return node.asText().trim();
    }

    private static String optString(JsonNode params, String field) {
        JsonNode node = params.get(field);
        if (node == null || node.isNull())
            return null;
        String val = node.asText().trim();
        return val.isEmpty() ? null : val;
    }

    private static boolean optBoolean(JsonNode params, String field, boolean defaultValue) {
        JsonNode node = params.get(field);
        if (node == null || node.isNull())
            return defaultValue;
        return node.asBoolean(defaultValue);
    }

    // ===== JSON Schema =====

    private static JsonNode buildParameterSchema() {
        ObjectMapper m = new ObjectMapper();
        ObjectNode schema = m.createObjectNode();
        schema.put("type", "object");

        ObjectNode props = schema.putObject("properties");

        // action (required enum)
        ObjectNode action = props.putObject("action");
        action.put("type", "string");
        action.putArray("enum")
                .add("status").add("start").add("stop").add("profiles").add("tabs")
                .add("open").add("focus").add("close").add("snapshot").add("screenshot")
                .add("navigate").add("console").add("pdf").add("upload").add("dialog").add("act");

        // Simple string params
        for (String f : List.of("profile", "targetUrl", "targetId", "node",
                "selector", "frame", "element", "inputRef", "level", "promptText")) {
            props.putObject(f).put("type", "string");
        }

        // Enum string params
        ObjectNode target = props.putObject("target");
        target.put("type", "string");
        target.putArray("enum").add("sandbox").add("host").add("node");

        ObjectNode snapshotFormat = props.putObject("snapshotFormat");
        snapshotFormat.put("type", "string");
        snapshotFormat.putArray("enum").add("aria").add("ai");

        ObjectNode mode = props.putObject("mode");
        mode.put("type", "string");
        mode.putArray("enum").add("efficient");

        ObjectNode refs = props.putObject("refs");
        refs.put("type", "string");
        refs.putArray("enum").add("role").add("aria");

        ObjectNode imgType = props.putObject("type");
        imgType.put("type", "string");
        imgType.putArray("enum").add("png").add("jpeg");

        // Number params
        for (String f : List.of("limit", "maxChars", "depth", "timeoutMs", "width", "height")) {
            props.putObject(f).put("type", "number");
        }

        // Boolean params
        for (String f : List.of("interactive", "compact", "labels", "fullPage", "accept", "headless")) {
            props.putObject(f).put("type", "boolean");
        }

        // ref (string)
        props.putObject("ref").put("type", "string");

        // paths (array of strings)
        ObjectNode paths = props.putObject("paths");
        paths.put("type", "array");
        paths.putObject("items").put("type", "string");

        // request (object)
        props.putObject("request").put("type", "object");

        // required
        schema.putArray("required").add("action");

        return schema;
    }
}

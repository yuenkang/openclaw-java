package com.openclaw.agent.tools.builtin.browser;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Browser control tool for the agent.
 * Corresponds to TypeScript's agents/tools/browser-tool.ts createBrowserTool().
 *
 * <p>
 * Uses {@link PlaywrightSession} directly (in-process) instead of HTTP.
 * Exposes 15 actions to the LLM: status, start, stop, profiles, tabs,
 * open, focus, close, snapshot, screenshot, navigate, console, pdf, upload,
 * dialog, act.
 * </p>
 */
@Slf4j
public class BrowserTool implements AgentTool {

    private static final ObjectMapper mapper = new ObjectMapper();

    /**
     * Shared singleton session — all BrowserTool instances share the same browser.
     */
    private static final PlaywrightSession session = new PlaywrightSession();

    private final OpenClawConfig config;
    private boolean headless = false;

    /**
     * JSON Schema for the tool parameters (built once, cached).
     */
    private static final JsonNode PARAMETER_SCHEMA = buildParameterSchema();

    public BrowserTool(OpenClawConfig config) {
        this.config = config;
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
                "wait supports: timeMs, text, textGone, selector, url, loadState, fn.");
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

    private ToolResult doExecute(JsonNode params) {
        String action = requireString(params, "action");

        switch (action) {
            case "status": {
                Map<String, Object> status = new LinkedHashMap<>();
                status.put("enabled", true);
                status.put("running", session.isRunning());
                status.put("headless", headless);
                return jsonResult(status);
            }
            case "start": {
                ensureBrowserStarted();
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("ok", true);
                result.put("running", session.isRunning());
                return jsonResult(result);
            }
            case "stop": {
                boolean stopped = session.stop();
                return jsonResult(Map.of("ok", true, "stopped", stopped));
            }
            case "profiles": {
                Map<String, Object> profile = new LinkedHashMap<>();
                profile.put("name", "default");
                profile.put("running", session.isRunning());
                return jsonResult(Map.of("profiles", List.of(profile)));
            }
            case "tabs": {
                if (!session.isRunning()) {
                    return jsonResult(Map.of("tabs", List.of()));
                }
                List<PlaywrightSession.TabInfo> tabs = session.listTabs();
                return jsonResult(Map.of("tabs", tabs));
            }
            case "open": {
                String targetUrl = requireString(params, "targetUrl");
                ensureBrowserStarted();
                PlaywrightSession.TabInfo tab = session.openTab(targetUrl);
                return jsonResult(tab);
            }
            case "focus": {
                String targetId = requireString(params, "targetId");
                session.focusTab(targetId);
                return jsonResult(Map.of("ok", true));
            }
            case "close": {
                String targetId = optString(params, "targetId");
                if (targetId != null) {
                    session.closeTab(targetId);
                } else {
                    session.act("close", null, Map.of());
                }
                return jsonResult(Map.of("ok", true));
            }
            case "snapshot": {
                return handleSnapshot(params);
            }
            case "screenshot": {
                return handleScreenshot(params);
            }
            case "navigate": {
                String targetUrl = requireString(params, "targetUrl");
                String targetId = optString(params, "targetId");
                ensureBrowserStarted();
                PlaywrightSession.NavigateResult nav = session.navigate(targetUrl, targetId);
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("ok", true);
                result.put("url", nav.getUrl());
                result.put("title", nav.getTitle());
                return jsonResult(result);
            }
            case "console": {
                String targetId = optString(params, "targetId");
                if (!session.isRunning()) {
                    return jsonResult(Map.of("ok", true, "messages", List.of()));
                }
                List<PlaywrightSession.ConsoleEntry> messages = session.getConsoleMessages(targetId);
                List<Map<String, Object>> mapped = messages.stream().map(e -> {
                    Map<String, Object> m = new LinkedHashMap<>();
                    m.put("type", e.getType());
                    m.put("text", e.getText());
                    m.put("timestamp", e.getTimestamp());
                    return m;
                }).toList();
                return jsonResult(Map.of("ok", true, "messages", mapped));
            }
            case "pdf": {
                return ToolResult.fail("PDF export not yet implemented");
            }
            case "upload": {
                return ToolResult.fail("File upload not yet implemented");
            }
            case "dialog": {
                return ToolResult.fail("Dialog handling not yet implemented");
            }
            case "act": {
                return handleAct(params);
            }
            default:
                return ToolResult.fail("Unknown browser action: " + action);
        }
    }

    // ===== Action handlers =====

    private ToolResult handleSnapshot(JsonNode params) {
        String targetId = optString(params, "targetId");
        ensureBrowserStarted();
        PlaywrightSession.SnapshotResult snap = session.snapshot(targetId);

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("format", "aria");
        result.put("snapshot", snap.getSnapshot());
        result.put("url", snap.getUrl());
        result.put("title", snap.getTitle());

        // Return snapshot text as the primary content (LLM reads this)
        if (snap.getSnapshot() != null) {
            return ToolResult.ok(snap.getSnapshot(), result);
        }
        return jsonResult(result);
    }

    private ToolResult handleScreenshot(JsonNode params) {
        String targetId = optString(params, "targetId");
        boolean fullPage = params.has("fullPage") && params.get("fullPage").asBoolean();
        String ref = optString(params, "ref");
        String element = optString(params, "element");
        ensureBrowserStarted();

        if (fullPage && (ref != null || element != null)) {
            return ToolResult.fail("fullPage is not supported for element screenshots");
        }

        PlaywrightSession.ScreenshotResult shot = session.screenshot(targetId, fullPage, ref, element);
        String base64 = Base64.getEncoder().encodeToString(shot.getBuffer());

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("data", base64);
        result.put("contentType", shot.getContentType());
        result.put("url", shot.getUrl());
        result.put("title", shot.getTitle());
        return jsonResult(result);
    }

    private ToolResult handleAct(JsonNode params) {
        JsonNode requestNode = params.get("request");
        if (requestNode == null || !requestNode.isObject()) {
            return ToolResult.fail("request required");
        }

        Map<String, Object> request = mapper.convertValue(requestNode,
                new com.fasterxml.jackson.core.type.TypeReference<LinkedHashMap<String, Object>>() {
                });

        String kind = (String) request.get("kind");
        if (kind == null) {
            return ToolResult.fail("request.kind is required");
        }

        String targetId = (String) request.get("targetId");
        ensureBrowserStarted();

        try {
            Map<String, Object> result = session.act(kind, targetId, request);
            return jsonResult(result);
        } catch (RuntimeException e) {
            return ToolResult.fail(e.getMessage());
        }
    }

    // ===== Utility methods =====

    private void ensureBrowserStarted() {
        if (!session.isRunning()) {
            session.launch(headless);
        }
    }

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
        for (String f : List.of("interactive", "compact", "labels", "fullPage", "accept")) {
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

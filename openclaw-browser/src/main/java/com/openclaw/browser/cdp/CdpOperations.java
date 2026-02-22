package com.openclaw.browser.cdp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * High-level CDP operations using the raw CdpClient.
 * Corresponds to the exported functions in TypeScript's cdp.ts.
 *
 * <p>All methods use {@link CdpClient#withCdpSocket(String, CdpClient.CdpAction)} internally —
 * each call opens a fresh WebSocket, performs the operation, and closes.
 */
@Slf4j
public final class CdpOperations {

    private static final ObjectMapper mapper = new ObjectMapper();

    private CdpOperations() {
    }

    // ==================== Screenshot ====================

    /**
     * Capture a PNG screenshot via CDP.
     */
    public static byte[] captureScreenshotPng(String wsUrl, boolean fullPage)
            throws CdpClient.CdpException {
        return captureScreenshot(wsUrl, fullPage, "png", 0);
    }

    /**
     * Capture a screenshot (png or jpeg) via CDP.
     */
    public static byte[] captureScreenshot(String wsUrl, boolean fullPage,
                                           String format, int quality)
            throws CdpClient.CdpException {
        return CdpClient.withCdpSocket(wsUrl, send -> {
            send.send("Page.enable", null);

            ObjectNode ssParams = mapper.createObjectNode();
            ssParams.put("format", format != null ? format : "png");
            ssParams.put("fromSurface", true);
            ssParams.put("captureBeyondViewport", true);

            if ("jpeg".equals(format) && quality > 0) {
                ssParams.put("quality", Math.max(0, Math.min(100, quality)));
            }

            if (fullPage) {
                JsonNode metrics = send.send("Page.getLayoutMetrics", null);
                JsonNode size = metrics.has("cssContentSize")
                        ? metrics.get("cssContentSize")
                        : metrics.has("contentSize") ? metrics.get("contentSize") : null;
                if (size != null) {
                    double w = size.has("width") ? size.get("width").asDouble() : 0;
                    double h = size.has("height") ? size.get("height").asDouble() : 0;
                    if (w > 0 && h > 0) {
                        ObjectNode clip = mapper.createObjectNode();
                        clip.put("x", 0);
                        clip.put("y", 0);
                        clip.put("width", w);
                        clip.put("height", h);
                        clip.put("scale", 1);
                        ssParams.set("clip", clip);
                    }
                }
            }

            JsonNode result = send.send("Page.captureScreenshot", ssParams);
            String base64 = result.has("data") ? result.get("data").asText("") : "";
            if (base64.isEmpty()) {
                throw new CdpClient.CdpException("Screenshot failed: missing data");
            }
            return Base64.getDecoder().decode(base64);
        });
    }

    // ==================== JavaScript Evaluation ====================

    /**
     * Evaluate JavaScript in the page context.
     */
    public static CdpTypes.CdpEvalResult evaluateJavaScript(String wsUrl, String expression,
                                                             boolean awaitPromise,
                                                             boolean returnByValue)
            throws CdpClient.CdpException {
        return CdpClient.withCdpSocket(wsUrl, send -> {
            try {
                send.send("Runtime.enable", null);
            } catch (Exception e) {
                // ignore — may not be supported
            }

            ObjectNode params = mapper.createObjectNode();
            params.put("expression", expression);
            params.put("awaitPromise", awaitPromise);
            params.put("returnByValue", returnByValue);
            params.put("userGesture", true);
            params.put("includeCommandLineAPI", true);

            JsonNode result = send.send("Runtime.evaluate", params);

            CdpTypes.CdpRemoteObject remoteObj = null;
            CdpTypes.CdpExceptionDetails exceptionDetails = null;

            try {
                if (result.has("result")) {
                    remoteObj = mapper.treeToValue(result.get("result"), CdpTypes.CdpRemoteObject.class);
                }
                if (result.has("exceptionDetails")) {
                    exceptionDetails = mapper.treeToValue(result.get("exceptionDetails"),
                            CdpTypes.CdpExceptionDetails.class);
                }
            } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
                throw new CdpClient.CdpException("Failed to parse eval result: " + e.getMessage());
            }
            if (remoteObj == null) {
                throw new CdpClient.CdpException("CDP Runtime.evaluate returned no result");
            }
            return CdpTypes.CdpEvalResult.builder()
                    .result(remoteObj)
                    .exceptionDetails(exceptionDetails)
                    .build();
        });
    }

    // ==================== Aria Snapshot ====================

    /**
     * Get accessibility tree snapshot via CDP.
     */
    public static List<CdpTypes.AriaSnapshotNode> snapshotAria(String wsUrl, int limit)
            throws CdpClient.CdpException {
        int effectiveLimit = Math.max(1, Math.min(2000, limit > 0 ? limit : 500));
        return CdpClient.withCdpSocket(wsUrl, send -> {
            try {
                send.send("Accessibility.enable", null);
            } catch (Exception e) {
                // ignore
            }
            JsonNode result = send.send("Accessibility.getFullAXTree", null);
            if (!result.has("nodes") || !result.get("nodes").isArray()) {
                return List.of();
            }

            try {
                List<CdpTypes.RawAXNode> rawNodes = new ArrayList<>();
                for (JsonNode node : result.get("nodes")) {
                    rawNodes.add(mapper.treeToValue(node, CdpTypes.RawAXNode.class));
                }
                return formatAriaSnapshot(rawNodes, effectiveLimit);
            } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
                throw new CdpClient.CdpException("Failed to parse AX nodes: " + e.getMessage());
            }
        });
    }

    /**
     * Format raw AX nodes into a flat AriaSnapshotNode list.
     * Mirrors TS formatAriaSnapshot().
     */
    static List<CdpTypes.AriaSnapshotNode> formatAriaSnapshot(List<CdpTypes.RawAXNode> nodes,
                                                               int limit) {
        Map<String, CdpTypes.RawAXNode> byId = new LinkedHashMap<>();
        for (CdpTypes.RawAXNode n : nodes) {
            if (n.getNodeId() != null) {
                byId.put(n.getNodeId(), n);
            }
        }

        // Find root — node not referenced as a child
        Set<String> referenced = new HashSet<>();
        for (CdpTypes.RawAXNode n : nodes) {
            if (n.getChildIds() != null) {
                referenced.addAll(n.getChildIds());
            }
        }
        CdpTypes.RawAXNode root = null;
        for (CdpTypes.RawAXNode n : nodes) {
            if (n.getNodeId() != null && !referenced.contains(n.getNodeId())) {
                root = n;
                break;
            }
        }
        if (root == null && !nodes.isEmpty()) {
            root = nodes.get(0);
        }
        if (root == null || root.getNodeId() == null) {
            return List.of();
        }

        List<CdpTypes.AriaSnapshotNode> out = new ArrayList<>();
        Deque<Map.Entry<String, Integer>> stack = new ArrayDeque<>();
        stack.push(Map.entry(root.getNodeId(), 0));

        while (!stack.isEmpty() && out.size() < limit) {
            Map.Entry<String, Integer> entry = stack.pop();
            String id = entry.getKey();
            int depth = entry.getValue();
            CdpTypes.RawAXNode n = byId.get(id);
            if (n == null) continue;

            String role = n.getRole() != null ? n.getRole().getStringValue() : "";
            String name = n.getName() != null ? n.getName().getStringValue() : "";
            String value = n.getValue() != null ? n.getValue().getStringValue() : null;
            String desc = n.getDescription() != null ? n.getDescription().getStringValue() : null;
            String ref = "ax" + (out.size() + 1);

            CdpTypes.AriaSnapshotNode.AriaSnapshotNodeBuilder builder = CdpTypes.AriaSnapshotNode.builder()
                    .ref(ref)
                    .role(role.isEmpty() ? "unknown" : role)
                    .name(name)
                    .depth(depth);

            if (value != null && !value.isEmpty()) builder.value(value);
            if (desc != null && !desc.isEmpty()) builder.description(desc);
            if (n.getBackendDOMNodeId() != null) builder.backendDOMNodeId(n.getBackendDOMNodeId());

            out.add(builder.build());

            // Push children in reverse order (so first child is processed first)
            List<String> children = n.getChildIds();
            if (children != null) {
                List<String> validChildren = children.stream()
                        .filter(byId::containsKey)
                        .toList();
                for (int i = validChildren.size() - 1; i >= 0; i--) {
                    stack.push(Map.entry(validChildren.get(i), depth + 1));
                }
            }
        }

        return out;
    }

    // ==================== DOM Snapshot ====================

    /**
     * Get DOM snapshot via JavaScript evaluation.
     */
    public static List<CdpTypes.DomSnapshotNode> snapshotDom(String wsUrl, int limit,
                                                              int maxTextChars)
            throws CdpClient.CdpException {
        int effectiveLimit = Math.max(1, Math.min(5000, limit > 0 ? limit : 800));
        int effectiveMaxText = Math.max(0, Math.min(5000, maxTextChars > 0 ? maxTextChars : 220));

        String expression = buildDomSnapshotExpression(effectiveLimit, effectiveMaxText);

        CdpTypes.CdpEvalResult result = evaluateJavaScript(wsUrl, expression, true, true);
        if (result.getResult() == null || result.getResult().getValue() == null) {
            return List.of();
        }

        Object value = result.getResult().getValue();
        try {
            JsonNode valueNode = mapper.valueToTree(value);
            if (valueNode.has("nodes") && valueNode.get("nodes").isArray()) {
                List<CdpTypes.DomSnapshotNode> nodes = new ArrayList<>();
                for (JsonNode node : valueNode.get("nodes")) {
                    nodes.add(mapper.treeToValue(node, CdpTypes.DomSnapshotNode.class));
                }
                return nodes;
            }
        } catch (Exception e) {
            log.debug("Failed to parse DOM snapshot: {}", e.getMessage());
        }
        return List.of();
    }

    // ==================== DOM Text ====================

    /**
     * Get DOM text content (html or text format).
     */
    public static String getDomText(String wsUrl, String format, String selector, int maxChars)
            throws CdpClient.CdpException {
        int effectiveMaxChars = Math.max(0, Math.min(5_000_000, maxChars > 0 ? maxChars : 200_000));
        String selectorExpr = selector != null
                ? "\"" + selector.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
                : "null";
        String formatExpr = "\"" + (format != null ? format : "text") + "\"";

        String expression = "(() => {"
                + "const fmt = " + formatExpr + ";"
                + "const max = " + effectiveMaxChars + ";"
                + "const sel = " + selectorExpr + ";"
                + "const pick = sel ? document.querySelector(sel) : null;"
                + "let out = '';"
                + "if (fmt === 'text') {"
                + "  const el = pick || document.body || document.documentElement;"
                + "  try { out = String(el && el.innerText ? el.innerText : ''); } catch { out = ''; }"
                + "} else {"
                + "  const el = pick || document.documentElement;"
                + "  try { out = String(el && el.outerHTML ? el.outerHTML : ''); } catch { out = ''; }"
                + "}"
                + "if (max && out.length > max) out = out.slice(0, max) + '\\n<!-- …truncated… -->';"
                + "return out;"
                + "})()";

        CdpTypes.CdpEvalResult result = evaluateJavaScript(wsUrl, expression, true, true);
        if (result.getResult() != null && result.getResult().getValue() instanceof String s) {
            return s;
        }
        if (result.getResult() != null && result.getResult().getValue() != null) {
            return String.valueOf(result.getResult().getValue());
        }
        return "";
    }

    // ==================== querySelector ====================

    /**
     * Query DOM elements by CSS selector.
     */
    public static List<CdpTypes.QueryMatch> querySelector(String wsUrl, String selector,
                                                           int limit, int maxTextChars,
                                                           int maxHtmlChars)
            throws CdpClient.CdpException {
        int effectiveLimit = Math.max(1, Math.min(200, limit > 0 ? limit : 20));
        int effectiveMaxText = Math.max(0, Math.min(5000, maxTextChars > 0 ? maxTextChars : 500));
        int effectiveMaxHtml = Math.max(0, Math.min(20000, maxHtmlChars > 0 ? maxHtmlChars : 1500));

        String selectorStr = selector.replace("\\", "\\\\").replace("\"", "\\\"");
        String expression = "(() => {"
                + "const sel = \"" + selectorStr + "\";"
                + "const lim = " + effectiveLimit + ";"
                + "const maxText = " + effectiveMaxText + ";"
                + "const maxHtml = " + effectiveMaxHtml + ";"
                + "const els = Array.from(document.querySelectorAll(sel)).slice(0, lim);"
                + "return els.map((el, i) => {"
                + "  const tag = (el.tagName || '').toLowerCase();"
                + "  const id = el.id ? String(el.id) : undefined;"
                + "  const className = el.className ? String(el.className).slice(0, 300) : undefined;"
                + "  let text = '';"
                + "  try { text = String(el.innerText || '').trim(); } catch {}"
                + "  if (maxText && text.length > maxText) text = text.slice(0, maxText) + '…';"
                + "  const value = (el.value !== undefined && el.value !== null) ? String(el.value).slice(0, 500) : undefined;"
                + "  const href = (el.href !== undefined && el.href !== null) ? String(el.href) : undefined;"
                + "  let outerHTML = '';"
                + "  try { outerHTML = String(el.outerHTML || ''); } catch {}"
                + "  if (maxHtml && outerHTML.length > maxHtml) outerHTML = outerHTML.slice(0, maxHtml) + '…';"
                + "  return {"
                + "    index: i + 1, tag,"
                + "    ...(id ? { id } : {}),"
                + "    ...(className ? { className } : {}),"
                + "    ...(text ? { text } : {}),"
                + "    ...(value ? { value } : {}),"
                + "    ...(href ? { href } : {}),"
                + "    ...(outerHTML ? { outerHTML } : {})"
                + "  };"
                + "});"
                + "})()";

        CdpTypes.CdpEvalResult result = evaluateJavaScript(wsUrl, expression, true, true);
        if (result.getResult() != null && result.getResult().getValue() != null) {
            try {
                JsonNode valueNode = mapper.valueToTree(result.getResult().getValue());
                if (valueNode.isArray()) {
                    List<CdpTypes.QueryMatch> matches = new ArrayList<>();
                    for (JsonNode node : valueNode) {
                        matches.add(mapper.treeToValue(node, CdpTypes.QueryMatch.class));
                    }
                    return matches;
                }
            } catch (Exception e) {
                log.debug("Failed to parse query matches: {}", e.getMessage());
            }
        }
        return List.of();
    }

    // ==================== Target Creation ====================

    /**
     * Create a new tab via CDP protocol.
     */
    public static String createTarget(String cdpUrl, String url) throws CdpClient.CdpException {
        try {
            String versionUrl = CdpHelpers.appendCdpPath(cdpUrl, "/json/version");
            CdpTypes.ChromeVersion version = CdpHelpers.fetchJson(versionUrl, 1500,
                    CdpTypes.ChromeVersion.class);
            String wsUrlRaw = version != null ? version.getWebSocketDebuggerUrl() : null;
            if (wsUrlRaw == null || wsUrlRaw.isBlank()) {
                throw new CdpClient.CdpException("CDP /json/version missing webSocketDebuggerUrl");
            }
            String wsUrl = CdpHelpers.normalizeCdpWsUrl(wsUrlRaw.trim(), cdpUrl);

            return CdpClient.withCdpSocket(wsUrl, send -> {
                ObjectNode params = mapper.createObjectNode();
                params.put("url", url);
                JsonNode result = send.send("Target.createTarget", params);
                String targetId = result.has("targetId")
                        ? result.get("targetId").asText("").trim() : "";
                if (targetId.isEmpty()) {
                    throw new CdpClient.CdpException(
                            "CDP Target.createTarget returned no targetId");
                }
                return targetId;
            });
        } catch (CdpClient.CdpException e) {
            throw e;
        } catch (Exception e) {
            throw new CdpClient.CdpException("Failed to create target: " + e.getMessage(), e);
        }
    }

    // ==================== Private helpers ====================

    private static String buildDomSnapshotExpression(int limit, int maxTextChars) {
        return "(() => {"
                + "const maxNodes = " + limit + ";"
                + "const maxText = " + maxTextChars + ";"
                + "const nodes = [];"
                + "const root = document.documentElement;"
                + "if (!root) return { nodes };"
                + "const stack = [{ el: root, depth: 0, parentRef: null }];"
                + "while (stack.length && nodes.length < maxNodes) {"
                + "  const cur = stack.pop();"
                + "  const el = cur.el;"
                + "  if (!el || el.nodeType !== 1) continue;"
                + "  const ref = 'n' + String(nodes.length + 1);"
                + "  const tag = (el.tagName || '').toLowerCase();"
                + "  const id = el.id ? String(el.id) : undefined;"
                + "  const className = el.className ? String(el.className).slice(0, 300) : undefined;"
                + "  const role = el.getAttribute && el.getAttribute('role') ? String(el.getAttribute('role')) : undefined;"
                + "  const name = el.getAttribute && el.getAttribute('aria-label') ? String(el.getAttribute('aria-label')) : undefined;"
                + "  let text = '';"
                + "  try { text = String(el.innerText || '').trim(); } catch {}"
                + "  if (maxText && text.length > maxText) text = text.slice(0, maxText) + '…';"
                + "  const href = (el.href !== undefined && el.href !== null) ? String(el.href) : undefined;"
                + "  const type = (el.type !== undefined && el.type !== null) ? String(el.type) : undefined;"
                + "  const value = (el.value !== undefined && el.value !== null) ? String(el.value).slice(0, 500) : undefined;"
                + "  nodes.push({ ref, parentRef: cur.parentRef, depth: cur.depth, tag,"
                + "    ...(id ? { id } : {}), ...(className ? { className } : {}),"
                + "    ...(role ? { role } : {}), ...(name ? { name } : {}),"
                + "    ...(text ? { text } : {}), ...(href ? { href } : {}),"
                + "    ...(type ? { type } : {}), ...(value ? { value } : {}) });"
                + "  const children = el.children ? Array.from(el.children) : [];"
                + "  for (let i = children.length - 1; i >= 0; i--) {"
                + "    stack.push({ el: children[i], depth: cur.depth + 1, parentRef: ref });"
                + "  }"
                + "}"
                + "return { nodes };"
                + "})()";
    }
}

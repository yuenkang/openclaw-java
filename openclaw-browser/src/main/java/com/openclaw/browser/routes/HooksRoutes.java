package com.openclaw.browser.routes;

import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;

import java.util.List;
import java.util.Map;

/**
 * Hooks, tracing, debugging, and misc routes:
 * /hooks/dialog, /hooks/file-chooser, /trace/*, /highlight, /pdf, /errors, /requests.
 */
public final class HooksRoutes {

    private HooksRoutes() {
    }

    /** POST /hooks/dialog */
    public static Object handleHooksDialog(RouteContext ctx, FullHttpRequest request,
                                            String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        boolean accept = reqBody != null && Boolean.TRUE.equals(reqBody.get("accept"));
        String promptText = reqBody != null ? (String) reqBody.get("promptText") : null;
        var page = session.resolveTargetPage(targetId);
        if (accept) {
            com.openclaw.browser.playwright.PwToolsCore.acceptDialog(page, promptText);
        } else {
            com.openclaw.browser.playwright.PwToolsCore.dismissDialog(page);
        }
        return Map.of("ok", true);
    }

    /** POST /hooks/file-chooser */
    @SuppressWarnings("unchecked")
    public static Object handleHooksFileChooser(RouteContext ctx, FullHttpRequest request,
                                                 String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        List<String> paths = reqBody != null ? (List<String>) reqBody.get("paths") : null;
        if (paths == null || paths.isEmpty())
            throw new RouteContext.BadRequestException("paths is required");
        var page = session.resolveTargetPage(targetId);
        List<java.nio.file.Path> filePaths = paths.stream()
                .map(java.nio.file.Path::of)
                .toList();
        String ref = reqBody != null ? (String) reqBody.get("ref") : null;
        String selector = ref != null ? ref : "input[type=file]";
        com.openclaw.browser.playwright.PwToolsCore.setInputFiles(page, selector, filePaths);
        return Map.of("ok", true);
    }

    /** POST /trace/start */
    public static Object handleTraceStart(RouteContext ctx, FullHttpRequest request,
                                           String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        boolean screenshots = reqBody != null && Boolean.TRUE.equals(reqBody.get("screenshots"));
        boolean snapshots = reqBody != null && Boolean.TRUE.equals(reqBody.get("snapshots"));
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore
                .startTracing(page.context(), screenshots, snapshots);
        return Map.of("ok", true);
    }

    /** POST /trace/stop */
    public static Object handleTraceStop(RouteContext ctx, FullHttpRequest request,
                                          String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        String path = reqBody != null ? (String) reqBody.get("path") : null;
        var page = session.resolveTargetPage(targetId);
        java.nio.file.Path outputPath;
        if (path != null && !path.isBlank()) {
            outputPath = java.nio.file.Path.of(path);
        } else {
            try {
                outputPath = java.nio.file.Files.createTempFile("browser-trace-", ".zip");
            } catch (Exception e) {
                throw new RuntimeException("Failed to create trace file: " + e.getMessage());
            }
        }
        com.openclaw.browser.playwright.PwToolsCore.stopTracing(page.context(), outputPath);
        return Map.of("ok", true, "path", outputPath.toAbsolutePath().toString());
    }

    /** POST /highlight */
    public static Object handleHighlight(RouteContext ctx, FullHttpRequest request,
                                          String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        String ref = reqBody != null ? (String) reqBody.get("ref") : null;
        if (ref == null) throw new RouteContext.BadRequestException("ref is required");
        var page = session.resolveTargetPage(targetId);
        String selector = ref.startsWith("e") ? "[aria-ref='" + ref + "']" : ref;
        com.openclaw.browser.playwright.PwToolsCore.highlight(page, selector);
        return Map.of("ok", true);
    }

    /** POST /pdf */
    public static Object handlePdf(RouteContext ctx, FullHttpRequest request,
                                    String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        var page = session.resolveTargetPage(targetId);
        byte[] pdf = com.openclaw.browser.playwright.PwToolsCore.generatePdf(page);
        try {
            java.nio.file.Path tempFile = java.nio.file.Files.createTempFile("browser-pdf-", ".pdf");
            java.nio.file.Files.write(tempFile, pdf);
            return Map.of("ok", true, "path", tempFile.toAbsolutePath().toString());
        } catch (Exception e) {
            throw new RuntimeException("Failed to save PDF: " + e.getMessage(), e);
        }
    }

    /** GET /errors */
    public static Object handleErrors(RouteContext ctx, QueryStringDecoder decoder,
                                       String profile) {
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = RouteContext.firstParam(decoder, "targetId");
        boolean clear = "true".equals(RouteContext.firstParam(decoder, "clear"));
        var errors = session.getPageErrors(targetId, clear);
        List<Map<String, Object>> mapped = errors.stream().map(e -> {
            Map<String, Object> m = new java.util.LinkedHashMap<>();
            m.put("message", e.getMessage());
            m.put("url", e.getUrl());
            m.put("timestamp", e.getTimestamp());
            return m;
        }).toList();
        return Map.of("ok", true, "errors", mapped);
    }

    /** GET /requests */
    public static Object handleRequests(RouteContext ctx, QueryStringDecoder decoder,
                                         String profile) {
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = RouteContext.firstParam(decoder, "targetId");
        String filter = RouteContext.firstParam(decoder, "filter");
        boolean clear = "true".equals(RouteContext.firstParam(decoder, "clear"));
        var requests = session.getNetworkRequests(targetId, filter, clear);
        List<Map<String, Object>> mapped = requests.stream().map(r -> {
            Map<String, Object> m = new java.util.LinkedHashMap<>();
            m.put("method", r.getMethod());
            m.put("url", r.getUrl());
            m.put("status", r.getStatus());
            m.put("resourceType", r.getResourceType());
            m.put("timestamp", r.getTimestamp());
            return m;
        }).toList();
        return Map.of("ok", true, "requests", mapped);
    }

    /** POST /hooks/arm-upload */
    @SuppressWarnings("unchecked")
    public static Object handleArmUpload(RouteContext ctx, FullHttpRequest request,
                                          String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        List<String> paths = reqBody != null ? (List<String>) reqBody.get("paths") : null;
        int timeoutMs = reqBody != null && reqBody.get("timeoutMs") != null
                ? ((Number) reqBody.get("timeoutMs")).intValue() : 0;
        session.armFileUpload(targetId, paths, timeoutMs);
        return Map.of("ok", true);
    }

    /** POST /hooks/arm-dialog */
    public static Object handleArmDialog(RouteContext ctx, FullHttpRequest request,
                                          String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        boolean accept = reqBody != null && Boolean.TRUE.equals(reqBody.get("accept"));
        String promptText = reqBody != null ? (String) reqBody.get("promptText") : null;
        int timeoutMs = reqBody != null && reqBody.get("timeoutMs") != null
                ? ((Number) reqBody.get("timeoutMs")).intValue() : 0;
        session.armDialog(targetId, accept, promptText, timeoutMs);
        return Map.of("ok", true);
    }

    /** GET /console */
    public static Object handleConsole(RouteContext ctx, QueryStringDecoder decoder,
                                        String profile) {
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = RouteContext.firstParam(decoder, "targetId");
        String level = RouteContext.firstParam(decoder, "level");
        var messages = session.getConsoleMessages(targetId, level);
        return Map.of("ok", true, "messages", messages);
    }

    /** POST /response/body — capture response body matching URL pattern */
    public static Object handleResponseBody(RouteContext ctx, FullHttpRequest request,
                                             String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        String url = reqBody != null ? (String) reqBody.get("url") : null;
        if (url == null || url.isBlank()) {
            throw new IllegalArgumentException("url is required");
        }
        int timeoutMs = reqBody != null && reqBody.get("timeoutMs") != null
                ? ((Number) reqBody.get("timeoutMs")).intValue() : 20000;
        int maxChars = reqBody != null && reqBody.get("maxChars") != null
                ? ((Number) reqBody.get("maxChars")).intValue() : 200_000;
        var page = session.resolveTargetPage(targetId);
        String resolvedTargetId = session.pageTargetId(page);
        var result = session.responseBody(resolvedTargetId, url, timeoutMs, maxChars);
        var response = new java.util.LinkedHashMap<String, Object>();
        response.put("ok", true);
        response.put("targetId", resolvedTargetId != null ? resolvedTargetId : "");
        response.put("response", result);
        return response;
    }
}

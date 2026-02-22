package com.openclaw.browser.routes;

import com.openclaw.browser.PlaywrightSession;
import com.openclaw.browser.server.DualChannelBridge;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;

import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Snapshot and screenshot routes: GET /snapshot, POST /screenshot.
 */
public final class SnapshotRoutes {

    private SnapshotRoutes() {
    }

    /** GET /snapshot */
    public static Object handleSnapshot(RouteContext ctx, QueryStringDecoder decoder,
                                         String profile) {
        String targetId = RouteContext.firstParam(decoder, "targetId");
        String refsMode = RouteContext.firstParam(decoder, "refs");
        boolean compact = "true".equals(RouteContext.firstParam(decoder, "compact"));
        boolean interactive = "true".equals(RouteContext.firstParam(decoder, "interactive"));
        String maxDepthStr = RouteContext.firstParam(decoder, "maxDepth");
        int maxDepth = maxDepthStr != null ? Integer.parseInt(maxDepthStr) : -1;
        String maxCharsStr = RouteContext.firstParam(decoder, "maxChars");
        int maxChars = maxCharsStr != null ? Integer.parseInt(maxCharsStr) : 0;

        PlaywrightSession session = ctx.ensureBrowserStarted(profile);
        String resolvedName = ctx.profileService().resolveProfileName(profile);
        DualChannelBridge bridge = ctx.bridges().get(resolvedName);

        PlaywrightSession.SnapshotResult snap;
        String channel;
        if (bridge != null && bridge.isCdpAvailable()) {
            snap = bridge.snapshotDual(targetId);
            channel = "cdp";
        } else {
            snap = session.snapshot(targetId);
            channel = "playwright";
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("url", snap.getUrl());
        result.put("title", snap.getTitle());
        result.put("channel", channel);

        if ("role".equals(refsMode)) {
            var options = com.openclaw.browser.snapshot.RoleSnapshot.RoleSnapshotOptions.builder()
                    .interactive(interactive)
                    .compact(compact)
                    .maxDepth(maxDepth)
                    .build();
            var roleResult = com.openclaw.browser.snapshot.RoleSnapshot
                    .buildFromAriaSnapshot(snap.getSnapshot(), options);
            var stats = com.openclaw.browser.snapshot.RoleSnapshot
                    .getStats(roleResult.getSnapshot(), roleResult.getRefs());

            // Store refs in PageState for resolveRef to use
            var page = session.resolveTargetPage(targetId);
            @SuppressWarnings("unchecked")
            Map<String, Object> refsAsMap = (Map<String, Object>) (Map<String, ?>) roleResult.getRefs();
            com.openclaw.browser.playwright.PageState.storeRoleRefs(
                    page, refsAsMap, "role", null);

            String snapshotText = roleResult.getSnapshot();
            if (maxChars > 0) {
                var tr = com.openclaw.browser.snapshot.AiSnapshotFormatter.truncate(snapshotText, maxChars);
                snapshotText = tr.snapshot();
                result.put("truncated", tr.truncated());
            }
            result.put("format", "role");
            result.put("snapshot", snapshotText);
            result.put("refs", roleResult.getRefs());
            result.put("lines", stats.getLines());
            result.put("chars", stats.getChars());
            result.put("refsCount", stats.getRefs());
            result.put("interactive", stats.getInteractive());
        } else {
            String ariaText = snap.getSnapshot();
            if (maxChars > 0) {
                var tr = com.openclaw.browser.snapshot.AiSnapshotFormatter.truncate(ariaText, maxChars);
                ariaText = tr.snapshot();
                result.put("truncated", tr.truncated());
            }
            result.put("format", "aria");
            result.put("snapshot", ariaText);
        }

        return result;
    }

    /** POST /screenshot */
    public static Object handleScreenshot(RouteContext ctx, FullHttpRequest request,
                                           String profile) {
        Map<String, Object> reqBody = ctx.readBody(request);
        PlaywrightSession session = ctx.ensureBrowserStarted(profile);
        String resolvedName = ctx.profileService().resolveProfileName(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        boolean fullPage = reqBody != null && Boolean.TRUE.equals(reqBody.get("fullPage"));
        String ref = reqBody != null ? (String) reqBody.get("ref") : null;
        String element = reqBody != null ? (String) reqBody.get("element") : null;

        PlaywrightSession.ScreenshotResult shot;
        String channel;
        DualChannelBridge bridge = ctx.bridges().get(resolvedName);
        if (bridge != null) {
            shot = bridge.screenshotDual(targetId, fullPage, ref, element);
            channel = bridge.isCdpAvailable() && ref == null && element == null && !fullPage
                    ? "cdp" : "playwright";
        } else {
            shot = session.screenshot(targetId, fullPage);
            channel = "playwright";
        }

        String base64 = Base64.getEncoder().encodeToString(shot.getBuffer());
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("data", base64);
        result.put("contentType", shot.getContentType());
        result.put("url", shot.getUrl());
        result.put("title", shot.getTitle());
        result.put("channel", channel);
        return result;
    }

    /** POST /screenshot-labels */
    @SuppressWarnings("unchecked")
    public static Object handleScreenshotWithLabels(RouteContext ctx, FullHttpRequest request,
                                                      String profile) {
        Map<String, Object> reqBody = ctx.readBody(request);
        PlaywrightSession session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        int maxLabels = reqBody != null && reqBody.get("maxLabels") != null
                ? ((Number) reqBody.get("maxLabels")).intValue() : 50;

        // Get refs - either from request body or from stored PageState
        Map<String, com.openclaw.browser.snapshot.RoleSnapshot.RoleRef> refs = null;
        if (reqBody != null && reqBody.get("refs") != null) {
            refs = (Map<String, com.openclaw.browser.snapshot.RoleSnapshot.RoleRef>) reqBody.get("refs");
        } else {
            // Use stored refs from last snapshot
            var page = session.resolveTargetPage(targetId);
            var stored = com.openclaw.browser.playwright.PageState.getStoredRoleRefs(page);
            if (stored != null) {
                refs = (Map<String, com.openclaw.browser.snapshot.RoleSnapshot.RoleRef>)
                        (Map<String, ?>) stored;
            }
        }

        if (refs == null || refs.isEmpty()) {
            // Fall back to regular screenshot
            var shot = session.screenshot(targetId, false);
            String base64 = Base64.getEncoder().encodeToString(shot.getBuffer());
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("data", base64);
            result.put("contentType", "image/png");
            result.put("labels", 0);
            result.put("skipped", 0);
            return result;
        }

        var labelResult = session.screenshotWithLabels(targetId, refs, maxLabels);
        String base64 = Base64.getEncoder().encodeToString(labelResult.getBuffer());
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("data", base64);
        result.put("contentType", "image/png");
        result.put("labels", labelResult.getLabels());
        result.put("skipped", labelResult.getSkipped());
        result.put("url", labelResult.getUrl());
        result.put("title", labelResult.getTitle());
        return result;
    }
}

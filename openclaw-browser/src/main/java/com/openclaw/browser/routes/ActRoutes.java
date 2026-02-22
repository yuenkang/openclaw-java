package com.openclaw.browser.routes;

import com.openclaw.browser.PlaywrightSession;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Action routes: POST /act, POST /navigate, GET /console.
 */
public final class ActRoutes {

    private ActRoutes() {
    }

    /** POST /act */
    public static Object handleAct(RouteContext ctx, FullHttpRequest request, String profile) {
        Map<String, Object> reqBody = ctx.readBody(request);
        String kind = reqBody != null ? (String) reqBody.get("kind") : null;
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        if (kind == null || kind.isBlank())
            throw new RouteContext.BadRequestException("kind is required");
        PlaywrightSession session = ctx.ensureBrowserStarted(profile);
        return session.act(kind, targetId, reqBody);
    }

    /** POST /navigate */
    public static Object handleNavigate(RouteContext ctx, FullHttpRequest request,
                                         String profile) {
        Map<String, Object> reqBody = ctx.readBody(request);
        String url = reqBody != null ? (String) reqBody.get("url") : null;
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        if (url == null || url.isBlank())
            throw new RouteContext.BadRequestException("url is required");
        PlaywrightSession session = ctx.ensureBrowserStarted(profile);
        PlaywrightSession.NavigateResult nav = session.navigate(url, targetId);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("url", nav.getUrl());
        result.put("title", nav.getTitle());
        return result;
    }

    /** GET /console */
    public static Object handleConsole(RouteContext ctx, QueryStringDecoder decoder,
                                        String profile) {
        String targetId = RouteContext.firstParam(decoder, "targetId");
        if (!ctx.profileService().isRunning(profile)) {
            return Map.of("ok", true, "messages", List.of());
        }
        PlaywrightSession session = ctx.profileService().getOrCreateSession(profile);
        List<PlaywrightSession.ConsoleEntry> messages = session.getConsoleMessages(targetId);
        List<Map<String, Object>> mapped = messages.stream().map(e -> {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("type", e.getType());
            m.put("text", e.getText());
            m.put("timestamp", e.getTimestamp());
            return m;
        }).toList();
        return Map.of("ok", true, "messages", mapped);
    }
}

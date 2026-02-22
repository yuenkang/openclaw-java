package com.openclaw.browser.routes;

import com.openclaw.browser.PlaywrightSession;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.HttpMethod;

import java.util.List;
import java.util.Map;

/**
 * Tab management routes: GET /tabs, POST /tabs/open, POST /tabs/focus, DELETE /tabs/{id}.
 */
public final class TabRoutes {

    private TabRoutes() {
    }

    public static Object handle(RouteContext ctx, HttpMethod method, String path,
                                 FullHttpRequest request, String profile) {
        // POST /tabs/open
        if (method == HttpMethod.POST && "/tabs/open".equals(path)) {
            Map<String, Object> reqBody = ctx.readBody(request);
            String url = reqBody != null ? (String) reqBody.get("url") : null;
            if (url == null || url.isBlank())
                throw new RouteContext.BadRequestException("url is required");
            PlaywrightSession session = ctx.ensureBrowserStarted(profile);
            PlaywrightSession.TabInfo tab = session.openTab(url);
            return ctx.tabToMap(tab);
        }

        // POST /tabs/focus
        if (method == HttpMethod.POST && "/tabs/focus".equals(path)) {
            Map<String, Object> reqBody = ctx.readBody(request);
            String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
            if (targetId == null || targetId.isBlank())
                throw new RouteContext.BadRequestException("targetId is required");
            PlaywrightSession session = ctx.profileService().getOrCreateSession(profile);
            session.focusTab(targetId);
            return Map.of("ok", true);
        }

        // DELETE /tabs/{targetId}
        if (method == HttpMethod.DELETE && path.startsWith("/tabs/")) {
            String targetId = path.substring("/tabs/".length());
            PlaywrightSession session = ctx.profileService().getOrCreateSession(profile);
            session.closeTab(targetId);
            return Map.of("ok", true);
        }

        // GET /tabs
        if (method != HttpMethod.GET)
            throw new RouteContext.MethodNotAllowedException();
        if (!ctx.profileService().isRunning(profile)) {
            return Map.of("running", false, "tabs", List.of());
        }
        PlaywrightSession session = ctx.profileService().getOrCreateSession(profile);
        List<PlaywrightSession.TabInfo> tabs = session.listTabs();
        return Map.of("running", true, "tabs",
                tabs.stream().map(ctx::tabToMap).toList());
    }
}

package com.openclaw.browser.routes;

import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;

import java.util.List;
import java.util.Map;

/**
 * Cookie and storage routes: /cookies, /storage.
 */
public final class StorageRoutes {

    private StorageRoutes() {
    }

    /** GET /cookies */
    public static Object handleCookies(RouteContext ctx, QueryStringDecoder decoder,
                                        String profile) {
        String targetId = RouteContext.firstParam(decoder, "targetId");
        var session = ctx.ensureBrowserStarted(profile);
        var page = session.resolveTargetPage(targetId);
        var cookies = com.openclaw.browser.playwright.PwToolsCore
                .getCookies(page.context());
        return Map.of("ok", true, "targetId", page.mainFrame().name(),
                "cookies", cookies);
    }

    /** POST /cookies */
    @SuppressWarnings("unchecked")
    public static Object handleCookiesSet(RouteContext ctx, FullHttpRequest request,
                                           String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        Map<String, Object> cookie = reqBody != null
                ? (Map<String, Object>) reqBody.get("cookie") : null;
        if (cookie == null) throw new RouteContext.BadRequestException("cookie is required");
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore.setCookie(page.context(), cookie);
        return Map.of("ok", true);
    }

    /** DELETE /cookies */
    public static Object handleCookiesClear(RouteContext ctx, String profile) {
        var session = ctx.ensureBrowserStarted(profile);
        com.openclaw.browser.playwright.PwToolsCore
                .clearCookies(session.resolveTargetPage(null).context());
        return Map.of("ok", true);
    }

    /** GET /storage */
    public static Object handleStorageGet(RouteContext ctx, QueryStringDecoder decoder,
                                           String profile) {
        String targetId = RouteContext.firstParam(decoder, "targetId");
        String kind = RouteContext.firstParam(decoder, "kind");
        String key = RouteContext.firstParam(decoder, "key");
        var session = ctx.ensureBrowserStarted(profile);
        var page = session.resolveTargetPage(targetId);
        var values = com.openclaw.browser.playwright.PwToolsCore
                .getStorage(page, kind != null ? kind : "local", key);
        return Map.of("ok", true, "values", values);
    }

    /** POST /storage */
    public static Object handleStorageSet(RouteContext ctx, FullHttpRequest request,
                                           String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        String kind = reqBody != null ? (String) reqBody.get("kind") : "local";
        String key = reqBody != null ? (String) reqBody.get("key") : null;
        String value = reqBody != null ? (String) reqBody.get("value") : null;
        if (key == null) throw new RouteContext.BadRequestException("key is required");
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore.setStorage(page, kind, key,
                value != null ? value : "");
        return Map.of("ok", true);
    }

    /** DELETE /storage */
    public static Object handleStorageClear(RouteContext ctx, QueryStringDecoder decoder,
                                             String profile) {
        String targetId = RouteContext.firstParam(decoder, "targetId");
        String kind = RouteContext.firstParam(decoder, "kind");
        var session = ctx.ensureBrowserStarted(profile);
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore
                .clearStorage(page, kind != null ? kind : "local");
        return Map.of("ok", true);
    }
}

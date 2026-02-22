package com.openclaw.browser.routes;

import io.netty.handler.codec.http.FullHttpRequest;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * State management routes: /state/offline, /state/headers, /state/credentials,
 * /state/geolocation, /state/media, /resize.
 */
public final class StateRoutes {

    private StateRoutes() {
    }

    /** POST /state/offline */
    public static Object handleSetOffline(RouteContext ctx, FullHttpRequest request,
                                           String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        boolean offline = reqBody != null && Boolean.TRUE.equals(reqBody.get("offline"));
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore.setOffline(page.context(), offline);
        return Map.of("ok", true);
    }

    /** POST /state/headers */
    @SuppressWarnings("unchecked")
    public static Object handleSetHeaders(RouteContext ctx, FullHttpRequest request,
                                           String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        Map<String, String> headers = reqBody != null
                ? (Map<String, String>) reqBody.get("headers") : Map.of();
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore
                .setExtraHTTPHeaders(page.context(), headers);
        return Map.of("ok", true);
    }

    /** POST /state/credentials */
    public static Object handleSetCredentials(RouteContext ctx, FullHttpRequest request,
                                               String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        String username = reqBody != null ? (String) reqBody.get("username") : null;
        String password = reqBody != null ? (String) reqBody.get("password") : null;
        boolean clear = reqBody != null && Boolean.TRUE.equals(reqBody.get("clear"));
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore
                .setHttpCredentials(page.context(), username, password, clear);
        return Map.of("ok", true);
    }

    /** POST /state/geolocation */
    public static Object handleSetGeolocation(RouteContext ctx, FullHttpRequest request,
                                               String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        boolean clear = reqBody != null && Boolean.TRUE.equals(reqBody.get("clear"));
        Double lat = reqBody != null && reqBody.get("latitude") != null
                ? ((Number) reqBody.get("latitude")).doubleValue() : null;
        Double lon = reqBody != null && reqBody.get("longitude") != null
                ? ((Number) reqBody.get("longitude")).doubleValue() : null;
        Double accuracy = reqBody != null && reqBody.get("accuracy") != null
                ? ((Number) reqBody.get("accuracy")).doubleValue() : null;
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore
                .setGeolocation(page.context(), lat, lon, accuracy, clear);
        return Map.of("ok", true);
    }

    /** POST /state/media */
    public static Object handleSetMedia(RouteContext ctx, FullHttpRequest request,
                                         String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        String colorScheme = reqBody != null ? (String) reqBody.get("colorScheme") : null;
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore.emulateMedia(page, colorScheme);
        return Map.of("ok", true);
    }

    /** POST /resize */
    public static Object handleResize(RouteContext ctx, FullHttpRequest request,
                                       String profile) {
        var reqBody = ctx.readBody(request);
        var session = ctx.ensureBrowserStarted(profile);
        String targetId = reqBody != null ? (String) reqBody.get("targetId") : null;
        int width = reqBody != null && reqBody.get("width") != null
                ? ((Number) reqBody.get("width")).intValue() : 1280;
        int height = reqBody != null && reqBody.get("height") != null
                ? ((Number) reqBody.get("height")).intValue() : 720;
        var page = session.resolveTargetPage(targetId);
        com.openclaw.browser.playwright.PwToolsCore.resizeViewport(page, width, height);
        return Map.of("ok", true);
    }
}

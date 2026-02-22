package com.openclaw.browser.routes;

import com.openclaw.browser.BrowserProfileService;
import com.openclaw.browser.PlaywrightSession;
import com.openclaw.browser.server.DualChannelBridge;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.HttpMethod;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Basic lifecycle routes: GET /, POST /start, POST /stop, /profiles/**, POST /reset-profile, GET /channels.
 */
public final class BasicRoutes {

    private BasicRoutes() {
    }

    /** GET / */
    public static Object handleStatus(RouteContext ctx, String profile) {
        String resolved = ctx.profileService().resolveProfileName(profile);
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("enabled", true);
        body.put("profile", resolved);
        body.put("running", ctx.profileService().isRunning(profile));
        body.put("headless", ctx.isHeadless());
        return body;
    }

    /** POST /start */
    public static Object handleStart(RouteContext ctx, FullHttpRequest request, String profile) {
        Map<String, Object> reqBody = ctx.readBody(request);
        PlaywrightSession session = ctx.profileService().getOrCreateSession(profile);
        String resolvedName = ctx.profileService().resolveProfileName(profile);

        if (session.isRunning()) {
            ctx.ensureBridge(resolvedName, session);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("profile", resolvedName);
            result.put("message", "already running");
            result.put("channels", ctx.getBridgeInfo(resolvedName));
            return result;
        }

        String cdpUrl = reqBody != null ? (String) reqBody.get("cdpUrl") : null;
        if (reqBody != null && reqBody.containsKey("headless")) {
            ctx.setHeadless(Boolean.TRUE.equals(reqBody.get("headless")));
        }

        if (cdpUrl != null && !cdpUrl.isBlank()) {
            session.connectCDP(cdpUrl);
        } else {
            session.launch(ctx.isHeadless());
        }

        DualChannelBridge bridge = ctx.ensureBridge(resolvedName, session);
        if (cdpUrl != null && !cdpUrl.isBlank()) {
            bridge.discoverCdp(cdpUrl);
        } else {
            bridge.discoverCdp("http://127.0.0.1:9222");
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("profile", resolvedName);
        result.put("channels", ctx.getBridgeInfo(resolvedName));
        return result;
    }

    /** POST /stop */
    public static Object handleStop(RouteContext ctx, String profile) {
        boolean stopped = ctx.profileService().stopSession(profile);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("profile", ctx.profileService().resolveProfileName(profile));
        result.put("stopped", stopped);
        return result;
    }

    /** /profiles/** */
    public static Object handleProfiles(RouteContext ctx, HttpMethod method, String path,
                                         FullHttpRequest request) throws Exception {
        if (method == HttpMethod.POST && "/profiles/create".equals(path)) {
            Map<String, Object> reqBody = ctx.readBody(request);
            String name = reqBody != null ? (String) reqBody.get("name") : null;
            String color = reqBody != null ? (String) reqBody.get("color") : null;
            String cdpUrl = reqBody != null ? (String) reqBody.get("cdpUrl") : null;
            if (name == null || name.isBlank()) {
                throw new RouteContext.BadRequestException("name is required");
            }
            BrowserProfileService.CreateProfileResult r =
                    ctx.profileService().createProfile(name, color, cdpUrl);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", r.isOk());
            result.put("profile", r.getProfile());
            result.put("cdpPort", r.getCdpPort());
            result.put("cdpUrl", r.getCdpUrl());
            result.put("color", r.getColor());
            return result;
        }

        if (method == HttpMethod.DELETE && path.startsWith("/profiles/")) {
            String name = path.substring("/profiles/".length());
            if (name.isEmpty())
                throw new RouteContext.BadRequestException("profile name is required");
            BrowserProfileService.DeleteProfileResult r =
                    ctx.profileService().deleteProfile(name);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", r.isOk());
            result.put("profile", r.getProfile());
            result.put("stopped", r.isStopped());
            return result;
        }

        if (method != HttpMethod.GET)
            throw new RouteContext.MethodNotAllowedException();
        List<BrowserProfileService.ProfileStatus> profiles = ctx.profileService().listProfiles();
        List<Map<String, Object>> list = profiles.stream().map(p -> {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("name", p.getName());
            m.put("cdpPort", p.getCdpPort());
            m.put("cdpUrl", p.getCdpUrl());
            m.put("color", p.getColor());
            m.put("running", p.isRunning());
            m.put("isDefault", p.isDefault());
            return m;
        }).toList();
        return Map.of("profiles", list);
    }

    /** GET /channels */
    public static Object handleChannels(RouteContext ctx, String profile) {
        String resolvedName = ctx.profileService().resolveProfileName(profile);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("profile", resolvedName);
        var bridge = ctx.bridges().get(resolvedName);
        if (bridge != null) {
            result.put("channels", bridge.getChannelInfo());
        } else {
            result.put("channels", Map.of(
                    "playwright", ctx.profileService().isRunning(profile),
                    "cdpDirect", false));
        }
        return result;
    }

    /** POST /reset-profile */
    public static Object handleResetProfile(RouteContext ctx, String profile) {
        ctx.profileService().stopSession(profile);
        String resolved = ctx.profileService().resolveProfileName(profile);
        return Map.of("ok", true, "profile", resolved, "reset", true);
    }
}

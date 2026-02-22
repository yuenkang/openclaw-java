package com.openclaw.browser.routes;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.browser.BrowserProfileService;
import com.openclaw.browser.PlaywrightSession;
import com.openclaw.browser.server.DualChannelBridge;
import io.netty.buffer.ByteBuf;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.QueryStringDecoder;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Shared context passed to all route handlers.
 * Provides access to the profile service, bridges, headless state,
 * and common utility methods (readBody, firstParam, ensureBrowserStarted).
 */
public class RouteContext {

    private static final ObjectMapper mapper = new ObjectMapper();

    private final BrowserProfileService profileService;
    private final ConcurrentHashMap<String, DualChannelBridge> bridges;
    private boolean headless;

    public RouteContext(BrowserProfileService profileService,
                        ConcurrentHashMap<String, DualChannelBridge> bridges) {
        this.profileService = profileService;
        this.bridges = bridges;
    }

    public BrowserProfileService profileService() {
        return profileService;
    }

    public ConcurrentHashMap<String, DualChannelBridge> bridges() {
        return bridges;
    }

    public boolean isHeadless() {
        return headless;
    }

    public void setHeadless(boolean headless) {
        this.headless = headless;
    }

    // ===== Utility methods shared by all routes =====

    public PlaywrightSession ensureBrowserStarted(String profile) {
        PlaywrightSession session = profileService.getOrCreateSession(profile);
        if (!session.isRunning()) {
            session.launch(headless);
            String resolvedName = profileService.resolveProfileName(profile);
            DualChannelBridge bridge = ensureBridge(resolvedName, session);
            bridge.discoverCdp("http://127.0.0.1:9222");
        }
        return session;
    }

    public DualChannelBridge ensureBridge(String resolvedName, PlaywrightSession session) {
        return bridges.computeIfAbsent(resolvedName, k -> new DualChannelBridge(session));
    }

    public Map<String, Object> getBridgeInfo(String resolvedName) {
        DualChannelBridge bridge = bridges.get(resolvedName);
        if (bridge != null) {
            return bridge.getChannelInfo();
        }
        return Map.of("playwright", true, "cdpDirect", false);
    }

    public Map<String, Object> readBody(FullHttpRequest request) {
        try {
            ByteBuf content = request.content();
            if (content.readableBytes() == 0) return null;
            byte[] bytes = new byte[content.readableBytes()];
            content.readBytes(bytes);
            return mapper.readValue(bytes, new TypeReference<>() {});
        } catch (Exception e) {
            return null;
        }
    }

    public static String firstParam(QueryStringDecoder decoder, String key) {
        List<String> values = decoder.parameters().get(key);
        if (values == null || values.isEmpty()) return null;
        return values.get(0);
    }

    public Map<String, Object> tabToMap(PlaywrightSession.TabInfo tab) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("targetId", tab.getTargetId());
        m.put("url", tab.getUrl());
        m.put("title", tab.getTitle());
        return m;
    }

    // ===== Exception types =====

    public static class BadRequestException extends RuntimeException {
        public BadRequestException(String msg) {
            super(msg);
        }
    }

    public static class MethodNotAllowedException extends RuntimeException {
    }

    public static class NotFoundException extends RuntimeException {
    }
}

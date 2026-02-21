package com.openclaw.plugin.http;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.BiConsumer;

/**
 * Plugin HTTP route registry — allows plugins to register custom
 * HTTP endpoints (webhooks, callbacks, etc.).
 * Corresponds to TypeScript's plugins/http-registry.ts.
 * In Java/Spring context, these are exposed via PluginBootstrap.
 */
@Slf4j
public class PluginHttpRegistry {

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HttpRouteRegistration {
        private String path;
        private String pluginId;
        private String source;
        /** Handler signature: (request body, response writer). */
        private BiConsumer<Object, Object> handler;
    }

    private final List<HttpRouteRegistration> routes = new CopyOnWriteArrayList<>();

    /**
     * Register a plugin HTTP route.
     *
     * @return a Runnable that, when called, unregisters the route
     */
    public Runnable register(String path, String fallbackPath,
            String pluginId, String source,
            BiConsumer<Object, Object> handler) {
        String normalizedPath = PluginHttpPath.normalize(path, fallbackPath);
        if (normalizedPath == null) {
            log.warn("plugin: webhook path missing for {}", pluginId);
            return () -> {
            };
        }

        if (routes.stream().anyMatch(r -> normalizedPath.equals(r.getPath()))) {
            log.warn("plugin: webhook path {} already registered ({})", normalizedPath, pluginId);
            return () -> {
            };
        }

        HttpRouteRegistration entry = HttpRouteRegistration.builder()
                .path(normalizedPath)
                .pluginId(pluginId)
                .source(source)
                .handler(handler)
                .build();
        routes.add(entry);
        log.debug("Registered plugin HTTP route: {} (plugin: {})", normalizedPath, pluginId);

        return () -> routes.remove(entry);
    }

    public List<HttpRouteRegistration> getRoutes() {
        return List.copyOf(routes);
    }

    public Optional<HttpRouteRegistration> findRoute(String path) {
        return routes.stream()
                .filter(r -> path.equals(r.getPath()))
                .findFirst();
    }

    public void clear() {
        routes.clear();
    }
}

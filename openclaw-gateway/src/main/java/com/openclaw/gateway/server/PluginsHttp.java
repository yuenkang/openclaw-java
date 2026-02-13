package com.openclaw.gateway.server;

/**
 * Plugin HTTP request routing within the gateway.
 * <p>
 * Mirrors {@code server/plugins-http.ts :: createGatewayPluginRequestHandler}.
 * In Java this is a functional interface; the actual request/response types
 * come from the HTTP framework in use (Servlet, Jetty, Netty, etc.).
 */
public final class PluginsHttp {

    private PluginsHttp() {
    }

    /**
     * A plugin HTTP request handler that returns {@code true} if the request
     * was handled, {@code false} otherwise.
     *
     * @param <REQ> HTTP request type (framework-specific)
     * @param <RES> HTTP response type (framework-specific)
     */
    @FunctionalInterface
    public interface PluginHttpRequestHandler<REQ, RES> {
        /**
         * @return {@code true} if the request was handled by a plugin
         */
        boolean handle(REQ req, RES res) throws Exception;
    }
}

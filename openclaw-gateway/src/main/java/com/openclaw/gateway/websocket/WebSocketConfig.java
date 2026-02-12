package com.openclaw.gateway.websocket;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.config.ConfigService;
import com.openclaw.gateway.auth.AuthService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import org.springframework.web.socket.server.HandshakeInterceptor;
import org.springframework.web.socket.server.standard.ServletServerContainerFactoryBean;

import java.net.URI;
import java.util.Map;

/**
 * WebSocket configuration for the Gateway.
 * Registers the WebSocket endpoint at /ws with authentication interceptor.
 */
@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {

    private final ObjectMapper objectMapper;
    private final GatewayMethodRouter methodRouter;
    private final AuthService authService;
    private final ConfigService configService;

    public WebSocketConfig(ObjectMapper objectMapper, GatewayMethodRouter methodRouter,
            AuthService authService, ConfigService configService) {
        this.objectMapper = objectMapper;
        this.methodRouter = methodRouter;
        this.authService = authService;
        this.configService = configService;
    }

    @Override
    public void registerWebSocketHandlers(@NonNull WebSocketHandlerRegistry registry) {
        registry.addHandler(gatewayWebSocketHandler(), "/ws")
                .addInterceptors(connectionInterceptor())
                .setAllowedOrigins("*");
    }

    @Bean
    public GatewayWebSocketHandler gatewayWebSocketHandler() {
        return new GatewayWebSocketHandler(objectMapper, methodRouter, authService, configService);
    }

    /**
     * Handshake interceptor that captures connection metadata:
     * - token from URL query parameter
     * - remoteAddr, forwardedFor, realIp, host headers for auth/proxy detection
     *
     * Corresponds to TS metadata capture in ws-connection setup.
     */
    @Bean
    public HandshakeInterceptor connectionInterceptor() {
        return new HandshakeInterceptor() {
            @Override
            public boolean beforeHandshake(@NonNull ServerHttpRequest request,
                    @NonNull ServerHttpResponse response,
                    @NonNull WebSocketHandler wsHandler,
                    @NonNull Map<String, Object> attributes) {
                URI uri = request.getURI();

                // Extract token from query parameter
                String query = uri.getQuery();
                if (query != null) {
                    for (String param : query.split("&")) {
                        String[] kv = param.split("=", 2);
                        if (kv.length == 2 && "token".equals(kv[0])) {
                            attributes.put("auth.token", kv[1]);
                        }
                    }
                }

                // Capture remote address
                if (request.getRemoteAddress() != null) {
                    attributes.put("auth.remoteAddr",
                            request.getRemoteAddress().getAddress().getHostAddress());
                }

                // Capture proxy/host headers for isLocalDirectRequest detection
                var headers = request.getHeaders();
                String forwardedFor = headers.getFirst("X-Forwarded-For");
                String realIp = headers.getFirst("X-Real-IP");
                String host = headers.getFirst("Host");
                String forwardedHost = headers.getFirst("X-Forwarded-Host");

                if (forwardedFor != null)
                    attributes.put("auth.forwardedFor", forwardedFor);
                if (realIp != null)
                    attributes.put("auth.realIp", realIp);
                if (host != null)
                    attributes.put("auth.host", host);
                if (forwardedHost != null)
                    attributes.put("auth.forwardedHost", forwardedHost);

                return true; // always allow handshake, auth check happens in handler
            }

            @Override
            public void afterHandshake(@NonNull ServerHttpRequest request,
                    @NonNull ServerHttpResponse response,
                    @NonNull WebSocketHandler wsHandler,
                    @Nullable Exception exception) {
                // no-op
            }
        };
    }

    @Bean
    public ServletServerContainerFactoryBean createWebSocketContainer() {
        ServletServerContainerFactoryBean container = new ServletServerContainerFactoryBean();
        container.setMaxTextMessageBufferSize(512 * 1024); // 512KB
        container.setMaxBinaryMessageBufferSize(512 * 1024);
        container.setMaxSessionIdleTimeout(300_000L); // 5 min
        return container;
    }
}

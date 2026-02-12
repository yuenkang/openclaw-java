package com.openclaw.gateway;

import com.openclaw.common.config.ConfigService;
import com.openclaw.gateway.auth.AuthService;
import com.openclaw.gateway.node.DevicePairingService;
import com.openclaw.gateway.node.NodePairingService;
import com.openclaw.gateway.node.NodeRegistry;
import com.openclaw.gateway.routing.RouteResolver;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.session.SessionTranscriptStore;
import com.openclaw.gateway.websocket.EventBroadcaster;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.nio.file.Path;

/**
 * Spring configuration for Gateway beans.
 */
@Configuration
public class GatewayBeanConfig {

    @Value("${openclaw.config.path:~/.openclaw/config.json}")
    private String configPath;
    @Value("${openclaw.state.dir:~/.openclaw}")
    private String stateDir;

    @Bean
    public ConfigService configService() {
        String resolvedPath = configPath;
        if (resolvedPath.startsWith("~")) {
            resolvedPath = System.getProperty("user.home") + resolvedPath.substring(1);
        }
        return new ConfigService(Path.of(resolvedPath));
    }

    @Bean
    public SessionStore sessionStore() {
        return new SessionStore();
    }

    @Bean
    public GatewayMethodRouter gatewayMethodRouter() {
        return new GatewayMethodRouter();
    }

    @Bean
    public AuthService authService(ConfigService configService) {
        return new AuthService(configService);
    }

    @Bean
    public RouteResolver routeResolver(ConfigService configService) {
        RouteResolver resolver = new RouteResolver();
        try {
            resolver.loadFromConfig(configService.loadConfig());
        } catch (Exception e) {
            // Config may not exist yet, that's OK
        }
        return resolver;
    }

    @Bean
    public SessionTranscriptStore sessionTranscriptStore(ObjectMapper objectMapper) {
        return new SessionTranscriptStore(objectMapper);
    }

    @Bean
    public EventBroadcaster eventBroadcaster(ObjectMapper objectMapper) {
        return new EventBroadcaster(objectMapper);
    }

    @Bean
    public com.openclaw.gateway.cron.CronService cronService() {
        String home = System.getProperty("user.home");
        return new com.openclaw.gateway.cron.CronService(
                java.nio.file.Path.of(home, ".openclaw", "cron"));
    }

    @Bean
    public NodeRegistry nodeRegistry(ObjectMapper objectMapper) {
        return new NodeRegistry(objectMapper);
    }

    @Bean
    public NodePairingService nodePairingService(ObjectMapper objectMapper) {
        return new NodePairingService(resolveStateDir(), objectMapper);
    }

    @Bean
    public DevicePairingService devicePairingService(ObjectMapper objectMapper) {
        return new DevicePairingService(resolveStateDir(), objectMapper);
    }

    private Path resolveStateDir() {
        String resolved = stateDir;
        if (resolved.startsWith("~")) {
            resolved = System.getProperty("user.home") + resolved.substring(1);
        }
        return Path.of(resolved);
    }
}

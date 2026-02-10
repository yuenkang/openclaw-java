package com.openclaw.gateway;

import com.openclaw.common.config.ConfigService;
import com.openclaw.gateway.routing.RouteResolver;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.nio.file.Path;

/**
 * Spring configuration for Gateway beans.
 */
@Configuration
public class GatewayBeanConfig {

    @Value("${openclaw.config.path:#{systemProperties['user.home']}/.openclaw/config.json}")
    private String configPath;

    @Bean
    public ConfigService configService() {
        return new ConfigService(Path.of(configPath));
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
    public RouteResolver routeResolver(ConfigService configService) {
        RouteResolver resolver = new RouteResolver();
        try {
            resolver.loadFromConfig(configService.loadConfig());
        } catch (Exception e) {
            // Config may not exist yet, that's OK
        }
        return resolver;
    }
}

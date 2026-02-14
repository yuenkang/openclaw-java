package com.openclaw.channel.wechat;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.servlet.function.RouterFunction;
import org.springframework.web.servlet.function.RouterFunctions;
import org.springframework.web.servlet.function.ServerResponse;

import java.util.Map;

/**
 * Conditional Spring configuration that registers WeChat webhook routes
 * ONLY when WeChat is configured. Uses Spring MVC functional endpoints
 * (RouterFunction) so no @RestController is component-scanned.
 */
@Slf4j
@Configuration
public class WeChatWebhookRouterConfig {

    @Bean
    public RouterFunction<ServerResponse> weChatWebhookRoutes(ConfigService configService) {
        WeChatWebhookController controller = resolveController(configService);
        if (controller == null) {
            // Return a no-op router with a never-matching predicate
            return RouterFunctions.route()
                    .GET("/__wechat_disabled__", request -> ServerResponse.notFound().build())
                    .build();
        }

        log.info("WeChat webhook routes registered at /wechat-webhook");

        return RouterFunctions.route()
                .GET("/wechat-webhook", request -> {
                    String signature = request.param("signature").orElse(null);
                    String timestamp = request.param("timestamp").orElse(null);
                    String nonce = request.param("nonce").orElse(null);
                    String echostr = request.param("echostr").orElse(null);

                    if (signature == null || timestamp == null || nonce == null || echostr == null) {
                        return ServerResponse.badRequest().body("Missing required parameters");
                    }

                    ResponseEntity<String> result = controller.verify(signature, timestamp, nonce, echostr);
                    return toServerResponse(result);
                })
                .POST("/wechat-webhook", request -> {
                    String signature = request.param("signature").orElse(null);
                    String timestamp = request.param("timestamp").orElse(null);
                    String nonce = request.param("nonce").orElse(null);
                    String xmlBody = request.body(String.class);

                    ResponseEntity<String> result = controller.receiveMessage(signature, timestamp, nonce, xmlBody);
                    return toServerResponse(result);
                })
                .build();
    }

    @SuppressWarnings("unchecked")
    private WeChatWebhookController resolveController(ConfigService configService) {
        try {
            OpenClawConfig config = configService.loadConfig();
            if (config.getChannels() == null || config.getChannels().getProviders() == null) {
                return null;
            }

            Object wechatProvider = config.getChannels().getProviders().get("wechat");
            if (!(wechatProvider instanceof Map)) {
                return null;
            }

            Map<String, Object> wechatMap = (Map<String, Object>) wechatProvider;
            String appId = resolveString(wechatMap, "appId", "WECHAT_APP_ID");
            String appSecret = resolveString(wechatMap, "appSecret", "WECHAT_APP_SECRET");
            String token = resolveString(wechatMap, "token", "WECHAT_TOKEN");

            if (appId == null || appSecret == null) {
                return null;
            }
            if (token == null) {
                token = "";
            }

            WeChatOutboundAdapter adapter = new WeChatOutboundAdapter(appId, appSecret);
            WeChatMessageHandler handler = new WeChatMessageHandler(adapter);
            return new WeChatWebhookController(token, handler);
        } catch (Exception e) {
            log.debug("Failed to configure WeChat webhook: {}", e.getMessage());
            return null;
        }
    }

    private static ServerResponse toServerResponse(ResponseEntity<String> entity) {
        ServerResponse.BodyBuilder builder = ServerResponse.status(entity.getStatusCode());
        MediaType contentType = entity.getHeaders().getContentType();
        if (contentType != null) {
            builder.contentType(contentType);
        }
        String body = entity.getBody();
        return builder.body(body != null ? body : "");
    }

    private static String resolveString(Map<String, Object> map, String key, String envKey) {
        Object value = map.get(key);
        if (value instanceof String s && !s.isBlank()) {
            return s;
        }
        String envValue = System.getenv(envKey);
        if (envValue != null && !envValue.isBlank()) {
            return envValue;
        }
        return null;
    }
}

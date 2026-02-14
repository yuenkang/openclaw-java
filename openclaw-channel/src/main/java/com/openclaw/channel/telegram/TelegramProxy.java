package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

/**
 * Telegram proxy configuration.
 * Resolves HTTP proxy settings for Telegram API calls.
 * Corresponds to TypeScript's telegram/proxy.ts.
 */
@Slf4j
public class TelegramProxy {

    /**
     * Resolve proxy URL from environment or config.
     */
    public static String resolveProxyUrl() {
        // Check environment variables
        String https = System.getenv("HTTPS_PROXY");
        if (https != null && !https.isBlank())
            return https;

        String http = System.getenv("HTTP_PROXY");
        if (http != null && !http.isBlank())
            return http;

        String allProxy = System.getenv("ALL_PROXY");
        if (allProxy != null && !allProxy.isBlank())
            return allProxy;

        return null;
    }

    /**
     * Check if proxy is configured.
     */
    public static boolean isProxyConfigured() {
        return resolveProxyUrl() != null;
    }

    /**
     * Apply proxy settings to JVM system properties if configured.
     */
    public static void applyProxySettings() {
        String proxyUrl = resolveProxyUrl();
        if (proxyUrl == null)
            return;

        try {
            java.net.URI uri = java.net.URI.create(proxyUrl);
            String host = uri.getHost();
            int port = uri.getPort();

            if (host != null && port > 0) {
                System.setProperty("https.proxyHost", host);
                System.setProperty("https.proxyPort", String.valueOf(port));
                System.setProperty("http.proxyHost", host);
                System.setProperty("http.proxyPort", String.valueOf(port));
                log.info("Applied proxy settings: {}:{}", host, port);
            }
        } catch (Exception e) {
            log.warn("Failed to parse proxy URL: {}", proxyUrl);
        }
    }
}

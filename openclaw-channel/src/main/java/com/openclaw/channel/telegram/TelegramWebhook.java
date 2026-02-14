package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

/**
 * Telegram webhook setup and management.
 * Corresponds to TypeScript's telegram/webhook.ts + webhook-set.ts.
 */
@Slf4j
public class TelegramWebhook {

    /**
     * Set a webhook URL for the Telegram bot.
     */
    public static boolean setWebhook(String token, String url, String secretToken) {
        StringBuilder json = new StringBuilder("{");
        json.append("\"url\":\"").append(escapeJson(url)).append("\"");
        if (secretToken != null && !secretToken.isBlank()) {
            json.append(",\"secret_token\":\"").append(escapeJson(secretToken)).append("\"");
        }
        json.append(",\"drop_pending_updates\":false");
        json.append("}");

        String response = TelegramFetch.callApi(token, "setWebhook", json.toString());
        if (response == null) {
            log.error("Failed to set webhook: no response");
            return false;
        }

        boolean ok = response.contains("\"ok\":true");
        if (ok) {
            log.info("Webhook set successfully: {}", url);
        } else {
            log.error("Failed to set webhook: {}", response);
        }
        return ok;
    }

    /**
     * Delete the webhook for the Telegram bot.
     */
    public static boolean deleteWebhook(String token, boolean dropPendingUpdates) {
        String json = "{\"drop_pending_updates\":" + dropPendingUpdates + "}";
        String response = TelegramFetch.callApi(token, "deleteWebhook", json);
        if (response == null) {
            log.error("Failed to delete webhook");
            return false;
        }

        boolean ok = response.contains("\"ok\":true");
        if (ok) {
            log.info("Webhook deleted successfully");
        } else {
            log.error("Failed to delete webhook: {}", response);
        }
        return ok;
    }

    /**
     * Get current webhook info.
     */
    public static String getWebhookInfo(String token) {
        return TelegramFetch.callApi(token, "getWebhookInfo", null);
    }

    /**
     * Create a webhook callback path.
     */
    public static String createWebhookPath(String basePath) {
        if (basePath == null || basePath.isBlank()) {
            return "/telegram-webhook";
        }
        return basePath.endsWith("/") ? basePath + "telegram-webhook"
                : basePath + "/telegram-webhook";
    }

    private static String escapeJson(String value) {
        if (value == null)
            return "";
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}

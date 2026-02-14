package com.openclaw.channel.telegram;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link TelegramWebhook} — webhook path creation.
 */
class TelegramWebhookTest {

    @Test
    void createWebhookPath_nullReturnsDefault() {
        assertEquals("/telegram-webhook",
                TelegramWebhook.createWebhookPath(null));
    }

    @Test
    void createWebhookPath_blankReturnsDefault() {
        assertEquals("/telegram-webhook",
                TelegramWebhook.createWebhookPath("  "));
    }

    @Test
    void createWebhookPath_withTrailingSlash() {
        assertEquals("/api/telegram-webhook",
                TelegramWebhook.createWebhookPath("/api/"));
    }

    @Test
    void createWebhookPath_withoutTrailingSlash() {
        assertEquals("/api/telegram-webhook",
                TelegramWebhook.createWebhookPath("/api"));
    }
}

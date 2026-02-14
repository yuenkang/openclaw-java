package com.openclaw.channel.telegram;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link TelegramSend} — chat ID normalization, message ID
 * validation, and parse error detection.
 */
class TelegramSendTest {

    // =========================================================================
    // normalizeChatId
    // =========================================================================

    @Test
    void normalizeChatId_atUsername() {
        assertEquals("@testbot", TelegramSend.normalizeChatId("@testbot"));
    }

    @Test
    void normalizeChatId_tmeUrl() {
        assertEquals("@testbot",
                TelegramSend.normalizeChatId("https://t.me/testbot"));
    }

    @Test
    void normalizeChatId_tmeUrlWithPath() {
        assertEquals("@testbot",
                TelegramSend.normalizeChatId("https://t.me/testbot/123"));
    }

    @Test
    void normalizeChatId_numericId() {
        assertEquals("12345", TelegramSend.normalizeChatId("12345"));
    }

    @Test
    void normalizeChatId_negativeGroupId() {
        assertEquals("-100123456789",
                TelegramSend.normalizeChatId("-100123456789"));
    }

    @Test
    void normalizeChatId_bareUsername() {
        // 5+ chars, starts with letter → treated as username
        assertEquals("@mybot", TelegramSend.normalizeChatId("mybot"));
    }

    // =========================================================================
    // normalizeMessageId
    // =========================================================================

    @Test
    void normalizeMessageId_validString() {
        assertEquals(42, TelegramSend.normalizeMessageId("42"));
    }

    @Test
    void normalizeMessageId_validInt() {
        assertEquals(1, TelegramSend.normalizeMessageId(1));
    }

    @Test
    void normalizeMessageId_zeroThrows() {
        assertThrows(IllegalArgumentException.class,
                () -> TelegramSend.normalizeMessageId("0"));
    }

    @Test
    void normalizeMessageId_negativeThrows() {
        assertThrows(IllegalArgumentException.class,
                () -> TelegramSend.normalizeMessageId("-1"));
    }

    @Test
    void normalizeMessageId_nonNumericThrows() {
        assertThrows(IllegalArgumentException.class,
                () -> TelegramSend.normalizeMessageId("abc"));
    }

    // =========================================================================
    // isParseError
    // =========================================================================

    @Test
    void isParseError_recognizesKnownPatterns() {
        assertTrue(TelegramSend.isParseError("can't parse entities in text"));
        assertTrue(TelegramSend.isParseError(
                "Bad Request: can't find end of the entity"));
    }

    @Test
    void isParseError_nullReturnsFalse() {
        assertFalse(TelegramSend.isParseError(null));
    }

    @Test
    void isParseError_unrelatedError() {
        assertFalse(TelegramSend.isParseError("chat not found"));
    }
}

package com.openclaw.gateway.chat;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ChatSanitizer}.
 */
class ChatSanitizerTest {

    // =========================================================================
    // stripEnvelope
    // =========================================================================

    @Test
    void stripEnvelope_noEnvelope_returnsOriginal() {
        String text = "Hello, world!";
        assertEquals(text, ChatSanitizer.stripEnvelope(text));
    }

    @Test
    void stripEnvelope_withWebChatEnvelope() {
        String text = "[WebChat 2024-01-01T00:00Z] Hello, world!";
        assertEquals("Hello, world!", ChatSanitizer.stripEnvelope(text));
    }

    @Test
    void stripEnvelope_withTelegramEnvelope() {
        String text = "[Telegram 2024-01-01T12:00Z] How are you?";
        assertEquals("How are you?", ChatSanitizer.stripEnvelope(text));
    }

    @Test
    void stripEnvelope_withSimpleTimestamp() {
        String text = "[2024-01-01 12:00] content here";
        assertEquals("content here", ChatSanitizer.stripEnvelope(text));
    }

    @Test
    void stripEnvelope_withNonEnvelopeBrackets_preservesOriginal() {
        String text = "[some random stuff] not stripped";
        assertEquals(text, ChatSanitizer.stripEnvelope(text));
    }

    // =========================================================================
    // stripMessageIdHints
    // =========================================================================

    @Test
    void stripMessageIdHints_noHints_returnsOriginal() {
        assertEquals("hello", ChatSanitizer.stripMessageIdHints("hello"));
    }

    @Test
    void stripMessageIdHints_withHint() {
        String text = "Hello\n[message_id: abc123]\nWorld";
        assertEquals("Hello\nWorld", ChatSanitizer.stripMessageIdHints(text));
    }

    @Test
    void stripMessageIdHints_multipleHints() {
        String text = "A\n[message_id: 1]\nB\n[message_id: 2]\nC";
        assertEquals("A\nB\nC", ChatSanitizer.stripMessageIdHints(text));
    }

    // =========================================================================
    // stripEnvelopeFromMessage
    // =========================================================================

    @Test
    void stripEnvelopeFromMessage_userMessage_strips() {
        Map<String, Object> msg = new LinkedHashMap<>();
        msg.put("role", "user");
        msg.put("content", "[WebChat 2024-01-01T00:00Z] hi");

        var result = ChatSanitizer.stripEnvelopeFromMessage(msg);
        assertEquals("hi", result.get("content"));
    }

    @Test
    void stripEnvelopeFromMessage_assistantMessage_unchanged() {
        Map<String, Object> msg = new LinkedHashMap<>();
        msg.put("role", "assistant");
        msg.put("content", "[WebChat 2024-01-01T00:00Z] should not change");

        var result = ChatSanitizer.stripEnvelopeFromMessage(msg);
        assertSame(msg, result); // should return same instance
    }

    @Test
    void stripEnvelopeFromMessage_null_returnsNull() {
        assertNull(ChatSanitizer.stripEnvelopeFromMessage(null));
    }

    // =========================================================================
    // stripEnvelopeFromMessages
    // =========================================================================

    @Test
    void stripEnvelopeFromMessages_nullOrEmpty() {
        assertNull(ChatSanitizer.stripEnvelopeFromMessages(null));
        var empty = List.<Map<String, Object>>of();
        assertSame(empty, ChatSanitizer.stripEnvelopeFromMessages(empty));
    }

    @Test
    void stripEnvelopeFromMessages_mixedRoles() {
        Map<String, Object> user = new LinkedHashMap<>();
        user.put("role", "user");
        user.put("content", "[Telegram 2024-01-01T12:00Z] question");

        Map<String, Object> assistant = new LinkedHashMap<>();
        assistant.put("role", "assistant");
        assistant.put("content", "answer");

        var result = ChatSanitizer.stripEnvelopeFromMessages(List.of(user, assistant));
        assertEquals("question", result.get(0).get("content"));
        assertEquals("answer", result.get(1).get("content"));
    }
}

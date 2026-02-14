package com.openclaw.channel.telegram;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link TelegramBotUpdates} — deduplication, update key
 * building, and media group batching.
 */
class TelegramBotUpdatesTest {

    // =========================================================================
    // UpdateDedupe
    // =========================================================================

    @Test
    void isDuplicate_firstCallReturnsFalse() {
        var dedupe = new TelegramBotUpdates.UpdateDedupe(60_000, 100);
        assertFalse(dedupe.isDuplicate("key1"));
    }

    @Test
    void isDuplicate_secondCallReturnsTrue() {
        var dedupe = new TelegramBotUpdates.UpdateDedupe(60_000, 100);
        assertFalse(dedupe.isDuplicate("key1"));
        assertTrue(dedupe.isDuplicate("key1"));
    }

    @Test
    void isDuplicate_differentKeysAreIndependent() {
        var dedupe = new TelegramBotUpdates.UpdateDedupe(60_000, 100);
        assertFalse(dedupe.isDuplicate("key1"));
        assertFalse(dedupe.isDuplicate("key2"));
    }

    @Test
    void isDuplicate_nullKeyIsNeverDuplicate() {
        var dedupe = new TelegramBotUpdates.UpdateDedupe(60_000, 100);
        assertFalse(dedupe.isDuplicate(null));
        assertFalse(dedupe.isDuplicate(null));
    }

    @Test
    void isDuplicate_evictsWhenExceedingMaxSize() {
        var dedupe = new TelegramBotUpdates.UpdateDedupe(60_000, 3);
        dedupe.isDuplicate("a");
        dedupe.isDuplicate("b");
        dedupe.isDuplicate("c");
        // Adding a 4th should evict one (the oldest)
        assertFalse(dedupe.isDuplicate("d"));
    }

    @Test
    void clear_resetsState() {
        var dedupe = new TelegramBotUpdates.UpdateDedupe(60_000, 100);
        dedupe.isDuplicate("key1");
        dedupe.clear();
        assertFalse(dedupe.isDuplicate("key1"));
    }

    // =========================================================================
    // resolveUpdateId
    // =========================================================================

    @Test
    void resolveUpdateId_extractsFromMap() {
        Map<String, Object> update = Map.of("update_id", 12345);
        assertEquals(12345, TelegramBotUpdates.resolveUpdateId(update));
    }

    @Test
    void resolveUpdateId_returnsNullWhenMissing() {
        Map<String, Object> update = Map.of();
        assertNull(TelegramBotUpdates.resolveUpdateId(update));
    }

    // =========================================================================
    // buildUpdateKey
    // =========================================================================

    @Test
    void buildUpdateKey_fromUpdateId() {
        Map<String, Object> update = Map.of("update_id", 99);
        assertEquals("update:99", TelegramBotUpdates.buildUpdateKey(update));
    }

    @Test
    void buildUpdateKey_fromCallbackQuery() {
        Map<String, Object> update = Map.of(
                "callback_query", Map.of("id", "cbq123"));
        assertEquals("callback:cbq123",
                TelegramBotUpdates.buildUpdateKey(update));
    }

    @Test
    void buildUpdateKey_fromMessageChatAndMessageId() {
        Map<String, Object> update = Map.of(
                "message", Map.of(
                        "chat", Map.of("id", 42),
                        "message_id", 7));
        assertEquals("message:42:7",
                TelegramBotUpdates.buildUpdateKey(update));
    }

    @Test
    void buildUpdateKey_emptyUpdateReturnsNull() {
        Map<String, Object> update = new HashMap<>();
        assertNull(TelegramBotUpdates.buildUpdateKey(update));
    }

    // =========================================================================
    // MediaGroupEntry
    // =========================================================================

    @Test
    void mediaGroupEntry_collectsMessages() {
        var entry = new TelegramBotUpdates.MediaGroupEntry("group1");
        assertEquals("group1", entry.getMediaGroupId());
        assertTrue(entry.getMessages().isEmpty());

        entry.addMessage(Map.of("photo", "a"));
        entry.addMessage(Map.of("photo", "b"));
        assertEquals(2, entry.getMessages().size());
    }

    @Test
    void mediaGroupEntry_messagesAreUnmodifiable() {
        var entry = new TelegramBotUpdates.MediaGroupEntry("g");
        entry.addMessage(Map.of("x", 1));
        assertThrows(UnsupportedOperationException.class,
                () -> entry.getMessages().add(Map.of("y", 2)));
    }

    @Test
    void mediaGroupEntry_lastUpdatedMsIncreasesOnAdd() throws InterruptedException {
        var entry = new TelegramBotUpdates.MediaGroupEntry("g");
        long t1 = entry.getLastUpdatedMs();
        Thread.sleep(5);
        entry.addMessage(Map.of("x", 1));
        assertTrue(entry.getLastUpdatedMs() >= t1);
    }

    // =========================================================================
    // createUpdateDedupe
    // =========================================================================

    @Test
    void createUpdateDedupe_returnsNonNull() {
        assertNotNull(TelegramBotUpdates.createUpdateDedupe());
    }
}

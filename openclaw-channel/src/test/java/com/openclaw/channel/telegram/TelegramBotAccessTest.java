package com.openclaw.channel.telegram;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link TelegramBotAccess} — allow-list normalization
 * and sender access control.
 */
class TelegramBotAccessTest {

    // =========================================================================
    // normalizeAllowFrom
    // =========================================================================

    @Test
    void normalizeAllowFrom_nullReturnsEmpty() {
        var result = TelegramBotAccess.normalizeAllowFrom(null);
        assertFalse(result.hasEntries());
        assertFalse(result.hasWildcard());
    }

    @Test
    void normalizeAllowFrom_emptyList() {
        var result = TelegramBotAccess.normalizeAllowFrom(List.of());
        assertFalse(result.hasEntries());
    }

    @Test
    void normalizeAllowFrom_detectsWildcard() {
        var result = TelegramBotAccess.normalizeAllowFrom(List.of("*", "alice"));
        assertTrue(result.hasWildcard());
        assertTrue(result.hasEntries());
        // wildcard is stripped from the entries list
        assertFalse(result.entries().contains("*"));
    }

    @Test
    void normalizeAllowFrom_stripsTelegramPrefix() {
        var result = TelegramBotAccess.normalizeAllowFrom(
                List.of("telegram:alice", "tg:bob"));
        assertTrue(result.entries().contains("alice"));
        assertTrue(result.entries().contains("bob"));
    }

    @Test
    void normalizeAllowFrom_caseInsensitiveLower() {
        var result = TelegramBotAccess.normalizeAllowFrom(List.of("Alice"));
        assertTrue(result.entriesLower().contains("alice"));
    }

    // =========================================================================
    // isSenderAllowed (NormalizedAllowFrom overload)
    // =========================================================================

    @Test
    void isSenderAllowed_noEntriesAllowsAll() {
        var allow = TelegramBotAccess.normalizeAllowFrom(null);
        assertTrue(TelegramBotAccess.isSenderAllowed(allow, "123", "anyone"));
    }

    @Test
    void isSenderAllowed_wildcardAllowsAll() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("*"));
        assertTrue(TelegramBotAccess.isSenderAllowed(allow, "123", "anyone"));
    }

    @Test
    void isSenderAllowed_matchByIdExact() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("12345"));
        assertTrue(TelegramBotAccess.isSenderAllowed(allow, "12345", null));
    }

    @Test
    void isSenderAllowed_matchByUsernameCaseInsensitive() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("Alice"));
        assertTrue(TelegramBotAccess.isSenderAllowed(allow, "999", "alice"));
        assertTrue(TelegramBotAccess.isSenderAllowed(allow, "999", "ALICE"));
    }

    @Test
    void isSenderAllowed_matchByUsernameWithAtPrefix() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("@alice"));
        assertTrue(TelegramBotAccess.isSenderAllowed(allow, "999", "alice"));
    }

    @Test
    void isSenderAllowed_deniedWhenNoMatch() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("alice"));
        assertFalse(TelegramBotAccess.isSenderAllowed(allow, "999", "bob"));
    }

    @Test
    void isSenderAllowed_deniedWithBlankUsername() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("alice"));
        assertFalse(TelegramBotAccess.isSenderAllowed(allow, "999", ""));
    }

    // =========================================================================
    // resolveSenderAllowMatch
    // =========================================================================

    @Test
    void resolveSenderAllowMatch_wildcardMatch() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("*"));
        var result = TelegramBotAccess.resolveSenderAllowMatch(allow, "123", "user");
        assertTrue(result.allowed());
        assertEquals("wildcard", result.matchSource());
    }

    @Test
    void resolveSenderAllowMatch_idMatch() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("123"));
        var result = TelegramBotAccess.resolveSenderAllowMatch(allow, "123", "user");
        assertTrue(result.allowed());
        assertEquals("id", result.matchSource());
    }

    @Test
    void resolveSenderAllowMatch_usernameMatch() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("alice"));
        var result = TelegramBotAccess.resolveSenderAllowMatch(allow, "999", "Alice");
        assertTrue(result.allowed());
        assertEquals("username", result.matchSource());
    }

    @Test
    void resolveSenderAllowMatch_denied() {
        var allow = TelegramBotAccess.normalizeAllowFrom(List.of("alice"));
        var result = TelegramBotAccess.resolveSenderAllowMatch(allow, "999", "bob");
        assertFalse(result.allowed());
        assertNull(result.matchSource());
    }

    // =========================================================================
    // normalizeAllowFromWithStore
    // =========================================================================

    @Test
    void normalizeAllowFromWithStore_combinesBothLists() {
        var result = TelegramBotAccess.normalizeAllowFromWithStore(
                List.of("alice"), List.of("bob"));
        assertTrue(result.entries().contains("alice"));
        assertTrue(result.entries().contains("bob"));
    }

    @Test
    void normalizeAllowFromWithStore_handlesNulls() {
        var result = TelegramBotAccess.normalizeAllowFromWithStore(null, List.of("bob"));
        assertTrue(result.entries().contains("bob"));
    }
}

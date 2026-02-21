package com.openclaw.agent.runtime;

import com.openclaw.agent.models.ModelProvider;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link CompactionService}.
 */
class CompactionServiceTest {

    // ── Token estimation ──────────────────────────────────────────

    @Nested
    class TokenEstimation {

        @Test
        void nullOrEmpty_returnsZero() {
            assertEquals(0, CompactionService.estimateTokens(null));
            assertEquals(0, CompactionService.estimateTokens(""));
        }

        @Test
        void shortText_atLeastOneToken() {
            assertEquals(1, CompactionService.estimateTokens("Hi"));
        }

        @Test
        void longerText_proportionalToLength() {
            // 100 chars / 4 = 25 tokens
            String text = "a".repeat(100);
            assertEquals(25, CompactionService.estimateTokens(text));
        }

        @Test
        void estimateMessagesTokens_sumOfContents() {
            var msgs = List.of(
                    msg("user", "Hello World"), // 11 chars -> 3 tokens
                    msg("assistant", "Hi there!") // 9 chars -> 3 tokens
            );
            int total = CompactionService.estimateMessagesTokens(msgs);
            assertTrue(total > 0);
            assertEquals(
                    CompactionService.estimateTokens("Hello World")
                            + CompactionService.estimateTokens("Hi there!"),
                    total);
        }

        @Test
        void estimateMessagesTokens_nullContentSkipped() {
            var msg = ModelProvider.ChatMessage.builder().role("user").content(null).build();
            assertEquals(0, CompactionService.estimateMessagesTokens(List.of(msg)));
        }
    }

    // ── History pruning ───────────────────────────────────────────

    @Nested
    class HistoryPruning {

        @Test
        void emptyMessages_noPruning() {
            var result = CompactionService.pruneHistoryForContextShare(List.of(), 100000);
            assertTrue(result.messages().isEmpty());
            assertTrue(result.droppedMessages().isEmpty());
            assertEquals(0, result.droppedTokens());
            assertEquals(0, result.keptTokens());
        }

        @Test
        void nullMessages_noPruning() {
            var result = CompactionService.pruneHistoryForContextShare(null, 100000);
            assertTrue(result.messages().isEmpty());
        }

        @Test
        void smallHistory_fitsInBudget() {
            var msgs = List.of(msg("user", "Hello"), msg("assistant", "Hi"));
            var result = CompactionService.pruneHistoryForContextShare(msgs, 100000);
            assertEquals(2, result.messages().size());
            assertTrue(result.droppedMessages().isEmpty());
        }

        @Test
        void largeHistory_keepsRecent() {
            // Create a history with lots of tokens
            List<ModelProvider.ChatMessage> msgs = new ArrayList<>();
            for (int i = 0; i < 100; i++) {
                msgs.add(msg("user", "Message " + i + " " + "x".repeat(200)));
            }

            // Small context window forces pruning
            var result = CompactionService.pruneHistoryForContextShare(msgs, 1000, 0.5);
            assertTrue(result.messages().size() < msgs.size());
            assertFalse(result.droppedMessages().isEmpty());
            assertTrue(result.droppedTokens() > 0);
            assertTrue(result.keptTokens() > 0);
            // The budget should be 1000 * 0.5 = 500
            assertEquals(500, result.budgetTokens());
        }

        @Test
        void invalidHistoryShare_defaultsToSixtyPercent() {
            var msgs = List.of(msg("user", "Hello"));
            var result = CompactionService.pruneHistoryForContextShare(msgs, 10000, -1);
            // default share = 0.6, so budget = 6000
            assertEquals(6000, result.budgetTokens());
        }
    }

    // ── Message chunking ──────────────────────────────────────────

    @Nested
    class MessageChunking {

        @Test
        void chunkByMaxTokens_singleChunk() {
            var msgs = List.of(msg("user", "Hello"), msg("assistant", "Hi"));
            var chunks = CompactionService.chunkMessagesByMaxTokens(msgs, 10000);
            assertEquals(1, chunks.size());
            assertEquals(2, chunks.get(0).size());
        }

        @Test
        void chunkByMaxTokens_multipleChunks() {
            List<ModelProvider.ChatMessage> msgs = new ArrayList<>();
            for (int i = 0; i < 10; i++) {
                msgs.add(msg("user", "x".repeat(100))); // ~25 tokens each
            }
            // Max 30 tokens per chunk -> ~1 msg per chunk
            var chunks = CompactionService.chunkMessagesByMaxTokens(msgs, 30);
            assertTrue(chunks.size() > 1);
        }

        @Test
        void splitByTokenShare_singlePart() {
            var msgs = List.of(msg("user", "Hello"));
            var parts = CompactionService.splitMessagesByTokenShare(msgs, 1);
            assertEquals(1, parts.size());
        }

        @Test
        void splitByTokenShare_multipleParts() {
            List<ModelProvider.ChatMessage> msgs = new ArrayList<>();
            for (int i = 0; i < 10; i++) {
                msgs.add(msg("user", "Message " + i + " content here"));
            }
            var parts = CompactionService.splitMessagesByTokenShare(msgs, 3);
            assertTrue(parts.size() >= 2);
            // All messages should be present
            int totalMsgs = parts.stream().mapToInt(List::size).sum();
            assertEquals(10, totalMsgs);
        }

        @Test
        void splitByTokenShare_partsExceedMessages() {
            var msgs = List.of(msg("user", "Hello"));
            var parts = CompactionService.splitMessagesByTokenShare(msgs, 100);
            assertEquals(1, parts.size());
        }
    }

    // ── PruneResult record ────────────────────────────────────────

    @Test
    void pruneResultRecord() {
        var result = new CompactionService.PruneResult(
                List.of(msg("user", "kept")),
                List.of(msg("user", "dropped")),
                50, 30, 100);
        assertEquals(1, result.messages().size());
        assertEquals(1, result.droppedMessages().size());
        assertEquals(50, result.droppedTokens());
        assertEquals(30, result.keptTokens());
        assertEquals(100, result.budgetTokens());
    }

    // ── CompactionResult record ───────────────────────────────────

    @Test
    void compactionResultRecord() {
        var result = new CompactionService.CompactionResult(
                List.of(msg("system", "summary")),
                "A summary", 200, 50);
        assertEquals(1, result.messages().size());
        assertEquals("A summary", result.summary());
        assertEquals(200, result.compactedTokens());
        assertEquals(50, result.remainingTokens());
    }

    // ── Helpers ───────────────────────────────────────────────────

    private static ModelProvider.ChatMessage msg(String role, String content) {
        return ModelProvider.ChatMessage.builder().role(role).content(content).build();
    }
}

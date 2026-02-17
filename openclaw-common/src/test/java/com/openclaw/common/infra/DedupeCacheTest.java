package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;

import static org.junit.jupiter.api.Assertions.*;

class DedupeCacheTest {

    @Nested
    class BasicBehavior {
        @Test
        void firstCheck_returnsFalse() {
            var cache = new DedupeCache(60_000, 100);
            assertFalse(cache.isDuplicate("key1", 1000));
        }

        @Test
        void secondCheck_withinTtl_returnsTrue() {
            var cache = new DedupeCache(60_000, 100);
            assertFalse(cache.isDuplicate("key1", 1000));
            assertTrue(cache.isDuplicate("key1", 2000));
        }

        @Test
        void afterTtlExpiry_returnsFalse() {
            var cache = new DedupeCache(5_000, 100);
            assertFalse(cache.isDuplicate("key1", 1000));
            // After TTL: 1000 + 5000 = 6000
            assertFalse(cache.isDuplicate("key1", 7000));
        }

        @Test
        void nullOrEmptyKey_returnsFalse() {
            var cache = new DedupeCache(60_000, 100);
            assertFalse(cache.isDuplicate(null, 1000));
            assertFalse(cache.isDuplicate("", 1000));
        }
    }

    @Nested
    class MaxSize {
        @Test
        void evictsOldestWhenFull() {
            var cache = new DedupeCache(60_000, 3);
            cache.isDuplicate("a", 1000);
            cache.isDuplicate("b", 2000);
            cache.isDuplicate("c", 3000);
            cache.isDuplicate("d", 4000); // should evict "a"
            assertEquals(3, cache.size());
            assertFalse(cache.isDuplicate("a", 5000)); // "a" was evicted => not a dup
        }
    }

    @Nested
    class ClearAndSize {
        @Test
        void clearEmptiesCache() {
            var cache = new DedupeCache(60_000, 100);
            cache.isDuplicate("a", 1000);
            cache.isDuplicate("b", 2000);
            assertEquals(2, cache.size());
            cache.clear();
            assertEquals(0, cache.size());
        }
    }
}

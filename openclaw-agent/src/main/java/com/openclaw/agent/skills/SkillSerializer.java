package com.openclaw.agent.skills;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * Serializes asynchronous tasks by key, ensuring that only one task per key
 * runs at a time. Subsequent calls with the same key queue behind the current
 * one.
 * Corresponds to TypeScript skills/serialize.ts.
 */
public final class SkillSerializer {

    private SkillSerializer() {
    }

    private static final Map<String, CompletableFuture<?>> SYNC_QUEUE = new ConcurrentHashMap<>();

    /**
     * Execute a task serially by key. If a task with the same key is already
     * running, the new task waits for it to complete first.
     *
     * @param key  serialization key
     * @param task the task to execute
     * @param <T>  result type
     * @return CompletableFuture with the task result
     */
    public static <T> CompletableFuture<T> serializeByKey(String key, Supplier<CompletableFuture<T>> task) {
        CompletableFuture<?> prev = SYNC_QUEUE.getOrDefault(key, CompletableFuture.completedFuture(null));

        CompletableFuture<T> next = prev
                .handle((ignored, ex) -> null) // always proceed, even on failure
                .thenCompose(ignored -> task.get());

        SYNC_QUEUE.put(key, next);

        // Clean up when done
        return next.whenComplete((result, ex) -> {
            if (SYNC_QUEUE.get(key) == next) {
                SYNC_QUEUE.remove(key);
            }
        });
    }

    /**
     * Execute a synchronous task serially by key.
     */
    public static <T> CompletableFuture<T> serializeByKeySync(String key, Supplier<T> task) {
        return serializeByKey(key, () -> CompletableFuture.supplyAsync(task));
    }
}

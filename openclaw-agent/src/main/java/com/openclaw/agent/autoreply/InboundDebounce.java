package com.openclaw.agent.autoreply;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Inbound message debouncing — buffers items by key and flushes after a delay.
 * Mirrors {@code auto-reply/inbound-debounce.ts}.
 */
public final class InboundDebounce {

    private InboundDebounce() {
    }

    /** Resolve a numeric millisecond value. */
    public static OptionalLong resolveMs(Object value) {
        if (!(value instanceof Number n))
            return OptionalLong.empty();
        double d = n.doubleValue();
        if (!Double.isFinite(d))
            return OptionalLong.empty();
        return OptionalLong.of(Math.max(0, (long) d));
    }

    /**
     * Resolve inbound debounce milliseconds from config.
     *
     * @param inboundConfig the messages.inbound config map
     * @param channel       channel name for per-channel overrides
     * @param overrideMs    explicit override (highest priority)
     * @return debounce time in ms, 0 if none
     */
    @SuppressWarnings("unchecked")
    public static long resolveInboundDebounceMs(Map<String, Object> inboundConfig,
            String channel, Long overrideMs) {
        OptionalLong ov = overrideMs != null ? OptionalLong.of(Math.max(0, overrideMs)) : OptionalLong.empty();
        if (ov.isPresent())
            return ov.getAsLong();

        if (inboundConfig != null) {
            // Per-channel override
            Object byChannel = inboundConfig.get("byChannel");
            if (byChannel instanceof Map<?, ?> byMap && channel != null) {
                Object chVal = byMap.get(channel);
                OptionalLong chMs = resolveMs(chVal);
                if (chMs.isPresent())
                    return chMs.getAsLong();
            }
            // Base debounce
            OptionalLong base = resolveMs(inboundConfig.get("debounceMs"));
            if (base.isPresent())
                return base.getAsLong();
        }
        return 0;
    }

    /**
     * Create a generic inbound debouncer that buffers items and flushes after a
     * delay.
     *
     * @param <T>            item type
     * @param debounceMs     debounce window in ms
     * @param buildKey       maps an item to a grouping key (null/empty = no
     *                       debounce)
     * @param shouldDebounce predicate to decide if item should be debounced
     *                       (default: true)
     * @param onFlush        consumer called with accumulated items on flush
     * @param onError        error handler (optional)
     */
    public static <T> Debouncer<T> createInboundDebouncer(
            long debounceMs,
            Function<T, String> buildKey,
            Predicate<T> shouldDebounce,
            Consumer<List<T>> onFlush,
            java.util.function.BiConsumer<Throwable, List<T>> onError) {
        return new Debouncer<>(debounceMs, buildKey, shouldDebounce, onFlush, onError);
    }

    /** A debouncer that buffers items by key and flushes after a delay. */
    public static class Debouncer<T> {
        private final long debounceMs;
        private final Function<T, String> buildKey;
        private final Predicate<T> shouldDebounce;
        private final Consumer<List<T>> onFlush;
        private final java.util.function.BiConsumer<Throwable, List<T>> onError;
        private final Map<String, DebounceBuffer<T>> buffers = new ConcurrentHashMap<>();
        private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "inbound-debounce");
            t.setDaemon(true);
            return t;
        });

        Debouncer(long debounceMs, Function<T, String> buildKey,
                Predicate<T> shouldDebounce,
                Consumer<List<T>> onFlush,
                java.util.function.BiConsumer<Throwable, List<T>> onError) {
            this.debounceMs = Math.max(0, debounceMs);
            this.buildKey = buildKey;
            this.shouldDebounce = shouldDebounce != null ? shouldDebounce : t -> true;
            this.onFlush = onFlush;
            this.onError = onError;
        }

        /** Enqueue an item for debounced processing. */
        public void enqueue(T item) {
            String key = buildKey.apply(item);
            boolean canDebounce = debounceMs > 0 && shouldDebounce.test(item);

            if (!canDebounce || key == null || key.isEmpty()) {
                if (key != null && buffers.containsKey(key)) {
                    flushKey(key);
                }
                doFlush(List.of(item));
                return;
            }

            DebounceBuffer<T> existing = buffers.get(key);
            if (existing != null) {
                existing.items.add(item);
                scheduleFlush(key, existing);
                return;
            }

            DebounceBuffer<T> buffer = new DebounceBuffer<>();
            buffer.items.add(item);
            buffers.put(key, buffer);
            scheduleFlush(key, buffer);
        }

        /** Force-flush items for a specific key. */
        public void flushKey(String key) {
            DebounceBuffer<T> buffer = buffers.remove(key);
            if (buffer == null)
                return;
            if (buffer.future != null)
                buffer.future.cancel(false);
            if (!buffer.items.isEmpty())
                doFlush(new ArrayList<>(buffer.items));
        }

        private void scheduleFlush(String key, DebounceBuffer<T> buffer) {
            if (buffer.future != null)
                buffer.future.cancel(false);
            buffer.future = scheduler.schedule(() -> {
                buffers.remove(key);
                if (!buffer.items.isEmpty())
                    doFlush(new ArrayList<>(buffer.items));
            }, debounceMs, TimeUnit.MILLISECONDS);
        }

        private void doFlush(List<T> items) {
            try {
                onFlush.accept(items);
            } catch (Exception e) {
                if (onError != null)
                    onError.accept(e, items);
            }
        }

        /** Shutdown the scheduler. */
        public void shutdown() {
            scheduler.shutdown();
        }
    }

    private static class DebounceBuffer<T> {
        final List<T> items = new ArrayList<>();
        ScheduledFuture<?> future;
    }
}

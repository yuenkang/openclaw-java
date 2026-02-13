package com.openclaw.agent.autoreply.reply.queue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Drain followup queues — dequeues items and dispatches them via a callback,
 * with collect-mode batching and summary generation.
 * Mirrors {@code auto-reply/reply/queue/drain.ts}.
 */
public final class QueueDrain {

    private static final Logger log = LoggerFactory.getLogger(QueueDrain.class);

    private QueueDrain() {
    }

    /**
     * Schedule an asynchronous drain of the followup queue for the given key.
     *
     * @param key         session queue key
     * @param runFollowup callback to execute each followup run
     */
    public static void scheduleFollowupDrain(
            String key,
            Function<QueueTypes.FollowupRun, CompletableFuture<Void>> runFollowup) {

        QueueState.FollowupQueueState queue = QueueState.FOLLOWUP_QUEUES.get(key);
        if (queue == null || queue.draining)
            return;
        queue.draining = true;

        CompletableFuture.runAsync(() -> {
            try {
                boolean forceIndividualCollect = false;

                while (!queue.items.isEmpty() || queue.droppedCount > 0) {
                    // Debounce wait
                    if (queue.debounceMs > 0) {
                        Thread.sleep(queue.debounceMs);
                    }

                    if (QueueTypes.MODE_COLLECT.equals(queue.mode)) {
                        if (forceIndividualCollect) {
                            QueueTypes.FollowupRun next = queue.items.isEmpty() ? null : queue.items.remove(0);
                            if (next == null)
                                break;
                            runFollowup.apply(next).join();
                            continue;
                        }

                        // Collect all items into a single batch prompt
                        List<QueueTypes.FollowupRun> items = new ArrayList<>(queue.items);
                        queue.items.clear();

                        String summaryPrompt = buildQueueSummaryPrompt(queue);
                        QueueTypes.FollowupRunConfig run = !items.isEmpty()
                                ? items.get(items.size() - 1).run()
                                : queue.lastRun;
                        if (run == null)
                            break;

                        // Build collect prompt
                        StringBuilder prompt = new StringBuilder("[Queued messages while agent was busy]\n");
                        if (summaryPrompt != null)
                            prompt.append(summaryPrompt).append("\n");
                        for (int i = 0; i < items.size(); i++) {
                            prompt.append("---\nQueued #").append(i + 1).append("\n")
                                    .append(items.get(i).prompt().trim()).append("\n");
                        }

                        // Preserve routing from collected items
                        String channel = items.stream().map(QueueTypes.FollowupRun::originatingChannel)
                                .filter(c -> c != null).findFirst().orElse(null);
                        String to = items.stream().map(QueueTypes.FollowupRun::originatingTo)
                                .filter(t -> t != null).findFirst().orElse(null);
                        String accountId = items.stream().map(QueueTypes.FollowupRun::originatingAccountId)
                                .filter(a -> a != null).findFirst().orElse(null);

                        QueueTypes.FollowupRun collectRun = new QueueTypes.FollowupRun(
                                prompt.toString(), null, null, System.currentTimeMillis(),
                                channel, to, accountId, null, null, run);
                        runFollowup.apply(collectRun).join();
                        continue;
                    }

                    // Non-collect modes: process summary or individual items
                    String summaryPrompt = buildQueueSummaryPrompt(queue);
                    if (summaryPrompt != null) {
                        QueueTypes.FollowupRunConfig run = queue.lastRun;
                        if (run == null)
                            break;
                        QueueTypes.FollowupRun summaryRun = new QueueTypes.FollowupRun(
                                summaryPrompt, null, null, System.currentTimeMillis(),
                                null, null, null, null, null, run);
                        runFollowup.apply(summaryRun).join();
                        continue;
                    }

                    QueueTypes.FollowupRun next = queue.items.isEmpty() ? null : queue.items.remove(0);
                    if (next == null)
                        break;
                    runFollowup.apply(next).join();
                }
            } catch (Exception e) {
                log.error("followup queue drain failed for {}: {}", key, String.valueOf(e));
            } finally {
                queue.draining = false;
                if (queue.items.isEmpty() && queue.droppedCount == 0) {
                    QueueState.FOLLOWUP_QUEUES.remove(key);
                } else {
                    // Re-schedule if still has items
                    scheduleFollowupDrain(key, runFollowup);
                }
            }
        });
    }

    /**
     * Build a summary prompt from dropped items, if any.
     */
    static String buildQueueSummaryPrompt(QueueState.FollowupQueueState queue) {
        if (queue.droppedCount <= 0 || queue.summaryLines.isEmpty())
            return null;
        int count = queue.droppedCount;
        String summaries = queue.summaryLines.stream()
                .map(s -> "- " + s)
                .collect(Collectors.joining("\n"));
        queue.droppedCount = 0;
        queue.summaryLines.clear();
        return String.format("[%d message(s) were dropped from the queue. Summaries:]\n%s", count, summaries);
    }
}

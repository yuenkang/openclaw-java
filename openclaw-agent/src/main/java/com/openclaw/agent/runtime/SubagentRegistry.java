package com.openclaw.agent.runtime;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * Registry for subagent (child agent) run lifecycle management.
 * Tracks spawned subagent runs with persistence, TTL sweeping, and cleanup
 * callbacks.
 * Corresponds to TypeScript's subagent-registry.ts + subagent-registry.store.ts
 * + subagent-announce.ts (core parts).
 */
@Slf4j
public class SubagentRegistry {

    private static final ObjectMapper JSON = new ObjectMapper();
    private static final long DEFAULT_ARCHIVE_AFTER_MS = 60 * 60_000; // 1 hour
    private static final long SWEEPER_INTERVAL_MS = 60_000;

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * Outcome of a subagent run.
     */
    public record SubagentRunOutcome(
            String status, // "ok" | "error" | "timeout" | "unknown"
            String error) {
        public static SubagentRunOutcome ok() {
            return new SubagentRunOutcome("ok", null);
        }

        public static SubagentRunOutcome error(String error) {
            return new SubagentRunOutcome("error", error);
        }

        public static SubagentRunOutcome timeout() {
            return new SubagentRunOutcome("timeout", null);
        }

        public static SubagentRunOutcome unknown() {
            return new SubagentRunOutcome("unknown", null);
        }
    }

    /**
     * A registered subagent run record.
     */
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SubagentRunRecord {
        private String runId;
        private String childSessionKey;
        private String requesterSessionKey;
        private String requesterDisplayKey;
        private String task;
        private String cleanup; // "delete" | "keep"
        private String label;
        private long createdAt;
        private Long startedAt;
        private Long endedAt;
        private SubagentRunOutcome outcome;
        private Long archiveAtMs;
        private Long cleanupCompletedAt;
        private Boolean cleanupHandled;
    }

    /**
     * Callback for handling subagent completion announcements.
     * Implementations typically call gateway RPC to notify the requester.
     */
    @FunctionalInterface
    public interface AnnounceCallback {
        void announce(SubagentRunRecord record);
    }

    // =========================================================================
    // Persisted format
    // =========================================================================

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    private static class PersistedRegistry {
        private int version = 2;
        private Map<String, SubagentRunRecord> runs = new LinkedHashMap<>();
    }

    // =========================================================================
    // State
    // =========================================================================

    private final Map<String, SubagentRunRecord> runs = new ConcurrentHashMap<>();
    private final String persistPath;
    private volatile AnnounceCallback announceCallback;
    private ScheduledFuture<?> sweeperFuture;
    private long archiveAfterMs = DEFAULT_ARCHIVE_AFTER_MS;

    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "subagent-sweeper");
        t.setDaemon(true);
        return t;
    });

    public SubagentRegistry(String persistPath) {
        this.persistPath = persistPath;
    }

    public SubagentRegistry(String persistPath, AnnounceCallback announceCallback) {
        this.persistPath = persistPath;
        this.announceCallback = announceCallback;
    }

    // =========================================================================
    // Registration
    // =========================================================================

    /**
     * Register a new subagent run.
     */
    public void registerRun(SubagentRunRecord record) {
        long now = System.currentTimeMillis();
        record.setCreatedAt(now);
        if (record.getStartedAt() == null)
            record.setStartedAt(now);
        if (record.getCleanupHandled() == null)
            record.setCleanupHandled(false);

        if (archiveAfterMs > 0) {
            record.setArchiveAtMs(now + archiveAfterMs);
        }

        runs.put(record.getRunId(), record);
        persist();
        startSweeper();
    }

    /**
     * Mark a run as completed.
     */
    public void markCompleted(String runId, SubagentRunOutcome outcome) {
        SubagentRunRecord record = runs.get(runId);
        if (record == null)
            return;

        record.setEndedAt(System.currentTimeMillis());
        record.setOutcome(outcome);
        persist();

        // Trigger announce callback
        if (beginCleanup(runId) && announceCallback != null) {
            try {
                announceCallback.announce(record);
            } catch (Exception e) {
                log.error("Subagent announce callback failed for {}: {}", runId, e.getMessage());
            }
            finalizeCleanup(runId, record.getCleanup());
        }
    }

    /**
     * Release (remove) a run.
     */
    public void releaseRun(String runId) {
        boolean removed = runs.remove(runId) != null;
        if (removed) {
            persist();
        }
        if (runs.isEmpty()) {
            stopSweeper();
        }
    }

    /**
     * List runs for a specific requester session.
     */
    public List<SubagentRunRecord> listRunsForRequester(String requesterSessionKey) {
        if (requesterSessionKey == null || requesterSessionKey.isBlank())
            return List.of();
        String key = requesterSessionKey.trim();
        return runs.values().stream()
                .filter(r -> key.equals(r.getRequesterSessionKey()))
                .collect(Collectors.toList());
    }

    /**
     * Get a run by ID.
     */
    public SubagentRunRecord getRun(String runId) {
        return runs.get(runId);
    }

    /**
     * Get all active runs.
     */
    public Collection<SubagentRunRecord> getAllRuns() {
        return Collections.unmodifiableCollection(runs.values());
    }

    // =========================================================================
    // Cleanup flow
    // =========================================================================

    private boolean beginCleanup(String runId) {
        SubagentRunRecord record = runs.get(runId);
        if (record == null)
            return false;
        if (record.getCleanupCompletedAt() != null)
            return false;
        if (Boolean.TRUE.equals(record.getCleanupHandled()))
            return false;
        record.setCleanupHandled(true);
        persist();
        return true;
    }

    private void finalizeCleanup(String runId, String cleanup) {
        SubagentRunRecord record = runs.get(runId);
        if (record == null)
            return;

        if ("delete".equals(cleanup)) {
            runs.remove(runId);
            persist();
            return;
        }
        record.setCleanupCompletedAt(System.currentTimeMillis());
        persist();
    }

    // =========================================================================
    // Persistence
    // =========================================================================

    private void persist() {
        if (persistPath == null)
            return;
        try {
            Path path = Path.of(persistPath);
            Files.createDirectories(path.getParent());
            PersistedRegistry registry = new PersistedRegistry();
            registry.setRuns(new LinkedHashMap<>(runs));
            String json = JSON.writerWithDefaultPrettyPrinter().writeValueAsString(registry);
            Files.writeString(path, json, StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        } catch (IOException e) {
            log.debug("Failed to persist subagent registry: {}", e.getMessage());
        }
    }

    /**
     * Restore runs from disk.
     */
    public void restoreFromDisk() {
        if (persistPath == null)
            return;
        Path path = Path.of(persistPath);
        if (!Files.isRegularFile(path))
            return;

        try {
            String content = Files.readString(path, StandardCharsets.UTF_8);
            PersistedRegistry registry = JSON.readValue(content, PersistedRegistry.class);
            if (registry == null || registry.getRuns() == null)
                return;

            for (Map.Entry<String, SubagentRunRecord> entry : registry.getRuns().entrySet()) {
                if (entry.getKey() == null || entry.getValue() == null)
                    continue;
                if (entry.getValue().getRunId() == null)
                    continue;
                runs.putIfAbsent(entry.getKey(), entry.getValue());
            }

            if (!runs.isEmpty()) {
                startSweeper();
            }
        } catch (IOException e) {
            log.debug("Failed to restore subagent registry: {}", e.getMessage());
        }
    }

    // =========================================================================
    // TTL sweeper
    // =========================================================================

    public void setArchiveAfterMs(long value) {
        this.archiveAfterMs = Math.max(60_000, value);
    }

    private synchronized void startSweeper() {
        if (sweeperFuture != null)
            return;
        sweeperFuture = scheduler.scheduleAtFixedRate(
                this::sweep, SWEEPER_INTERVAL_MS, SWEEPER_INTERVAL_MS, TimeUnit.MILLISECONDS);
    }

    private synchronized void stopSweeper() {
        if (sweeperFuture != null) {
            sweeperFuture.cancel(false);
            sweeperFuture = null;
        }
    }

    private void sweep() {
        long now = System.currentTimeMillis();
        boolean mutated = false;
        Iterator<Map.Entry<String, SubagentRunRecord>> it = runs.entrySet().iterator();
        while (it.hasNext()) {
            SubagentRunRecord record = it.next().getValue();
            if (record.getArchiveAtMs() != null && record.getArchiveAtMs() <= now) {
                it.remove();
                mutated = true;
            }
        }
        if (mutated)
            persist();
        if (runs.isEmpty())
            stopSweeper();
    }

    // =========================================================================
    // Subagent system prompt
    // =========================================================================

    /**
     * Build system prompt for a subagent.
     * Corresponds to TS buildSubagentSystemPrompt().
     */
    public static String buildSubagentSystemPrompt(
            String childSessionKey, String requesterSessionKey,
            String label, String task) {

        String taskText = (task != null && !task.isBlank())
                ? task.replaceAll("\\s+", " ").trim()
                : "{{TASK_DESCRIPTION}}";

        StringBuilder sb = new StringBuilder();
        sb.append("# Subagent Context\n\n");
        sb.append("You are a **subagent** spawned by the main agent for a specific task.\n\n");
        sb.append("## Your Role\n");
        sb.append("- You were created to handle: ").append(taskText).append("\n");
        sb.append("- Complete this task. That's your entire purpose.\n");
        sb.append("- You are NOT the main agent. Don't try to be.\n\n");
        sb.append("## Rules\n");
        sb.append("1. **Stay focused** - Do your assigned task, nothing else\n");
        sb.append("2. **Complete the task** - Your final message will be automatically reported to the main agent\n");
        sb.append("3. **Don't initiate** - No heartbeats, no proactive actions, no side quests\n");
        sb.append("4. **Be ephemeral** - You may be terminated after task completion. That's fine.\n\n");
        sb.append("## Output Format\n");
        sb.append("When complete, your final response should include:\n");
        sb.append("- What you accomplished or found\n");
        sb.append("- Any relevant details the main agent should know\n");
        sb.append("- Keep it concise but informative\n\n");
        sb.append("## What You DON'T Do\n");
        sb.append("- NO user conversations (that's main agent's job)\n");
        sb.append("- NO external messages unless explicitly tasked with a specific recipient/channel\n");
        sb.append("- NO cron jobs or persistent state\n");
        sb.append("- NO pretending to be the main agent\n");
        sb.append(
                "- Only use the `message` tool when explicitly instructed to contact a specific external recipient\n\n");
        sb.append("## Session Context\n");
        if (label != null && !label.isBlank()) {
            sb.append("- Label: ").append(label).append("\n");
        }
        if (requesterSessionKey != null && !requesterSessionKey.isBlank()) {
            sb.append("- Requester session: ").append(requesterSessionKey).append(".\n");
        }
        sb.append("- Your session: ").append(childSessionKey).append(".\n");

        return sb.toString();
    }

    // =========================================================================
    // Lifecycle
    // =========================================================================

    public void setAnnounceCallback(AnnounceCallback callback) {
        this.announceCallback = callback;
    }

    public void shutdown() {
        stopSweeper();
        scheduler.shutdown();
    }

    public void reset() {
        runs.clear();
        stopSweeper();
        persist();
    }
}

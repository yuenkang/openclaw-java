package com.openclaw.gateway.cron;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Consumer;

/**
 * Manages cron jobs: CRUD, persistence, scheduling, and execution.
 * Corresponds to TypeScript's cron/service.ts.
 */
@Slf4j
public class CronService {

    private final Map<String, CronJob> jobs = new ConcurrentHashMap<>();
    private final List<CronRunLog> runLogs = Collections.synchronizedList(new ArrayList<>());
    private final ScheduledExecutorService scheduler;
    private final Map<String, ScheduledFuture<?>> scheduledTasks = new ConcurrentHashMap<>();
    private final Path persistPath;
    private final ObjectMapper objectMapper;

    private Consumer<CronJob> jobExecutor;

    public CronService(Path dataDir) {
        this.persistPath = dataDir.resolve("cron-jobs.json");
        this.scheduler = Executors.newScheduledThreadPool(2, r -> {
            Thread t = new Thread(r, "cron-worker");
            t.setDaemon(true);
            return t;
        });
        this.objectMapper = new ObjectMapper()
                .registerModule(new JavaTimeModule())
                .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        loadFromDisk();
    }

    /**
     * Set the executor that runs a job (typically sends message to agent).
     */
    public void setJobExecutor(Consumer<CronJob> executor) {
        this.jobExecutor = executor;
    }

    // --- CRUD ---

    public CronJob addJob(CronJob job) {
        if (job.getId() == null) {
            job.setId(UUID.randomUUID().toString().substring(0, 8));
        }
        jobs.put(job.getId(), job);
        scheduleJob(job);
        persistToDisk();
        log.info("Added cron job: {} ({})", job.getName(), job.getSchedule());
        return job;
    }

    public Optional<CronJob> getJob(String id) {
        return Optional.ofNullable(jobs.get(id));
    }

    public List<CronJob> listJobs() {
        return new ArrayList<>(jobs.values());
    }

    public boolean removeJob(String id) {
        CronJob removed = jobs.remove(id);
        if (removed != null) {
            ScheduledFuture<?> task = scheduledTasks.remove(id);
            if (task != null)
                task.cancel(false);
            persistToDisk();
            log.info("Removed cron job: {}", id);
            return true;
        }
        return false;
    }

    public void enableJob(String id, boolean enabled) {
        CronJob job = jobs.get(id);
        if (job != null) {
            job.setEnabled(enabled);
            if (enabled) {
                scheduleJob(job);
            } else {
                ScheduledFuture<?> task = scheduledTasks.remove(id);
                if (task != null)
                    task.cancel(false);
            }
            persistToDisk();
        }
    }

    // --- Execution ---

    /**
     * Force-execute a job immediately.
     */
    public CronRunLog forceRun(String id) {
        CronJob job = jobs.get(id);
        if (job == null) {
            throw new IllegalArgumentException("Job not found: " + id);
        }
        return executeJob(job);
    }

    private CronRunLog executeJob(CronJob job) {
        Instant start = Instant.now();
        CronRunLog.CronRunLogBuilder logBuilder = CronRunLog.builder()
                .jobId(job.getId())
                .jobName(job.getName())
                .startedAt(start)
                .agentId(job.getAgentId())
                .message(job.getMessage());

        try {
            if (jobExecutor != null) {
                jobExecutor.accept(job);
            }
            Instant end = Instant.now();

            job.setLastRun(end);
            job.setLastRunStatus("success");
            job.setLastRunError(null);
            job.setRunCount(job.getRunCount() + 1);

            CronRunLog runLog = logBuilder
                    .finishedAt(end)
                    .durationMs(Duration.between(start, end).toMillis())
                    .success(true)
                    .build();

            runLogs.add(runLog);
            persistToDisk();
            log.info("Cron job {} executed successfully in {}ms", job.getName(), runLog.getDurationMs());
            return runLog;

        } catch (Exception e) {
            Instant end = Instant.now();
            job.setLastRun(end);
            job.setLastRunStatus("error");
            job.setLastRunError(e.getMessage());

            CronRunLog runLog = logBuilder
                    .finishedAt(end)
                    .durationMs(Duration.between(start, end).toMillis())
                    .success(false)
                    .error(e.getMessage())
                    .build();

            runLogs.add(runLog);
            persistToDisk();
            log.error("Cron job {} failed: {}", job.getName(), e.getMessage());
            return runLog;
        }
    }

    // --- Scheduling (simplified: fixed-rate based on cron expression) ---

    private void scheduleJob(CronJob job) {
        if (!job.isEnabled())
            return;

        // Cancel existing schedule
        ScheduledFuture<?> existing = scheduledTasks.remove(job.getId());
        if (existing != null)
            existing.cancel(false);

        // Parse simple interval from cron (simplified: extract hours from */N pattern)
        long intervalMinutes = parseCronInterval(job.getSchedule());

        ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(
                () -> executeJob(job),
                intervalMinutes,
                intervalMinutes,
                TimeUnit.MINUTES);
        scheduledTasks.put(job.getId(), future);
        log.debug("Scheduled job {} every {} minutes", job.getName(), intervalMinutes);
    }

    /**
     * Parse a simplified cron interval (minutes).
     * Supports: "* /N * * *" (every N hours) → N*60 minutes
     * "N * * * *" (at minute N every hour) → 60 minutes
     * Default: 60 minutes
     */
    long parseCronInterval(String cron) {
        if (cron == null)
            return 60;
        String[] parts = cron.trim().split("\\s+");
        if (parts.length < 2)
            return 60;

        // Check hour field for */N pattern
        if (parts.length >= 2 && parts[1].startsWith("*/")) {
            try {
                int hours = Integer.parseInt(parts[1].substring(2));
                return hours * 60L;
            } catch (NumberFormatException e) {
                /* fallthrough */ }
        }

        // Check minute field for */N pattern
        if (parts[0].startsWith("*/")) {
            try {
                return Long.parseLong(parts[0].substring(2));
            } catch (NumberFormatException e) {
                /* fallthrough */ }
        }

        return 60; // default: hourly
    }

    // --- Persistence ---

    private void persistToDisk() {
        try {
            Files.createDirectories(persistPath.getParent());
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(persistPath.toFile(), jobs.values());
        } catch (IOException e) {
            log.error("Failed to persist cron jobs: {}", e.getMessage());
        }
    }

    private void loadFromDisk() {
        if (!Files.exists(persistPath))
            return;
        try {
            List<CronJob> loaded = objectMapper.readValue(
                    persistPath.toFile(), new TypeReference<List<CronJob>>() {
                    });
            for (CronJob job : loaded) {
                jobs.put(job.getId(), job);
            }
            log.info("Loaded {} cron jobs from disk", loaded.size());
        } catch (IOException e) {
            log.error("Failed to load cron jobs: {}", e.getMessage());
        }
    }

    public List<CronRunLog> getRunLogs(String jobId, int limit) {
        return runLogs.stream()
                .filter(l -> jobId == null || jobId.equals(l.getJobId()))
                .sorted(Comparator.comparing(CronRunLog::getStartedAt).reversed())
                .limit(limit)
                .toList();
    }

    /**
     * Start scheduling all enabled jobs.
     */
    public void start() {
        for (CronJob job : jobs.values()) {
            if (job.isEnabled()) {
                scheduleJob(job);
            }
        }
        log.info("Cron service started with {} jobs ({} enabled)",
                jobs.size(),
                jobs.values().stream().filter(CronJob::isEnabled).count());
    }

    /**
     * Stop all scheduled jobs and shut down the scheduler.
     */
    public void stop() {
        for (ScheduledFuture<?> task : scheduledTasks.values()) {
            task.cancel(false);
        }
        scheduledTasks.clear();
        shutdown();
    }

    public void shutdown() {
        scheduler.shutdown();
    }
}

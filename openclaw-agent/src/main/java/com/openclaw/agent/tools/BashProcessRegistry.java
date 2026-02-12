package com.openclaw.agent.tools;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Registry for background bash/exec process sessions.
 * Tracks running and finished processes with output buffering, TTL cleanup.
 * Corresponds to TypeScript's bash-process-registry.ts.
 */
@Slf4j
public class BashProcessRegistry {

    private static final long DEFAULT_JOB_TTL_MS = 30 * 60 * 1000; // 30 min
    private static final long MIN_JOB_TTL_MS = 60 * 1000;
    private static final long MAX_JOB_TTL_MS = 3 * 60 * 60 * 1000; // 3 hours
    private static final int DEFAULT_MAX_OUTPUT_CHARS = 50_000;
    private static final int DEFAULT_TAIL_CHARS = 2000;

    private final Map<String, ProcessSession> runningSessions = new ConcurrentHashMap<>();
    private final Map<String, FinishedSession> finishedSessions = new ConcurrentHashMap<>();
    private volatile long jobTtlMs;
    private ScheduledFuture<?> sweeperFuture;
    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "bash-process-sweeper");
        t.setDaemon(true);
        return t;
    });

    public BashProcessRegistry() {
        this(DEFAULT_JOB_TTL_MS);
    }

    public BashProcessRegistry(long jobTtlMs) {
        this.jobTtlMs = clampTtl(jobTtlMs);
    }

    // =========================================================================
    // Process status
    // =========================================================================

    public enum ProcessStatus {
        RUNNING, COMPLETED, FAILED, KILLED
    }

    // =========================================================================
    // Process session
    // =========================================================================

    public static class ProcessSession {
        private final String id;
        private final String command;
        private final long startedAt;
        private String scopeKey;
        private String sessionKey;
        private String cwd;
        private Process process;
        private int maxOutputChars;

        // Output buffers
        private final StringBuilder aggregated = new StringBuilder();
        private int totalOutputChars;
        private boolean truncated;
        private boolean backgrounded;

        // Exit state
        private volatile boolean exited;
        private Integer exitCode;
        private String exitSignal;

        public ProcessSession(String id, String command) {
            this.id = id;
            this.command = command;
            this.startedAt = System.currentTimeMillis();
            this.maxOutputChars = DEFAULT_MAX_OUTPUT_CHARS;
        }

        public String getId() {
            return id;
        }

        public String getCommand() {
            return command;
        }

        public long getStartedAt() {
            return startedAt;
        }

        public String getScopeKey() {
            return scopeKey;
        }

        public void setScopeKey(String scopeKey) {
            this.scopeKey = scopeKey;
        }

        public String getSessionKey() {
            return sessionKey;
        }

        public void setSessionKey(String sessionKey) {
            this.sessionKey = sessionKey;
        }

        public String getCwd() {
            return cwd;
        }

        public void setCwd(String cwd) {
            this.cwd = cwd;
        }

        public Process getProcess() {
            return process;
        }

        public void setProcess(Process process) {
            this.process = process;
        }

        public int getMaxOutputChars() {
            return maxOutputChars;
        }

        public void setMaxOutputChars(int maxOutputChars) {
            this.maxOutputChars = maxOutputChars;
        }

        public boolean isExited() {
            return exited;
        }

        public Integer getExitCode() {
            return exitCode;
        }

        public String getExitSignal() {
            return exitSignal;
        }

        public boolean isBackgrounded() {
            return backgrounded;
        }

        public int getTotalOutputChars() {
            return totalOutputChars;
        }

        public boolean isTruncated() {
            return truncated;
        }

        public String getAggregated() {
            synchronized (aggregated) {
                return aggregated.toString();
            }
        }

        public String getTail() {
            synchronized (aggregated) {
                return tail(aggregated.toString(), DEFAULT_TAIL_CHARS);
            }
        }
    }

    // =========================================================================
    // Finished session (immutable snapshot)
    // =========================================================================

    public record FinishedSession(
            String id,
            String command,
            String scopeKey,
            long startedAt,
            long endedAt,
            String cwd,
            ProcessStatus status,
            Integer exitCode,
            String exitSignal,
            String aggregated,
            String tail,
            boolean truncated,
            int totalOutputChars) {
    }

    // =========================================================================
    // Registry operations
    // =========================================================================

    public void addSession(ProcessSession session) {
        runningSessions.put(session.getId(), session);
        startSweeper();
    }

    public ProcessSession getSession(String id) {
        return runningSessions.get(id);
    }

    public FinishedSession getFinishedSession(String id) {
        return finishedSessions.get(id);
    }

    public void deleteSession(String id) {
        runningSessions.remove(id);
        finishedSessions.remove(id);
    }

    /**
     * Append output to a session's buffer, applying cap/truncation.
     */
    public void appendOutput(ProcessSession session, String chunk) {
        if (chunk == null || chunk.isEmpty())
            return;
        synchronized (session.aggregated) {
            session.totalOutputChars += chunk.length();
            session.aggregated.append(chunk);

            // Cap aggregated output
            if (session.aggregated.length() > session.maxOutputChars) {
                session.truncated = true;
                int excess = session.aggregated.length() - session.maxOutputChars;
                session.aggregated.delete(0, excess);
            }
        }
    }

    /**
     * Mark the session as exited and move to finished.
     */
    public void markExited(ProcessSession session, int exitCode, ProcessStatus status) {
        session.exited = true;
        session.exitCode = exitCode;
        moveToFinished(session, status);
    }

    /**
     * Mark the session as backgrounded.
     */
    public void markBackgrounded(ProcessSession session) {
        session.backgrounded = true;
    }

    private void moveToFinished(ProcessSession session, ProcessStatus status) {
        runningSessions.remove(session.getId());
        if (!session.backgrounded)
            return;

        finishedSessions.put(session.getId(), new FinishedSession(
                session.getId(),
                session.getCommand(),
                session.getScopeKey(),
                session.getStartedAt(),
                System.currentTimeMillis(),
                session.getCwd(),
                status,
                session.getExitCode(),
                session.getExitSignal(),
                session.getAggregated(),
                session.getTail(),
                session.isTruncated(),
                session.getTotalOutputChars()));
    }

    // =========================================================================
    // Listing
    // =========================================================================

    public List<ProcessSession> listRunningSessions() {
        return runningSessions.values().stream()
                .filter(ProcessSession::isBackgrounded)
                .toList();
    }

    public List<FinishedSession> listFinishedSessions() {
        return new ArrayList<>(finishedSessions.values());
    }

    public void clearFinished() {
        finishedSessions.clear();
    }

    // =========================================================================
    // TTL sweeper
    // =========================================================================

    public void setJobTtlMs(long value) {
        this.jobTtlMs = clampTtl(value);
        stopSweeper();
        startSweeper();
    }

    private synchronized void startSweeper() {
        if (sweeperFuture != null)
            return;
        long interval = Math.max(30_000, jobTtlMs / 6);
        sweeperFuture = scheduler.scheduleAtFixedRate(
                this::pruneFinishedSessions,
                interval, interval, TimeUnit.MILLISECONDS);
    }

    private synchronized void stopSweeper() {
        if (sweeperFuture != null) {
            sweeperFuture.cancel(false);
            sweeperFuture = null;
        }
    }

    private void pruneFinishedSessions() {
        long cutoff = System.currentTimeMillis() - jobTtlMs;
        finishedSessions.entrySet().removeIf(e -> e.getValue().endedAt() < cutoff);
    }

    /**
     * Shutdown the sweeper — call on application shutdown.
     */
    public void shutdown() {
        stopSweeper();
        scheduler.shutdown();
    }

    /**
     * Reset for testing.
     */
    public void reset() {
        runningSessions.clear();
        finishedSessions.clear();
        stopSweeper();
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static long clampTtl(long value) {
        if (value <= 0)
            return DEFAULT_JOB_TTL_MS;
        return Math.min(Math.max(value, MIN_JOB_TTL_MS), MAX_JOB_TTL_MS);
    }

    static String tail(String text, int max) {
        if (text == null || text.length() <= max)
            return text;
        return text.substring(text.length() - max);
    }
}

package com.openclaw.agent.session;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * File-based session write lock with PID tracking and stale detection.
 * Prevents concurrent writes to the same session file.
 * Corresponds to TypeScript's session-write-lock.ts.
 */
@Slf4j
public class SessionWriteLock {

    private static final long DEFAULT_TIMEOUT_MS = 10_000;
    private static final long DEFAULT_STALE_MS = 30 * 60 * 1000; // 30 minutes
    private static final long POLL_BASE_MS = 50;
    private static final long POLL_MAX_MS = 1000;

    private static final Map<String, HeldLock> HELD_LOCKS = new ConcurrentHashMap<>();
    private static volatile boolean shutdownHookRegistered = false;

    /**
     * A held lock with reference counting for re-entrant acquisition.
     */
    public static class HeldLock implements AutoCloseable {
        private final String normalizedPath;
        private final Path lockPath;
        private final RandomAccessFile raf;
        private final FileLock fileLock;
        private int count;

        HeldLock(String normalizedPath, Path lockPath,
                RandomAccessFile raf, FileLock fileLock) {
            this.normalizedPath = normalizedPath;
            this.lockPath = lockPath;
            this.raf = raf;
            this.fileLock = fileLock;
            this.count = 1;
        }

        /**
         * Release one reference. When count hits 0, actually release the lock.
         */
        public void release() {
            synchronized (this) {
                count--;
                if (count > 0)
                    return;
            }
            HELD_LOCKS.remove(normalizedPath);
            try {
                fileLock.release();
            } catch (IOException e) {
                log.debug("Error releasing file lock: {}", e.getMessage());
            }
            try {
                raf.close();
            } catch (IOException e) {
                log.debug("Error closing lock RAF: {}", e.getMessage());
            }
            try {
                Files.deleteIfExists(lockPath);
            } catch (IOException e) {
                log.debug("Error deleting lock file: {}", e.getMessage());
            }
        }

        @Override
        public void close() {
            release();
        }

        synchronized void incrementCount() {
            count++;
        }
    }

    /**
     * Acquire a write lock for the given session file.
     *
     * @param sessionFile path to the session file
     * @param timeoutMs   timeout in milliseconds (default 10s)
     * @param staleMs     stale threshold in milliseconds (default 30min)
     * @return a HeldLock that must be released when done
     * @throws IOException          on I/O failure
     * @throws InterruptedException if the thread is interrupted while waiting
     */
    public static HeldLock acquireLock(String sessionFile, long timeoutMs, long staleMs)
            throws IOException, InterruptedException {
        registerShutdownHook();

        Path sessionPath = Path.of(sessionFile).toAbsolutePath().normalize();
        String normalizedKey = sessionPath.toString();
        Path lockPath = Path.of(normalizedKey + ".lock");

        // Re-entrant: if we already hold this lock, bump the count
        HeldLock existing = HELD_LOCKS.get(normalizedKey);
        if (existing != null) {
            existing.incrementCount();
            return existing;
        }

        // Ensure parent directory exists
        Files.createDirectories(lockPath.getParent());

        long startedAt = System.currentTimeMillis();
        int attempt = 0;

        while (System.currentTimeMillis() - startedAt < timeoutMs) {
            attempt++;
            try {
                RandomAccessFile raf = new RandomAccessFile(lockPath.toFile(), "rw");
                try {
                    FileLock lock = raf.getChannel().tryLock();
                    if (lock != null) {
                        // Write PID payload
                        String payload = String.format(
                                "{\"pid\":%d,\"createdAt\":\"%s\"}",
                                ProcessHandle.current().pid(),
                                Instant.now().toString());
                        raf.setLength(0);
                        raf.write(payload.getBytes(StandardCharsets.UTF_8));

                        HeldLock held = new HeldLock(normalizedKey, lockPath, raf, lock);
                        HELD_LOCKS.put(normalizedKey, held);
                        return held;
                    } else {
                        raf.close();
                    }
                } catch (OverlappingFileLockException e) {
                    raf.close();
                }
            } catch (IOException e) {
                // Lock file in use, check staleness
            }

            // Check if existing lock is stale
            if (isLockStale(lockPath, staleMs)) {
                log.info("Removing stale lock file: {}", lockPath);
                try {
                    Files.deleteIfExists(lockPath);
                } catch (IOException e) {
                    log.debug("Failed to remove stale lock: {}", e.getMessage());
                }
                continue;
            }

            long delay = Math.min(POLL_MAX_MS, POLL_BASE_MS * attempt);
            Thread.sleep(delay);
        }

        // Timeout
        String owner = readLockOwner(lockPath);
        throw new IOException(String.format(
                "Session file locked (timeout %dms): %s %s", timeoutMs, owner, lockPath));
    }

    /**
     * Acquire with default timeout and stale settings.
     */
    public static HeldLock acquireLock(String sessionFile)
            throws IOException, InterruptedException {
        return acquireLock(sessionFile, DEFAULT_TIMEOUT_MS, DEFAULT_STALE_MS);
    }

    /**
     * Release all held locks synchronously. Called during JVM shutdown.
     */
    static void releaseAllLocks() {
        for (var entry : HELD_LOCKS.entrySet()) {
            try {
                HeldLock held = entry.getValue();
                held.fileLock.release();
                held.raf.close();
                Files.deleteIfExists(held.lockPath);
            } catch (Exception e) {
                // Best effort during shutdown
            }
        }
        HELD_LOCKS.clear();
    }

    // =========================================================================
    // Private helpers
    // =========================================================================

    private static void registerShutdownHook() {
        if (shutdownHookRegistered)
            return;
        synchronized (SessionWriteLock.class) {
            if (shutdownHookRegistered)
                return;
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                releaseAllLocks();
            }, "session-lock-cleanup"));
            shutdownHookRegistered = true;
        }
    }

    private static boolean isLockStale(Path lockPath, long staleMs) {
        try {
            if (!Files.exists(lockPath))
                return false;
            String content = Files.readString(lockPath, StandardCharsets.UTF_8);

            // Parse createdAt
            int idx = content.indexOf("\"createdAt\"");
            if (idx < 0)
                return true; // Malformed → treat as stale
            int colonIdx = content.indexOf(':', idx);
            int quoteStart = content.indexOf('"', colonIdx + 1);
            int quoteEnd = content.indexOf('"', quoteStart + 1);
            if (quoteStart < 0 || quoteEnd < 0)
                return true;
            String createdAt = content.substring(quoteStart + 1, quoteEnd);
            Instant created = Instant.parse(createdAt);
            if (Instant.now().toEpochMilli() - created.toEpochMilli() > staleMs) {
                return true;
            }

            // Parse PID and check if alive
            int pidIdx = content.indexOf("\"pid\"");
            if (pidIdx >= 0) {
                int pidColon = content.indexOf(':', pidIdx);
                String pidStr = content.substring(pidColon + 1)
                        .replaceAll("[^0-9]", " ").trim().split("\\s+")[0];
                long pid = Long.parseLong(pidStr);
                return !ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(false);
            }

            return false;
        } catch (Exception e) {
            return true; // Can't read → treat as stale
        }
    }

    private static String readLockOwner(Path lockPath) {
        try {
            String content = Files.readString(lockPath, StandardCharsets.UTF_8);
            int pidIdx = content.indexOf("\"pid\"");
            if (pidIdx >= 0) {
                int colonIdx = content.indexOf(':', pidIdx);
                String pidStr = content.substring(colonIdx + 1)
                        .replaceAll("[^0-9]", " ").trim().split("\\s+")[0];
                return "pid=" + pidStr;
            }
        } catch (Exception e) {
            // Ignore
        }
        return "unknown";
    }
}

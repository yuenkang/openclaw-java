package com.openclaw.gateway.runtime;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.Map;

/**
 * Gateway lock — prevents multiple gateway instances from running
 * simultaneously.
 * Uses file-based locking with PID recording and stale lock detection.
 * Corresponds to TypeScript's infra/gateway-lock.ts.
 */
public class GatewayLock implements AutoCloseable {

    private static final Logger log = LoggerFactory.getLogger(GatewayLock.class);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final long DEFAULT_TIMEOUT_MS = 5_000;
    private static final long DEFAULT_POLL_INTERVAL_MS = 100;
    private static final long DEFAULT_STALE_MS = 30_000;

    // ── Error ───────────────────────────────────────────────────────────

    public static class GatewayLockError extends RuntimeException {
        public GatewayLockError(String message) {
            super(message);
        }

        public GatewayLockError(String message, Throwable cause) {
            super(message, cause);
        }
    }

    // ── Handle ──────────────────────────────────────────────────────────

    private final Path lockPath;
    private final FileChannel channel;
    private final FileLock lock;

    private GatewayLock(Path lockPath, FileChannel channel, FileLock lock) {
        this.lockPath = lockPath;
        this.channel = channel;
        this.lock = lock;
    }

    // ── Acquire ─────────────────────────────────────────────────────────

    /**
     * Acquire the gateway lock. Blocks up to {@code timeoutMs} if another instance
     * holds it.
     *
     * @param lockDir   directory to store lock files
     * @param lockName  lock file name (e.g. "gateway.lock")
     * @param timeoutMs max time to wait (default 5000ms)
     * @return the lock handle
     * @throws GatewayLockError if unable to acquire within timeout
     */
    public static GatewayLock acquire(Path lockDir, String lockName, long timeoutMs) {
        Path lockPath = lockDir.resolve(lockName != null ? lockName : "gateway.lock");
        try {
            Files.createDirectories(lockDir);
        } catch (IOException e) {
            throw new GatewayLockError("Cannot create lock directory: " + lockDir, e);
        }

        long startedAt = System.currentTimeMillis();
        long effectiveTimeout = timeoutMs > 0 ? timeoutMs : DEFAULT_TIMEOUT_MS;

        while (System.currentTimeMillis() - startedAt < effectiveTimeout) {
            try {
                FileChannel fc = FileChannel.open(lockPath,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.WRITE);
                FileLock fl = fc.tryLock();
                if (fl != null) {
                    // Write lock payload
                    long pid = ProcessHandle.current().pid();
                    String payload = MAPPER.writeValueAsString(Map.of(
                            "pid", pid,
                            "createdAt", Instant.now().toString(),
                            "javaVersion", System.getProperty("java.version", "unknown")));
                    fc.truncate(0);
                    fc.write(java.nio.ByteBuffer.wrap(payload.getBytes()));
                    fc.force(true);
                    log.info("Gateway lock acquired at {} (pid={})", lockPath, pid);
                    return new GatewayLock(lockPath, fc, fl);
                }
                fc.close();

                // Check if existing lock is stale
                if (isLockStale(lockPath, DEFAULT_STALE_MS)) {
                    log.warn("Stale lock detected at {}, removing", lockPath);
                    Files.deleteIfExists(lockPath);
                    continue;
                }

                Thread.sleep(DEFAULT_POLL_INTERVAL_MS);
            } catch (IOException | InterruptedException e) {
                if (e instanceof InterruptedException) {
                    Thread.currentThread().interrupt();
                    throw new GatewayLockError("Interrupted while acquiring gateway lock", e);
                }
                // retry
                try {
                    Thread.sleep(DEFAULT_POLL_INTERVAL_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new GatewayLockError("Interrupted", ie);
                }
            }
        }

        // Read existing lock for error message
        String ownerInfo = "";
        try {
            String content = Files.readString(lockPath);
            var map = MAPPER.readValue(content, Map.class);
            ownerInfo = " (pid " + map.get("pid") + ")";
        } catch (Exception ignored) {
        }

        throw new GatewayLockError(
                "Gateway already running" + ownerInfo + "; lock timeout after " + effectiveTimeout + "ms");
    }

    /**
     * Acquire with default timeout.
     */
    public static GatewayLock acquire(Path lockDir) {
        return acquire(lockDir, null, DEFAULT_TIMEOUT_MS);
    }

    // ── Release ─────────────────────────────────────────────────────────

    /**
     * Release the gateway lock.
     */
    public void release() {
        try {
            if (lock != null && lock.isValid()) {
                lock.release();
            }
        } catch (IOException e) {
            log.warn("Error releasing lock: {}", e.getMessage());
        }
        try {
            if (channel != null && channel.isOpen()) {
                channel.close();
            }
        } catch (IOException e) {
            log.warn("Error closing lock channel: {}", e.getMessage());
        }
        try {
            Files.deleteIfExists(lockPath);
        } catch (IOException e) {
            log.warn("Error deleting lock file: {}", e.getMessage());
        }
        log.info("Gateway lock released at {}", lockPath);
    }

    @Override
    public void close() {
        release();
    }

    // ── Stale detection ─────────────────────────────────────────────────

    private static boolean isLockStale(Path lockPath, long staleMs) {
        try {
            if (!Files.exists(lockPath))
                return true;

            // Check PID
            String content = Files.readString(lockPath);
            var map = MAPPER.readValue(content, Map.class);
            Object pidObj = map.get("pid");
            if (pidObj instanceof Number pid) {
                try {
                    ProcessHandle ph = ProcessHandle.of(pid.longValue()).orElse(null);
                    if (ph == null || !ph.isAlive()) {
                        return true; // process is dead
                    }
                } catch (Exception e) {
                    // can't check — fall through to mtime check
                }
            }

            // Fallback: mtime check
            long mtime = Files.getLastModifiedTime(lockPath).toMillis();
            return System.currentTimeMillis() - mtime > staleMs;
        } catch (Exception e) {
            return true; // if we can't read, treat as stale
        }
    }
}

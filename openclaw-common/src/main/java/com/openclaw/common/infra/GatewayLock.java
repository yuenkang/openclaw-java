package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.io.Closeable;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Instant;

/**
 * Gateway lock — ensures only one gateway instance runs per config.
 * Uses OS-level file locking via {@link FileLock}.
 * <p>
 * Corresponds to TypeScript's {@code infra/gateway-lock.ts}.
 */
@Slf4j
public class GatewayLock {

    private static final long DEFAULT_TIMEOUT_MS = 5000;
    private static final long DEFAULT_POLL_INTERVAL_MS = 100;

    /**
     * Handle to a held gateway lock. Call {@link #release()} when done.
     */
    public static class LockHandle implements Closeable {
        private final Path lockPath;
        private final FileChannel channel;
        private final FileLock lock;
        private volatile boolean released = false;

        LockHandle(Path lockPath, FileChannel channel, FileLock lock) {
            this.lockPath = lockPath;
            this.channel = channel;
            this.lock = lock;
        }

        public Path getLockPath() {
            return lockPath;
        }

        public void release() {
            if (released)
                return;
            released = true;
            try {
                lock.release();
            } catch (IOException e) {
                log.debug("Failed to release lock: {}", e.getMessage());
            }
            try {
                channel.close();
            } catch (IOException e) {
                log.debug("Failed to close lock channel: {}", e.getMessage());
            }
            try {
                Files.deleteIfExists(lockPath);
            } catch (IOException e) {
                log.debug("Failed to delete lock file: {}", e.getMessage());
            }
        }

        @Override
        public void close() {
            release();
        }
    }

    /**
     * Thrown when a gateway lock cannot be acquired.
     */
    public static class GatewayLockError extends RuntimeException {
        public GatewayLockError(String message) {
            super(message);
        }

        public GatewayLockError(String message, Throwable cause) {
            super(message, cause);
        }
    }

    /**
     * Attempt to acquire the gateway lock.
     *
     * @param lockDir  directory for lock files
     * @param configId unique identifier for this config (e.g., config file hash)
     * @return lock handle, or null if locking is disabled
     * @throws GatewayLockError if the lock cannot be acquired within the timeout
     */
    public static LockHandle acquire(Path lockDir, String configId) {
        return acquire(lockDir, configId, DEFAULT_TIMEOUT_MS, DEFAULT_POLL_INTERVAL_MS);
    }

    /**
     * Attempt to acquire the gateway lock with custom timeouts.
     */
    public static LockHandle acquire(Path lockDir, String configId, long timeoutMs, long pollIntervalMs) {
        Path lockPath = lockDir.resolve("gateway." + configId + ".lock");

        try {
            Files.createDirectories(lockDir);
        } catch (IOException e) {
            throw new GatewayLockError("Failed to create lock directory: " + lockDir, e);
        }

        long startedAt = System.currentTimeMillis();

        while (System.currentTimeMillis() - startedAt < timeoutMs) {
            try {
                FileChannel channel = FileChannel.open(lockPath,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.WRITE);

                FileLock lock = channel.tryLock();
                if (lock != null) {
                    // Write lock payload
                    String payload = String.format(
                            "{\"pid\":%d,\"createdAt\":\"%s\",\"configId\":\"%s\"}",
                            ProcessHandle.current().pid(),
                            Instant.now().toString(),
                            configId);
                    channel.write(java.nio.ByteBuffer.wrap(payload.getBytes(StandardCharsets.UTF_8)));
                    channel.force(true);

                    log.info("Acquired gateway lock: {}", lockPath);
                    return new LockHandle(lockPath, channel, lock);
                }

                // Lock held by another process
                channel.close();
            } catch (IOException e) {
                log.debug("Lock attempt failed: {}", e.getMessage());
            }

            try {
                Thread.sleep(pollIntervalMs);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new GatewayLockError("Lock acquisition interrupted");
            }
        }

        throw new GatewayLockError(
                "Gateway already running; lock timeout after " + timeoutMs + "ms at: " + lockPath);
    }
}

package com.openclaw.gateway.runtime;

import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Channel lifecycle manager — starts/stops channels via gateway.
 * Corresponds to TS {@code gateway/server-channels.ts}.
 *
 * <p>
 * Manages channel runtime state: online/offline status, error tracking,
 * and multi-account support.
 */
@Slf4j
public class GatewayChannelsBridge {

    /** Channel IDs (e.g., "telegram", "discord", "slack"). */
    public enum ChannelId {
        TELEGRAM, DISCORD, SLACK, SIGNAL, WHATSAPP, IMESSAGE, LINE, WEB
    }

    /** Channel account runtime status. */
    public enum ChannelStatus {
        STOPPED, STARTING, RUNNING, ERROR, LOGGED_OUT
    }

    /** Snapshot of a single channel account's runtime state. */
    public record ChannelAccountSnapshot(
            ChannelId channelId,
            String accountId,
            ChannelStatus status,
            String error,
            long startedAtMs,
            long stoppedAtMs) {
        public ChannelAccountSnapshot withStatus(ChannelStatus newStatus) {
            return new ChannelAccountSnapshot(channelId, accountId, newStatus,
                    error, startedAtMs, stoppedAtMs);
        }

        public ChannelAccountSnapshot withError(String err) {
            return new ChannelAccountSnapshot(channelId, accountId, ChannelStatus.ERROR,
                    err, startedAtMs, System.currentTimeMillis());
        }
    }

    // ---- Runtime store per channel ----

    private final Map<ChannelId, Map<String, ChannelAccountSnapshot>> runtimes = new ConcurrentHashMap<>();

    private final Map<ChannelId, List<ChannelLifecycleListener>> lifecycleListeners = new ConcurrentHashMap<>();

    /** Default account ID when multi-account is not configured. */
    public static final String DEFAULT_ACCOUNT_ID = "default";

    /**
     * Register a lifecycle listener for a channel.
     */
    public void addLifecycleListener(ChannelId channel, ChannelLifecycleListener listener) {
        lifecycleListeners.computeIfAbsent(channel, k -> new CopyOnWriteArrayList<>())
                .add(listener);
    }

    /**
     * Start a channel (default account).
     */
    public void startChannel(ChannelId channel) {
        startChannel(channel, DEFAULT_ACCOUNT_ID);
    }

    /**
     * Start a specific channel account.
     */
    public void startChannel(ChannelId channel, String accountId) {
        log.info("channel start: {} (account={})", channel, accountId);

        var snapshot = new ChannelAccountSnapshot(
                channel, accountId, ChannelStatus.STARTING,
                null, System.currentTimeMillis(), 0);
        setRuntime(channel, accountId, snapshot);

        try {
            // Notify lifecycle listeners
            var listeners = lifecycleListeners.get(channel);
            if (listeners != null) {
                for (var listener : listeners) {
                    listener.onStart(channel, accountId);
                }
            }
            setRuntime(channel, accountId, snapshot.withStatus(ChannelStatus.RUNNING));
            log.info("channel started: {} (account={})", channel, accountId);
        } catch (Exception e) {
            log.error("channel start failed: {} (account={}): {}",
                    channel, accountId, e.getMessage());
            setRuntime(channel, accountId, snapshot.withError(e.getMessage()));
        }
    }

    /**
     * Stop a channel (default account).
     */
    public void stopChannel(ChannelId channel) {
        stopChannel(channel, DEFAULT_ACCOUNT_ID);
    }

    /**
     * Stop a specific channel account.
     */
    public void stopChannel(ChannelId channel, String accountId) {
        log.info("channel stop: {} (account={})", channel, accountId);

        var current = getRuntime(channel, accountId);
        if (current == null || current.status() == ChannelStatus.STOPPED) {
            return;
        }

        try {
            var listeners = lifecycleListeners.get(channel);
            if (listeners != null) {
                for (var listener : listeners) {
                    listener.onStop(channel, accountId);
                }
            }
        } catch (Exception e) {
            log.warn("channel stop error: {} (account={}): {}",
                    channel, accountId, e.getMessage());
        }

        setRuntime(channel, accountId, new ChannelAccountSnapshot(
                channel, accountId, ChannelStatus.STOPPED,
                null, current.startedAtMs(), System.currentTimeMillis()));
    }

    /**
     * Start all enabled channels.
     */
    public void startAllChannels(Set<ChannelId> enabledChannels) {
        for (ChannelId ch : enabledChannels) {
            startChannel(ch);
        }
    }

    /**
     * Mark a channel as logged out.
     */
    public void markLoggedOut(ChannelId channel, String accountId) {
        var current = getRuntime(channel, accountId != null ? accountId : DEFAULT_ACCOUNT_ID);
        if (current != null) {
            setRuntime(channel, current.accountId(),
                    current.withStatus(ChannelStatus.LOGGED_OUT));
        }
    }

    /**
     * Get runtime snapshot for a channel account.
     */
    public ChannelAccountSnapshot getRuntime(ChannelId channel, String accountId) {
        var channelMap = runtimes.get(channel);
        return channelMap != null ? channelMap.get(accountId) : null;
    }

    /**
     * Get all runtime snapshots.
     */
    public Map<ChannelId, Map<String, ChannelAccountSnapshot>> getAllRuntimes() {
        return Map.copyOf(runtimes);
    }

    private void setRuntime(ChannelId channel, String accountId,
            ChannelAccountSnapshot snapshot) {
        runtimes.computeIfAbsent(channel, k -> new ConcurrentHashMap<>())
                .put(accountId, snapshot);
    }

    /**
     * Listener interface for channel lifecycle events.
     */
    public interface ChannelLifecycleListener {
        void onStart(ChannelId channel, String accountId) throws Exception;

        void onStop(ChannelId channel, String accountId) throws Exception;
    }
}

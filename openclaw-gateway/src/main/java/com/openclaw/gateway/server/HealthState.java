package com.openclaw.gateway.server;

import com.openclaw.gateway.protocol.ProtocolTypes;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Gateway health state cache and versioning.
 * <p>
 * Mirrors the state management from {@code server/health-state.ts}.
 * In TypeScript these are module-level singletons; here we use
 * static fields with atomic access for thread-safety.
 */
public final class HealthState {

    private HealthState() {
    }

    private static final AtomicInteger presenceVersion = new AtomicInteger(1);
    private static final AtomicInteger healthVersion = new AtomicInteger(1);
    private static final AtomicReference<Object> healthCache = new AtomicReference<>(null);
    private static volatile Consumer<Object> broadcastHealthUpdate = null;

    // ── Presence version ────────────────────────────────────────

    public static int getPresenceVersion() {
        return presenceVersion.get();
    }

    public static int incrementPresenceVersion() {
        return presenceVersion.incrementAndGet();
    }

    // ── Health version + cache ───────────────────────────────────

    public static int getHealthVersion() {
        return healthVersion.get();
    }

    public static Object getHealthCache() {
        return healthCache.get();
    }

    /**
     * Build a gateway snapshot (presence, health, session defaults).
     * The actual snapshot type is defined in {@link ProtocolTypes.Snapshot}.
     * <p>
     * In TypeScript this calls loadConfig, resolveDefaultAgentId, etc.
     * In Java the caller is expected to provide dependencies.
     */
    public static ProtocolTypes.Snapshot buildGatewaySnapshot(SnapshotDependencies deps) {
        ProtocolTypes.Snapshot snapshot = new ProtocolTypes.Snapshot();
        snapshot.setPresence(deps.getPresenceList());
        snapshot.setHealth(deps.getHealthSummary());
        ProtocolTypes.StateVersion sv = new ProtocolTypes.StateVersion();
        sv.setPresence(presenceVersion.get());
        sv.setHealth(healthVersion.get());
        snapshot.setStateVersion(sv);
        snapshot.setUptimeMs(deps.getUptimeMs());
        snapshot.setConfigPath(deps.getConfigPath());
        snapshot.setStateDir(deps.getStateDir());
        return snapshot;
    }

    public static void setBroadcastHealthUpdate(Consumer<Object> fn) {
        broadcastHealthUpdate = fn;
    }

    /**
     * Refresh the health cache. The actual health-check callback is injected
     * to avoid coupling to the health-check implementation.
     */
    public static void refreshHealthSnapshot(Object newHealth) {
        healthCache.set(newHealth);
        healthVersion.incrementAndGet();
        Consumer<Object> cb = broadcastHealthUpdate;
        if (cb != null) {
            cb.accept(newHealth);
        }
    }

    // ── Dependency injection interface ──────────────────────────

    /**
     * Dependencies required to build a Snapshot.
     * Callers must provide an implementation.
     */
    public interface SnapshotDependencies {
        java.util.List<Object> getPresenceList();

        Object getHealthSummary();

        long getUptimeMs();

        String getConfigPath();

        String getStateDir();
    }
}

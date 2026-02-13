package com.openclaw.gateway.server;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Manages pending exec approval requests with timeout.
 * Corresponds to TypeScript's exec-approval-manager.ts.
 */
public class ExecApprovalManager {

    // =========================================================================
    // Types
    // =========================================================================

    public record ExecApprovalRequestPayload(
            String command,
            String cwd,
            String host,
            String security,
            String ask,
            String agentId,
            String resolvedPath,
            String sessionKey) {
    }

    public record ExecApprovalRecord(
            String id,
            ExecApprovalRequestPayload request,
            long createdAtMs,
            long expiresAtMs,
            Long resolvedAtMs,
            String decision,
            String resolvedBy) {
        public ExecApprovalRecord withResolution(String decision, String resolvedBy) {
            return new ExecApprovalRecord(id, request, createdAtMs, expiresAtMs,
                    System.currentTimeMillis(), decision, resolvedBy);
        }
    }

    // =========================================================================
    // State
    // =========================================================================

    private record PendingEntry(
            ExecApprovalRecord record,
            CompletableFuture<String> future,
            java.util.concurrent.ScheduledFuture<?> timer) {
    }

    private final Map<String, PendingEntry> pending = new ConcurrentHashMap<>();
    private final ScheduledExecutorService scheduler;

    public ExecApprovalManager(ScheduledExecutorService scheduler) {
        this.scheduler = scheduler;
    }

    // =========================================================================
    // Operations
    // =========================================================================

    /**
     * Create a new approval record.
     */
    public ExecApprovalRecord create(ExecApprovalRequestPayload request, long timeoutMs, String id) {
        long now = System.currentTimeMillis();
        String resolvedId = (id != null && !id.trim().isEmpty()) ? id.trim() : UUID.randomUUID().toString();
        return new ExecApprovalRecord(resolvedId, request, now, now + timeoutMs, null, null, null);
    }

    /**
     * Wait for a decision on an approval record.
     * Returns the decision string, or null if timed out.
     */
    public CompletableFuture<String> waitForDecision(ExecApprovalRecord record, long timeoutMs) {
        CompletableFuture<String> future = new CompletableFuture<>();
        var timer = scheduler.schedule(() -> {
            pending.remove(record.id());
            future.complete(null);
        }, timeoutMs, TimeUnit.MILLISECONDS);
        pending.put(record.id(), new PendingEntry(record, future, timer));
        return future;
    }

    /**
     * Resolve a pending approval.
     * 
     * @return true if the record was found and resolved
     */
    public boolean resolve(String recordId, String decision, String resolvedBy) {
        PendingEntry entry = pending.remove(recordId);
        if (entry == null)
            return false;
        entry.timer().cancel(false);
        entry.future().complete(decision);
        return true;
    }

    /**
     * Get a snapshot of a pending record.
     */
    public ExecApprovalRecord getSnapshot(String recordId) {
        PendingEntry entry = pending.get(recordId);
        return entry != null ? entry.record() : null;
    }
}

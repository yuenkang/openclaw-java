package com.openclaw.agent.sandbox;

import com.openclaw.agent.sandbox.SandboxTypes.SandboxConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.List;

/**
 * Sandbox container pruning — removes idle/old containers and browsers.
 * Corresponds to TypeScript sandbox/prune.ts.
 */
@Slf4j
public final class SandboxPruner {

    private SandboxPruner() {
    }

    private static volatile long lastPruneAtMs = 0;
    private static final long PRUNE_INTERVAL_MS = 5 * 60 * 1000; // 5 min

    /**
     * Prune sandbox containers if enough time has elapsed since last prune.
     */
    public static void maybePruneSandboxes(SandboxConfig cfg) {
        long now = System.currentTimeMillis();
        if (now - lastPruneAtMs < PRUNE_INTERVAL_MS) {
            return;
        }
        lastPruneAtMs = now;
        try {
            pruneSandboxContainers(cfg);
            pruneSandboxBrowsers(cfg);
        } catch (Exception e) {
            log.error("Sandbox prune failed: {}", e.getMessage());
        }
    }

    private static void pruneSandboxContainers(SandboxConfig cfg) throws IOException {
        int idleHours = cfg.getPrune().getIdleHours();
        int maxAgeDays = cfg.getPrune().getMaxAgeDays();
        if (idleHours == 0 && maxAgeDays == 0)
            return;

        long now = System.currentTimeMillis();
        List<SandboxRegistry.SandboxRegistryEntry> entries = SandboxRegistry.readRegistry();

        for (var entry : entries) {
            long idleMs = now - entry.getLastUsedAtMs();
            long ageMs = now - entry.getCreatedAtMs();
            boolean shouldRemove = (idleHours > 0 && idleMs > (long) idleHours * 60 * 60 * 1000) ||
                    (maxAgeDays > 0 && ageMs > (long) maxAgeDays * 24 * 60 * 60 * 1000);
            if (shouldRemove) {
                try {
                    SandboxDocker.execDocker(List.of("rm", "-f", entry.getContainerName()), true);
                } catch (Exception e) {
                    // ignore prune failures
                } finally {
                    SandboxRegistry.removeRegistryEntry(entry.getContainerName());
                }
            }
        }
    }

    private static void pruneSandboxBrowsers(SandboxConfig cfg) throws IOException {
        int idleHours = cfg.getPrune().getIdleHours();
        int maxAgeDays = cfg.getPrune().getMaxAgeDays();
        if (idleHours == 0 && maxAgeDays == 0)
            return;

        long now = System.currentTimeMillis();
        List<SandboxRegistry.SandboxBrowserRegistryEntry> entries = SandboxRegistry.readBrowserRegistry();

        for (var entry : entries) {
            long idleMs = now - entry.getLastUsedAtMs();
            long ageMs = now - entry.getCreatedAtMs();
            boolean shouldRemove = (idleHours > 0 && idleMs > (long) idleHours * 60 * 60 * 1000) ||
                    (maxAgeDays > 0 && ageMs > (long) maxAgeDays * 24 * 60 * 60 * 1000);
            if (shouldRemove) {
                try {
                    SandboxDocker.execDocker(List.of("rm", "-f", entry.getContainerName()), true);
                } catch (Exception e) {
                    // ignore prune failures
                } finally {
                    SandboxRegistry.removeBrowserRegistryEntry(entry.getContainerName());
                }
            }
        }
    }

    /**
     * Ensure a specific Docker container is running (restart if stopped).
     */
    public static void ensureDockerContainerIsRunning(String containerName) throws IOException {
        var state = SandboxDocker.dockerContainerState(containerName);
        if (state.isExists() && !state.isRunning()) {
            SandboxDocker.execDocker(List.of("start", containerName));
        }
    }
}

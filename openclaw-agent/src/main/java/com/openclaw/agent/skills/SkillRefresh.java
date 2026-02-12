package com.openclaw.agent.skills;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.regex.Pattern;

/**
 * Skills file-watcher and snapshot versioning.
 * Monitors workspace/managed/extra skill directories for changes and bumps
 * snapshot versions to trigger prompt rebuilds.
 * Corresponds to TypeScript skills/refresh.ts.
 */
@Slf4j
public final class SkillRefresh {

    private SkillRefresh() {
    }

    // ── Ignored patterns (same as TS DEFAULT_SKILLS_WATCH_IGNORED) ──
    public static final List<Pattern> DEFAULT_WATCH_IGNORED = List.of(
            Pattern.compile("(^|[\\\\/])\\.git([\\\\/]|$)"),
            Pattern.compile("(^|[\\\\/])node_modules([\\\\/]|$)"),
            Pattern.compile("(^|[\\\\/])dist([\\\\/]|$)"));

    // ── Change event ────────────────────────────────────────────────

    public record SkillsChangeEvent(
            String workspaceDir,
            ChangeReason reason,
            String changedPath) {

        public enum ChangeReason {
            WATCH, MANUAL, REMOTE_NODE
        }
    }

    // ── State ───────────────────────────────────────────────────────

    private static final Set<Consumer<SkillsChangeEvent>> listeners = ConcurrentHashMap.newKeySet();
    private static final Map<String, Long> workspaceVersions = new ConcurrentHashMap<>();
    private static final Map<String, WatchState> watchers = new ConcurrentHashMap<>();
    private static volatile long globalVersion = 0;

    private record WatchState(
            WatchService service,
            String pathsKey,
            long debounceMs,
            ScheduledFuture<?> timer,
            Thread watchThread) {
    }

    // ── Listeners ───────────────────────────────────────────────────

    /**
     * Register a listener for skills change events.
     * Returns a Runnable to unregister.
     */
    public static Runnable registerChangeListener(Consumer<SkillsChangeEvent> listener) {
        listeners.add(listener);
        return () -> listeners.remove(listener);
    }

    // ── Version management ──────────────────────────────────────────

    /**
     * Bump the snapshot version for a workspace (or global if null).
     */
    public static long bumpSnapshotVersion(String workspaceDir,
            SkillsChangeEvent.ChangeReason reason,
            String changedPath) {
        if (workspaceDir != null && !workspaceDir.isBlank()) {
            long current = workspaceVersions.getOrDefault(workspaceDir, 0L);
            long next = bumpVersion(current);
            workspaceVersions.put(workspaceDir, next);
            emit(new SkillsChangeEvent(workspaceDir, reason, changedPath));
            return next;
        }
        globalVersion = bumpVersion(globalVersion);
        emit(new SkillsChangeEvent(null, reason, changedPath));
        return globalVersion;
    }

    /**
     * Get the current snapshot version for a workspace.
     */
    public static long getSnapshotVersion(String workspaceDir) {
        if (workspaceDir == null || workspaceDir.isBlank()) {
            return globalVersion;
        }
        long local = workspaceVersions.getOrDefault(workspaceDir, 0L);
        return Math.max(globalVersion, local);
    }

    // ── Watcher management ──────────────────────────────────────────

    /**
     * Ensure a file watcher is running for the given workspace.
     */
    public static void ensureWatcher(String workspaceDir, OpenClawConfig config) {
        if (workspaceDir == null || workspaceDir.isBlank())
            return;

        boolean watchEnabled = config == null
                || config.getSkills() == null
                || config.getSkills().getLoad() == null
                || config.getSkills().getLoad().getWatch() != Boolean.FALSE;

        long debounceMs = 250;
        if (config != null && config.getSkills() != null
                && config.getSkills().getLoad() != null
                && config.getSkills().getLoad().getWatchDebounceMs() != null) {
            debounceMs = Math.max(0, config.getSkills().getLoad().getWatchDebounceMs());
        }

        WatchState existing = watchers.get(workspaceDir);
        if (!watchEnabled) {
            if (existing != null) {
                stopWatcher(workspaceDir, existing);
            }
            return;
        }

        List<String> watchPaths = resolveWatchPaths(workspaceDir, config);
        String pathsKey = String.join("|", watchPaths);
        if (existing != null && existing.pathsKey().equals(pathsKey)
                && existing.debounceMs() == debounceMs) {
            return; // Already watching with same config
        }

        if (existing != null) {
            stopWatcher(workspaceDir, existing);
        }

        // Start new watcher
        try {
            startWatcher(workspaceDir, watchPaths, pathsKey, debounceMs);
        } catch (IOException e) {
            log.warn("Failed to start skills watcher for {}: {}", workspaceDir, e.getMessage());
        }
    }

    // ── Internal ────────────────────────────────────────────────────

    private static long bumpVersion(long current) {
        long now = System.currentTimeMillis();
        return now <= current ? current + 1 : now;
    }

    private static void emit(SkillsChangeEvent event) {
        for (Consumer<SkillsChangeEvent> listener : listeners) {
            try {
                listener.accept(event);
            } catch (Exception e) {
                log.warn("Skills change listener failed: {}", e.getMessage());
            }
        }
    }

    private static List<String> resolveWatchPaths(String workspaceDir, OpenClawConfig config) {
        List<String> paths = new ArrayList<>();
        if (!workspaceDir.isBlank()) {
            paths.add(Path.of(workspaceDir, "skills").toString());
        }
        // Managed skills dir
        String configDir = System.getProperty("user.home") + "/.openclaw";
        paths.add(Path.of(configDir, "skills").toString());

        // Extra dirs from config
        if (config != null && config.getSkills() != null
                && config.getSkills().getLoad() != null
                && config.getSkills().getLoad().getExtraDirs() != null) {
            for (String dir : config.getSkills().getLoad().getExtraDirs()) {
                if (dir != null && !dir.isBlank()) {
                    paths.add(dir.trim());
                }
            }
        }
        return paths;
    }

    private static void startWatcher(String workspaceDir, List<String> watchPaths,
            String pathsKey, long debounceMs) throws IOException {
        WatchService service = FileSystems.getDefault().newWatchService();
        ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "skills-watcher-" + workspaceDir.hashCode());
            t.setDaemon(true);
            return t;
        });

        for (String watchPath : watchPaths) {
            Path p = Path.of(watchPath);
            if (Files.isDirectory(p)) {
                try {
                    p.register(service,
                            StandardWatchEventKinds.ENTRY_CREATE,
                            StandardWatchEventKinds.ENTRY_MODIFY,
                            StandardWatchEventKinds.ENTRY_DELETE);
                } catch (IOException e) {
                    log.debug("Cannot watch {}: {}", watchPath, e.getMessage());
                }
            }
        }

        Thread watchThread = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    WatchKey key = service.take();
                    List<WatchEvent<?>> events = key.pollEvents();
                    for (WatchEvent<?> event : events) {
                        String changed = event.context() != null ? event.context().toString() : null;
                        scheduler.schedule(() -> bumpSnapshotVersion(workspaceDir,
                                SkillsChangeEvent.ChangeReason.WATCH, changed),
                                debounceMs, TimeUnit.MILLISECONDS);
                    }
                    key.reset();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                } catch (ClosedWatchServiceException e) {
                    break;
                }
            }
        }, "skills-watch-poll-" + workspaceDir.hashCode());
        watchThread.setDaemon(true);
        watchThread.start();

        watchers.put(workspaceDir, new WatchState(service, pathsKey, debounceMs, null, watchThread));
    }

    private static void stopWatcher(String workspaceDir, WatchState state) {
        watchers.remove(workspaceDir);
        try {
            state.service().close();
        } catch (IOException e) {
            // ignore
        }
        if (state.watchThread() != null) {
            state.watchThread().interrupt();
        }
    }
}

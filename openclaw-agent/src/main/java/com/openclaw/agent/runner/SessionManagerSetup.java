package com.openclaw.agent.runner;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Session manager initialization and cache for embedded agent runs.
 * Combines {@code agents/pi-embedded-runner/session-manager-init.ts}
 * and {@code agents/pi-embedded-runner/session-manager-cache.ts}.
 */
public final class SessionManagerSetup {

    private SessionManagerSetup() {
    }

    // ── session-manager-cache ──────────────────────────────────────────

    private static final long DEFAULT_TTL_MS = 45_000; // 45 seconds

    /** Cache entry tracking when a session file was last loaded. */
    private static final Map<String, Long> SESSION_CACHE = new java.util.concurrent.ConcurrentHashMap<>();

    /** Resolve the TTL from environment or default. */
    private static long getSessionManagerTtl() {
        String env = System.getenv("OPENCLAW_SESSION_MANAGER_CACHE_TTL_MS");
        if (env != null) {
            try {
                long val = Long.parseLong(env.trim());
                return val >= 0 ? val : DEFAULT_TTL_MS;
            } catch (NumberFormatException ignored) {
            }
        }
        return DEFAULT_TTL_MS;
    }

    private static boolean isCacheEnabled() {
        return getSessionManagerTtl() > 0;
    }

    /** Track access to a session file for caching purposes. */
    public static void trackSessionManagerAccess(String sessionFile) {
        if (!isCacheEnabled())
            return;
        SESSION_CACHE.put(sessionFile, System.currentTimeMillis());
    }

    /** Check if a session file is still cached (loaded recently). */
    public static boolean isSessionManagerCached(String sessionFile) {
        if (!isCacheEnabled())
            return false;
        Long loadedAt = SESSION_CACHE.get(sessionFile);
        if (loadedAt == null)
            return false;
        return System.currentTimeMillis() - loadedAt <= getSessionManagerTtl();
    }

    /**
     * Pre-warm the OS page cache for a session file by reading a small chunk.
     */
    public static void prewarmSessionFile(String sessionFile) {
        if (!isCacheEnabled())
            return;
        if (isSessionManagerCached(sessionFile))
            return;
        try {
            Path path = Path.of(sessionFile);
            if (Files.exists(path)) {
                // Read a small chunk to encourage OS page-cache warmup
                byte[] buf = new byte[4096];
                try (var is = Files.newInputStream(path)) {
                    is.read(buf);
                }
                trackSessionManagerAccess(sessionFile);
            }
        } catch (IOException ignored) {
            // File doesn't exist yet, SessionManager will create it
        }
    }

    // ── session-manager-init ──────────────────────────────────────────

    /**
     * Prepare session manager data for a run.
     * Normalizes the session file/state so the first user prompt is persisted
     * correctly.
     *
     * @param fileEntries    list of parsed session file entries (mutated in place)
     * @param sessionFile    path to the session file
     * @param hadSessionFile whether the file existed before this run
     * @param sessionId      the session ID to use
     * @param cwd            current working directory
     * @return updated session ID (may be unchanged)
     */
    @SuppressWarnings("unchecked")
    public static String prepareSessionManagerForRun(
            List<Map<String, Object>> fileEntries,
            String sessionFile,
            boolean hadSessionFile,
            String sessionId,
            String cwd) {
        // Find header entry
        Map<String, Object> header = null;
        boolean hasAssistant = false;
        for (Map<String, Object> entry : fileEntries) {
            if (entry == null)
                continue;
            if ("session".equals(entry.get("type"))) {
                if (header == null)
                    header = entry;
            }
            if ("message".equals(entry.get("type"))) {
                Object msgObj = entry.get("message");
                if (msgObj instanceof Map<?, ?> msg && "assistant".equals(msg.get("role"))) {
                    hasAssistant = true;
                }
            }
        }

        if (!hadSessionFile && header != null) {
            header.put("id", sessionId);
            header.put("cwd", cwd);
            return sessionId;
        }

        if (hadSessionFile && header != null && !hasAssistant) {
            // Reset file so the first assistant flush includes header+user+assistant in
            // order
            try {
                Files.writeString(Path.of(sessionFile), "", StandardCharsets.UTF_8);
            } catch (IOException ignored) {
            }
            fileEntries.clear();
            fileEntries.add(header);
        }

        return sessionId;
    }
}

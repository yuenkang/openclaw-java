package com.openclaw.gateway.session;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.openclaw.common.config.SessionPaths;
import com.openclaw.common.model.AcpSession;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.*;

/**
 * File-system backed session metadata persistence.
 * Manages a sessions.json file that maps sessionKey → session metadata.
 * Corresponds to TypeScript's config/sessions/store.ts.
 *
 * <p>
 * File layout:
 * </p>
 * 
 * <pre>
 *   ~/.openclaw/state/agents/{agentId}/sessions/sessions.json
 * </pre>
 *
 * <p>
 * The file is a JSON object mapping sessionKey to session metadata entries.
 * </p>
 */
@Slf4j
public class SessionPersistence {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT);

    /**
     * Serializable session entry that maps to the sessions.json format.
     */
    public record SessionEntry(
            String sessionId,
            String sessionKey,
            String sessionFile,
            String cwd,
            long createdAt,
            long updatedAt,
            String lastChannel,
            String lastAccountId) {
        /**
         * Create from an AcpSession.
         */
        public static SessionEntry fromAcpSession(AcpSession session, String agentId) {
            Path transcriptPath = SessionPaths.resolveSessionTranscriptPath(
                    session.getSessionId(), agentId);
            return new SessionEntry(
                    session.getSessionId(),
                    session.getSessionKey(),
                    transcriptPath.toString(),
                    session.getCwd(),
                    session.getCreatedAt(),
                    session.getUpdatedAt() > 0 ? session.getUpdatedAt() : session.getCreatedAt(),
                    null, null);
        }

        /**
         * Convert to an AcpSession.
         */
        public AcpSession toAcpSession() {
            return AcpSession.builder()
                    .sessionId(sessionId)
                    .sessionKey(sessionKey)
                    .cwd(cwd)
                    .createdAt(createdAt)
                    .updatedAt(updatedAt)
                    .build();
        }
    }

    // =========================================================================
    // Load / Save
    // =========================================================================

    /**
     * Load the session store from disk.
     *
     * @param storePath path to the sessions.json file
     * @return map of sessionKey → SessionEntry
     */
    public static Map<String, SessionEntry> loadSessionStore(Path storePath) {
        if (!Files.exists(storePath)) {
            return new LinkedHashMap<>();
        }

        try {
            String json = Files.readString(storePath, StandardCharsets.UTF_8);
            Map<String, SessionEntry> store = MAPPER.readValue(json,
                    new TypeReference<Map<String, SessionEntry>>() {
                    });
            log.debug("Loaded session store with {} entries from {}", store.size(), storePath);
            return store;
        } catch (IOException e) {
            log.error("Failed to load session store from {}: {}", storePath, e.getMessage());
            return new LinkedHashMap<>();
        }
    }

    /**
     * Save the session store to disk.
     *
     * @param storePath path to the sessions.json file
     * @param store     map of sessionKey → SessionEntry
     */
    public static void saveSessionStore(Path storePath, Map<String, SessionEntry> store) {
        try {
            Files.createDirectories(storePath.getParent());
            // Write to temp file then rename for atomicity
            Path tempFile = storePath.resolveSibling(storePath.getFileName() + ".tmp");
            String json = MAPPER.writeValueAsString(store);
            Files.writeString(tempFile, json, StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE);
            Files.move(tempFile, storePath, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
            log.debug("Saved session store with {} entries to {}", store.size(), storePath);
        } catch (IOException e) {
            log.error("Failed to save session store to {}: {}", storePath, e.getMessage());
        }
    }

    /**
     * Update a single session entry in the store and save.
     */
    public static void updateSessionEntry(Path storePath, String sessionKey, SessionEntry entry) {
        Map<String, SessionEntry> store = loadSessionStore(storePath);
        store.put(sessionKey, entry);
        saveSessionStore(storePath, store);
    }

    /**
     * Remove a session entry from the store and save.
     */
    public static void removeSessionEntry(Path storePath, String sessionKey) {
        Map<String, SessionEntry> store = loadSessionStore(storePath);
        store.remove(sessionKey);
        saveSessionStore(storePath, store);
    }

    // =========================================================================
    // Convenience: restore sessions into SessionStore on startup
    // =========================================================================

    /**
     * Load persisted sessions from disk and populate the in-memory SessionStore.
     * Called once at application startup.
     *
     * @param sessionStore the in-memory session store to populate
     * @param agentId      the agent ID to resolve paths for
     */
    public static void restoreSessionsFromDisk(SessionStore sessionStore, String agentId) {
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        Map<String, SessionEntry> persisted = loadSessionStore(storePath);

        int restored = 0;
        for (Map.Entry<String, SessionEntry> e : persisted.entrySet()) {
            SessionEntry entry = e.getValue();
            AcpSession session = entry.toAcpSession();
            // Only restore if not already in memory
            if (sessionStore.findBySessionKey(entry.sessionKey()).isEmpty()) {
                sessionStore.createSession(
                        session.getSessionId(),
                        session.getSessionKey(),
                        session.getCwd());
                restored++;
            }
        }

        if (restored > 0) {
            log.info("Restored {} sessions from {}", restored, storePath);
        } else {
            log.debug("No persisted sessions to restore from {}", storePath);
        }
    }

    /**
     * Persist a single session to the store file.
     * Called after session creation or update.
     *
     * @param session the session to persist
     * @param agentId the agent ID for path resolution
     */
    public static void persistSession(AcpSession session, String agentId) {
        Path storePath = SessionPaths.resolveDefaultSessionStorePath(agentId);
        SessionEntry entry = SessionEntry.fromAcpSession(session, agentId);
        updateSessionEntry(storePath, session.getSessionKey(), entry);
    }
}

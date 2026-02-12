package com.openclaw.gateway.session;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * JSONL-based session transcript store.
 * Corresponds to TypeScript's session-utils.fs.ts.
 *
 * <p>
 * Each session's transcript is stored as a JSONL file at:
 * ~/.openclaw/sessions/{sessionId}.jsonl
 * </p>
 *
 * <p>
 * Format: one JSON object per line.
 * </p>
 * <ul>
 * <li>Line 1 (header):
 * {@code {"type":"session","version":1,"id":"...","timestamp":"..."}}</li>
 * <li>Subsequent lines:
 * {@code {"message":{"role":"user|assistant|tool|system","content":"..."}}}</li>
 * </ul>
 */
@Slf4j
public class SessionTranscriptStore {

    private static final int CURRENT_SESSION_VERSION = 1;
    private final Path sessionsDir;
    private final ObjectMapper mapper;

    public SessionTranscriptStore(ObjectMapper mapper) {
        this(resolveDefaultSessionsDir(), mapper);
    }

    public SessionTranscriptStore(Path sessionsDir, ObjectMapper mapper) {
        this.sessionsDir = sessionsDir;
        this.mapper = mapper;
    }

    private static Path resolveDefaultSessionsDir() {
        String home = System.getProperty("user.home");
        return Path.of(home, ".openclaw", "sessions");
    }

    /**
     * Ensure a JSONL transcript file exists for the given session.
     * Creates the file with a header line if it doesn't exist.
     *
     * @return the path to the transcript file
     */
    public Path ensureTranscriptFile(String sessionId) throws IOException {
        Path filePath = sessionsDir.resolve(sessionId + ".jsonl");
        if (Files.exists(filePath)) {
            return filePath;
        }

        Files.createDirectories(sessionsDir);

        Map<String, Object> header = new LinkedHashMap<>();
        header.put("type", "session");
        header.put("version", CURRENT_SESSION_VERSION);
        header.put("id", sessionId);
        header.put("timestamp", java.time.Instant.now().toString());

        Files.writeString(filePath, mapper.writeValueAsString(header) + "\n",
                StandardOpenOption.CREATE, StandardOpenOption.WRITE);

        log.debug("Created transcript file: {}", filePath);
        return filePath;
    }

    /**
     * Append a message to the session transcript.
     */
    public void appendMessage(String sessionId, String role, String content) throws IOException {
        Path filePath = ensureTranscriptFile(sessionId);

        Map<String, Object> message = new LinkedHashMap<>();
        message.put("role", role);
        message.put("content", content);

        Map<String, Object> entry = new LinkedHashMap<>();
        entry.put("message", message);
        entry.put("ts", System.currentTimeMillis());

        Files.writeString(filePath, mapper.writeValueAsString(entry) + "\n",
                StandardOpenOption.APPEND);
    }

    /**
     * Append a message with tool call information.
     */
    public void appendToolMessage(String sessionId, String role, String content,
            String toolName, String toolId) throws IOException {
        Path filePath = ensureTranscriptFile(sessionId);

        Map<String, Object> message = new LinkedHashMap<>();
        message.put("role", role);
        message.put("content", content);
        if (toolName != null)
            message.put("toolName", toolName);
        if (toolId != null)
            message.put("toolId", toolId);

        Map<String, Object> entry = new LinkedHashMap<>();
        entry.put("message", message);
        entry.put("ts", System.currentTimeMillis());

        Files.writeString(filePath, mapper.writeValueAsString(entry) + "\n",
                StandardOpenOption.APPEND);
    }

    /**
     * Read all messages from a session transcript.
     *
     * @return list of message maps with "role" and "content" keys
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> readMessages(String sessionId) {
        Path filePath = sessionsDir.resolve(sessionId + ".jsonl");
        if (!Files.exists(filePath)) {
            return Collections.emptyList();
        }

        List<Map<String, Object>> messages = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(filePath)) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty())
                    continue;
                try {
                    Map<String, Object> parsed = mapper.readValue(line, Map.class);
                    Object msg = parsed.get("message");
                    if (msg instanceof Map) {
                        messages.add((Map<String, Object>) msg);
                    }
                } catch (Exception e) {
                    // skip malformed lines
                }
            }
        } catch (IOException e) {
            log.warn("Failed to read transcript for session {}: {}", sessionId, e.getMessage());
        }
        return messages;
    }

    /**
     * Read the last N messages from a session transcript (for preview).
     */
    public List<Map<String, Object>> readLastMessages(String sessionId, int maxMessages) {
        List<Map<String, Object>> all = readMessages(sessionId);
        if (all.size() <= maxMessages) {
            return all;
        }
        return all.subList(all.size() - maxMessages, all.size());
    }

    /**
     * Get the transcript file path for a session.
     */
    public Path getTranscriptPath(String sessionId) {
        return sessionsDir.resolve(sessionId + ".jsonl");
    }

    /**
     * Check if a transcript exists for the given session.
     */
    public boolean hasTranscript(String sessionId) {
        return Files.exists(sessionsDir.resolve(sessionId + ".jsonl"));
    }

    /**
     * Delete the transcript file for a session.
     *
     * @return true if the file was deleted, false if it did not exist
     */
    public boolean deleteTranscript(String sessionId) {
        Path filePath = sessionsDir.resolve(sessionId + ".jsonl");
        try {
            boolean deleted = Files.deleteIfExists(filePath);
            if (deleted) {
                log.debug("Deleted transcript: {}", filePath);
            }
            return deleted;
        } catch (IOException e) {
            log.warn("Failed to delete transcript for session {}: {}", sessionId, e.getMessage());
            return false;
        }
    }

    /**
     * Reset the transcript file for a session (delete existing and create a fresh
     * header).
     *
     * @return the path to the new transcript file
     */
    public Path resetTranscript(String sessionId) throws IOException {
        Path filePath = sessionsDir.resolve(sessionId + ".jsonl");
        // Delete existing
        Files.deleteIfExists(filePath);
        // Create fresh with header
        return ensureTranscriptFile(sessionId);
    }

    /**
     * Read a preview of recent transcript items, truncated to maxChars per item.
     * Corresponds to TypeScript's readSessionPreviewItemsFromTranscript.
     *
     * @param sessionId the session ID
     * @param limit     max number of items to return
     * @param maxChars  max characters per content snippet
     * @return list of preview maps with "role", "content", "ts" keys
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> readPreview(String sessionId, int limit, int maxChars) {
        Path filePath = sessionsDir.resolve(sessionId + ".jsonl");
        if (!Files.exists(filePath)) {
            return Collections.emptyList();
        }

        // Read all message lines, then take the last N
        List<Map<String, Object>> items = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(filePath)) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty())
                    continue;
                try {
                    Map<String, Object> parsed = mapper.readValue(line, Map.class);
                    Object msg = parsed.get("message");
                    if (msg instanceof Map) {
                        Map<String, Object> msgMap = (Map<String, Object>) msg;
                        Map<String, Object> item = new LinkedHashMap<>();
                        item.put("role", msgMap.getOrDefault("role", "unknown"));

                        String content = String.valueOf(msgMap.getOrDefault("content", ""));
                        if (content.length() > maxChars) {
                            content = content.substring(0, maxChars) + "…";
                        }
                        item.put("content", content);

                        Object ts = parsed.get("ts");
                        if (ts != null)
                            item.put("ts", ts);

                        items.add(item);
                    }
                } catch (Exception e) {
                    // skip malformed
                }
            }
        } catch (IOException e) {
            log.warn("Failed to read preview for session {}: {}", sessionId, e.getMessage());
            return Collections.emptyList();
        }

        // Return the last `limit` items
        if (items.size() <= limit) {
            return items;
        }
        return items.subList(items.size() - limit, items.size());
    }

    /**
     * Compact a session transcript, keeping only the last maxLines lines.
     * Archives the original file before truncating.
     * Corresponds to TypeScript's sessions.compact handler logic.
     *
     * @param sessionId the session ID
     * @param maxLines  max lines to keep
     * @return CompactResult with details
     */
    public CompactResult compact(String sessionId, int maxLines) throws IOException {
        Path filePath = sessionsDir.resolve(sessionId + ".jsonl");
        if (!Files.exists(filePath)) {
            return new CompactResult(false, 0, null);
        }

        List<String> lines = Files.readAllLines(filePath);
        // Filter empty lines
        lines.removeIf(l -> l.trim().isEmpty());

        if (lines.size() <= maxLines) {
            return new CompactResult(false, lines.size(), null);
        }

        // Archive original file
        String archiveName = sessionId + ".bak." + System.currentTimeMillis() + ".jsonl";
        Path archivePath = sessionsDir.resolve(archiveName);
        Files.copy(filePath, archivePath, StandardCopyOption.REPLACE_EXISTING);

        // Keep only the last maxLines
        List<String> keptLines = lines.subList(lines.size() - maxLines, lines.size());
        Files.writeString(filePath, String.join("\n", keptLines) + "\n",
                StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);

        log.info("Compacted transcript for session {}: {} → {} lines, archived to {}",
                sessionId, lines.size(), keptLines.size(), archivePath);

        return new CompactResult(true, keptLines.size(), archivePath.toString());
    }

    /**
     * Result of a transcript compaction operation.
     */
    @lombok.Data
    @lombok.AllArgsConstructor
    public static class CompactResult {
        private final boolean compacted;
        private final int keptLines;
        private final String archived;
    }
}

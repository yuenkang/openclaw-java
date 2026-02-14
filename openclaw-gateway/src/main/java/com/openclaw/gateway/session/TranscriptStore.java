package com.openclaw.gateway.session;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.*;

/**
 * JSONL-based conversation transcript store.
 * Each session has a transcript file (one JSON object per line).
 * Format mirrors the TypeScript SessionManager transcript files.
 *
 * <p>
 * File layout:
 * </p>
 * 
 * <pre>
 *   ~/.openclaw/state/agents/{agentId}/sessions/{sessionId}.jsonl
 * </pre>
 *
 * <p>
 * Line 1 is always a header:
 * </p>
 * 
 * <pre>
 *   {"type":"session","version":1,"id":"...","timestamp":"..."}
 * </pre>
 *
 * <p>
 * Subsequent lines are messages:
 * </p>
 * 
 * <pre>
 *   {"type":"message","role":"user","content":[...],"timestamp":...}
 *   {"type":"message","role":"assistant","content":[...],"timestamp":...,"usage":{...}}
 * </pre>
 */
@Slf4j
public class TranscriptStore {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final int CURRENT_SESSION_VERSION = 1;

    // =========================================================================
    // Write operations
    // =========================================================================

    /**
     * Ensure the transcript file exists and has a valid header.
     * Creates parent directories if needed.
     */
    public static void ensureTranscriptFile(Path transcriptPath, String sessionId) {
        try {
            if (Files.exists(transcriptPath)) {
                return;
            }
            Files.createDirectories(transcriptPath.getParent());

            Map<String, Object> header = new LinkedHashMap<>();
            header.put("type", "session");
            header.put("version", CURRENT_SESSION_VERSION);
            header.put("id", sessionId);
            header.put("timestamp", Instant.now().toString());

            String headerLine = MAPPER.writeValueAsString(header) + "\n";
            Files.writeString(transcriptPath, headerLine,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.WRITE);
            log.debug("Created transcript file: {}", transcriptPath);
        } catch (IOException e) {
            log.error("Failed to create transcript file: {}", transcriptPath, e);
        }
    }

    /**
     * Append a user message to the transcript.
     */
    public static void appendUserMessage(Path transcriptPath, String sessionId,
            String text, long timestampMs) {
        Map<String, Object> entry = new LinkedHashMap<>();
        entry.put("type", "message");
        entry.put("role", "user");
        entry.put("content", List.of(Map.of("type", "text", "text", text)));
        entry.put("timestamp", timestampMs);

        appendEntry(transcriptPath, sessionId, entry);
    }

    /**
     * Append an assistant message to the transcript.
     */
    public static void appendAssistantMessage(Path transcriptPath, String sessionId,
            String text, long timestampMs,
            Map<String, Object> usage,
            String reasoningContent) {
        Map<String, Object> entry = new LinkedHashMap<>();
        entry.put("type", "message");
        entry.put("role", "assistant");
        entry.put("content", List.of(Map.of("type", "text", "text", text)));
        entry.put("timestamp", timestampMs);
        if (usage != null) {
            entry.put("usage", usage);
        }
        if (reasoningContent != null && !reasoningContent.isEmpty()) {
            entry.put("reasoningContent", reasoningContent);
        }

        appendEntry(transcriptPath, sessionId, entry);
    }

    /**
     * Append a raw entry (any map) to the transcript.
     */
    public static void appendEntry(Path transcriptPath, String sessionId,
            Map<String, Object> entry) {
        try {
            ensureTranscriptFile(transcriptPath, sessionId);
            String line = MAPPER.writeValueAsString(entry) + "\n";
            Files.writeString(transcriptPath, line,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.APPEND);
            log.debug("Appended {} entry to transcript: {}",
                    entry.get("role"), transcriptPath.getFileName());
        } catch (IOException e) {
            log.error("Failed to append to transcript: {}", transcriptPath, e);
        }
    }

    // =========================================================================
    // Read operations
    // =========================================================================

    /**
     * Read all message entries from a transcript file.
     * Skips the header line and any non-message entries.
     *
     * @param transcriptPath path to the JSONL transcript file
     * @param maxMessages    maximum number of messages to return (0 = all).
     *                       When limited, returns the most recent messages.
     * @return list of message entries (maps with role, content, timestamp etc.)
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> readMessages(Path transcriptPath, int maxMessages) {
        if (!Files.exists(transcriptPath)) {
            return List.of();
        }

        List<Map<String, Object>> messages = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(transcriptPath, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty())
                    continue;

                try {
                    Map<String, Object> entry = MAPPER.readValue(line, Map.class);
                    String type = (String) entry.get("type");
                    if ("message".equals(type)) {
                        messages.add(entry);
                    }
                } catch (JsonProcessingException e) {
                    log.warn("Skipping malformed JSONL line in {}", transcriptPath.getFileName());
                }
            }
        } catch (IOException e) {
            log.error("Failed to read transcript: {}", transcriptPath, e);
            return List.of();
        }

        // Return most recent messages if limited
        if (maxMessages > 0 && messages.size() > maxMessages) {
            return messages.subList(messages.size() - maxMessages, messages.size());
        }
        return messages;
    }

    /**
     * Read all messages from a transcript file.
     */
    public static List<Map<String, Object>> readMessages(Path transcriptPath) {
        return readMessages(transcriptPath, 0);
    }

    /**
     * Convert transcript message entries to ChatMessage format used by AgentRunner.
     *
     * @param messages list of transcript entries
     * @return list of ChatMessage-compatible maps with "role" and "content"
     *         (string)
     */
    public static List<Map<String, String>> toChatMessages(List<Map<String, Object>> messages) {
        List<Map<String, String>> chatMessages = new ArrayList<>();
        for (Map<String, Object> msg : messages) {
            String role = (String) msg.get("role");
            if (role == null)
                continue;

            // Extract text from content array
            String text = "";
            Object content = msg.get("content");
            if (content instanceof List<?> contentList) {
                for (Object item : contentList) {
                    if (item instanceof Map<?, ?> contentItem) {
                        if ("text".equals(contentItem.get("type"))) {
                            text = String.valueOf(contentItem.get("text"));
                            break;
                        }
                    }
                }
            } else if (content instanceof String s) {
                text = s;
            }

            if (!text.isEmpty()) {
                chatMessages.add(Map.of("role", role, "content", text));
            }
        }
        return chatMessages;
    }

    /**
     * Count the total number of messages in a transcript.
     */
    public static int countMessages(Path transcriptPath) {
        return readMessages(transcriptPath).size();
    }

    /**
     * Clear a transcript file (remove all messages but keep header).
     */
    public static void clearTranscript(Path transcriptPath, String sessionId) {
        try {
            if (Files.exists(transcriptPath)) {
                Files.delete(transcriptPath);
            }
            ensureTranscriptFile(transcriptPath, sessionId);
            log.info("Cleared transcript: {}", transcriptPath);
        } catch (IOException e) {
            log.error("Failed to clear transcript: {}", transcriptPath, e);
        }
    }
}

package com.openclaw.agent.runtime;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Repair corrupted JSONL session files by dropping malformed lines.
 * Mirrors {@code agents/session-file-repair.ts}.
 */
public final class SessionFileRepair {

    private SessionFileRepair() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Result of a repair attempt. */
    public record RepairReport(
            boolean repaired,
            int droppedLines,
            String backupPath,
            String reason) {
    }

    /**
     * Attempt to repair a JSONL session file by dropping unparseable lines.
     *
     * @param sessionFile path to the session file
     * @param warn        optional warning consumer
     * @return repair report
     */
    public static RepairReport repairSessionFileIfNeeded(
            String sessionFile, Consumer<String> warn) {
        String trimmed = sessionFile == null ? "" : sessionFile.trim();
        if (trimmed.isEmpty()) {
            return new RepairReport(false, 0, null, "missing session file");
        }

        Path path = Path.of(trimmed);
        String content;
        try {
            content = Files.readString(path);
        } catch (NoSuchFileException e) {
            return new RepairReport(false, 0, null, "missing session file");
        } catch (IOException e) {
            String reason = "failed to read session file: " + e.getMessage();
            if (warn != null) {
                warn.accept("session file repair skipped: " + reason
                        + " (" + path.getFileName() + ")");
            }
            return new RepairReport(false, 0, null, reason);
        }

        String[] lines = content.split("\\r?\\n");
        List<JsonNode> entries = new ArrayList<>();
        int droppedLines = 0;

        for (String line : lines) {
            if (line.trim().isEmpty()) {
                continue;
            }
            try {
                entries.add(MAPPER.readTree(line));
            } catch (Exception e) {
                droppedLines++;
            }
        }

        if (entries.isEmpty()) {
            return new RepairReport(false, droppedLines, null, "empty session file");
        }

        // Validate session header
        JsonNode header = entries.get(0);
        if (!isSessionHeader(header)) {
            if (warn != null) {
                warn.accept("session file repair skipped: invalid session header ("
                        + path.getFileName() + ")");
            }
            return new RepairReport(false, droppedLines, null, "invalid session header");
        }

        if (droppedLines == 0) {
            return new RepairReport(false, 0, null, null);
        }

        // Write repaired file
        StringBuilder cleaned = new StringBuilder();
        for (JsonNode entry : entries) {
            cleaned.append(entry.toString()).append('\n');
        }

        long pid = ProcessHandle.current().pid();
        long now = System.currentTimeMillis();
        String backupPath = trimmed + ".bak-" + pid + "-" + now;
        String tmpPath = trimmed + ".repair-" + pid + "-" + now + ".tmp";

        try {
            Files.writeString(Path.of(backupPath), content);
            Files.writeString(Path.of(tmpPath), cleaned.toString());
            Files.move(Path.of(tmpPath), path, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            try {
                Files.deleteIfExists(Path.of(tmpPath));
            } catch (IOException ignored) {
                if (warn != null) {
                    warn.accept("session file repair cleanup failed: " + ignored.getMessage()
                            + " (" + Path.of(tmpPath).getFileName() + ")");
                }
            }
            return new RepairReport(false, droppedLines, null,
                    "repair failed: " + e.getMessage());
        }

        if (warn != null) {
            warn.accept("session file repaired: dropped " + droppedLines
                    + " malformed line(s) (" + path.getFileName() + ")");
        }
        return new RepairReport(true, droppedLines, backupPath, null);
    }

    private static boolean isSessionHeader(JsonNode node) {
        if (!node.isObject())
            return false;
        JsonNode type = node.get("type");
        JsonNode id = node.get("id");
        return type != null && "session".equals(type.asText())
                && id != null && id.isTextual() && !id.asText().isEmpty();
    }
}

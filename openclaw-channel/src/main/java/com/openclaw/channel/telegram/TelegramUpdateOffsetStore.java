package com.openclaw.channel.telegram;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;
import java.util.UUID;

/**
 * Telegram update offset persistence for long-polling resumption.
 * Corresponds to TypeScript's telegram/update-offset-store.ts.
 */
public final class TelegramUpdateOffsetStore {

    private TelegramUpdateOffsetStore() {
    }

    private static final int STORE_VERSION = 1;

    /**
     * Normalize an account ID for use as a filename component.
     */
    static String normalizeAccountId(String accountId) {
        String trimmed = accountId != null ? accountId.trim() : "";
        if (trimmed.isEmpty())
            return "default";
        return trimmed.replaceAll("[^a-zA-Z0-9._-]+", "_");
    }

    /**
     * Resolve the file path for a given account's update offset.
     */
    public static Path resolveOffsetPath(Path stateDir, String accountId) {
        return stateDir.resolve("telegram")
                .resolve("update-offset-" + normalizeAccountId(accountId) + ".json");
    }

    /**
     * Read the last update offset from disk.
     *
     * @return the last update ID, or null if not available
     */
    public static Integer readOffset(Path stateDir, String accountId) {
        Path filePath = resolveOffsetPath(stateDir, accountId);
        if (!Files.exists(filePath))
            return null;
        try {
            String raw = Files.readString(filePath, StandardCharsets.UTF_8);
            // Simple parse: find "lastUpdateId" value
            String key = "\"lastUpdateId\"";
            int idx = raw.indexOf(key);
            if (idx < 0)
                return null;
            int colon = raw.indexOf(':', idx + key.length());
            if (colon < 0)
                return null;
            int start = colon + 1;
            while (start < raw.length() && raw.charAt(start) == ' ')
                start++;
            if (start >= raw.length())
                return null;
            if (raw.startsWith("null", start))
                return null;
            int end = start;
            while (end < raw.length() && (Character.isDigit(raw.charAt(end))
                    || raw.charAt(end) == '-'))
                end++;
            if (end == start)
                return null;
            return Integer.parseInt(raw.substring(start, end));
        } catch (IOException | NumberFormatException e) {
            return null;
        }
    }

    /**
     * Write the update offset to disk atomically.
     */
    public static void writeOffset(Path stateDir, String accountId, int updateId)
            throws IOException {
        Path filePath = resolveOffsetPath(stateDir, accountId);
        Path dir = filePath.getParent();
        Files.createDirectories(dir);
        Path tmp = dir.resolve(filePath.getFileName() + "." + UUID.randomUUID() + ".tmp");
        String payload = String.format(
                "{\n  \"version\": %d,\n  \"lastUpdateId\": %d\n}\n", STORE_VERSION, updateId);
        Files.writeString(tmp, payload, StandardCharsets.UTF_8,
                StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        try {
            Files.setPosixFilePermissions(tmp, Set.of(
                    PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE));
        } catch (UnsupportedOperationException ignored) {
            // Windows doesn't support POSIX permissions
        }
        Files.move(tmp, filePath, java.nio.file.StandardCopyOption.ATOMIC_MOVE);
    }
}

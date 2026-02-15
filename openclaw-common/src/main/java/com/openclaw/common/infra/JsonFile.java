package com.openclaw.common.infra;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

/**
 * JSON file load/save with secure file permissions.
 * Corresponds to TypeScript's infra/json-file.ts.
 */
public final class JsonFile {

    private JsonFile() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT);

    /**
     * Load and parse a JSON file. Returns null if the file does not exist or is
     * invalid.
     */
    public static <T> T load(Path path, Class<T> type) {
        try {
            if (!Files.exists(path))
                return null;
            String raw = Files.readString(path);
            return MAPPER.readValue(raw, type);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Load a JSON file as a generic Object (Map or List).
     */
    public static Object load(Path path) {
        return load(path, Object.class);
    }

    /**
     * Save data as a JSON file with restrictive permissions (owner-only rw).
     */
    public static void save(Path path, Object data) throws IOException {
        Path dir = path.getParent();
        if (dir != null && !Files.exists(dir)) {
            Files.createDirectories(dir);
        }
        String json = MAPPER.writeValueAsString(data) + "\n";
        Files.writeString(path, json);

        // Set restrictive permissions on POSIX systems
        try {
            Set<PosixFilePermission> perms = PosixFilePermissions.fromString("rw-------");
            Files.setPosixFilePermissions(path, perms);
        } catch (UnsupportedOperationException ignored) {
            // Non-POSIX (e.g. Windows) — skip
        }
    }
}

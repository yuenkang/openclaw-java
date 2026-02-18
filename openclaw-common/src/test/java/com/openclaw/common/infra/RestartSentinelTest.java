package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class RestartSentinelTest {

    @TempDir
    Path tempDir;

    @Test
    void writeThenRead() throws IOException {
        RestartSentinel.Payload payload = RestartSentinel.Payload.of(
                "update", "completed", "Updated to v1.2.3");

        Path path = RestartSentinel.writeSentinel(tempDir, payload);
        assertTrue(Files.exists(path));

        RestartSentinel.Sentinel sentinel = RestartSentinel.readSentinel(tempDir);
        assertNotNull(sentinel);
        assertEquals(1, sentinel.version());
        assertEquals("update", sentinel.payload().kind());
        assertEquals("completed", sentinel.payload().status());
        assertEquals("Updated to v1.2.3", sentinel.payload().message());
        assertNotNull(sentinel.payload().timestampMs());
    }

    @Test
    void consumeSentinel_deletesFile() throws IOException {
        RestartSentinel.Payload payload = RestartSentinel.Payload.of(
                "restart", "pending", "Scheduled restart");

        RestartSentinel.writeSentinel(tempDir, payload);

        RestartSentinel.Payload consumed = RestartSentinel.consumeSentinel(tempDir);
        assertNotNull(consumed);
        assertEquals("restart", consumed.kind());

        // File should be deleted after consume
        assertFalse(Files.exists(RestartSentinel.resolveSentinelPath(tempDir)));

        // Second consume should return null
        assertNull(RestartSentinel.consumeSentinel(tempDir));
    }

    @Test
    void readSentinel_missingFile() {
        assertNull(RestartSentinel.readSentinel(tempDir));
    }

    @Test
    void payloadWithStats() throws IOException {
        RestartSentinel.Payload payload = RestartSentinel.Payload.of(
                "update", "ok", "done",
                Map.of("sessions", 5, "uptime", 3600));

        RestartSentinel.writeSentinel(tempDir, payload);
        RestartSentinel.Sentinel sentinel = RestartSentinel.readSentinel(tempDir);

        assertNotNull(sentinel.payload().stats());
        assertEquals(5, sentinel.payload().stats().get("sessions"));
    }
}

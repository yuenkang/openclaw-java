package com.openclaw.agent.runtime.subscribe;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Map;

/**
 * Raw stream JSONL logger for debug/diagnostics.
 * Corresponds to TypeScript pi-embedded-subscribe.raw-stream.ts.
 */
@Slf4j
public final class RawStreamHandler {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final boolean RAW_STREAM_ENABLED;
    private static final Path RAW_STREAM_PATH;
    private static boolean rawStreamReady = false;

    static {
        String envEnabled = System.getenv("OPENCLAW_RAW_STREAM");
        RAW_STREAM_ENABLED = "1".equals(envEnabled) || "true".equalsIgnoreCase(envEnabled);
        String pathEnv = System.getenv("OPENCLAW_RAW_STREAM_PATH");
        if (pathEnv != null && !pathEnv.isBlank()) {
            RAW_STREAM_PATH = Path.of(pathEnv.trim());
        } else {
            RAW_STREAM_PATH = Path.of(
                    System.getProperty("user.home"), ".openclaw", "logs", "raw-stream.jsonl");
        }
    }

    private RawStreamHandler() {
    }

    /**
     * Append a payload to the raw stream JSONL file (fire-and-forget).
     */
    public static void appendRawStream(Map<String, Object> payload) {
        if (!RAW_STREAM_ENABLED)
            return;
        try {
            if (!rawStreamReady) {
                rawStreamReady = true;
                Files.createDirectories(RAW_STREAM_PATH.getParent());
            }
            String line = MAPPER.writeValueAsString(payload) + "\n";
            Files.writeString(RAW_STREAM_PATH, line,
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        } catch (IOException e) {
            // ignore raw stream write failures
        }
    }
}

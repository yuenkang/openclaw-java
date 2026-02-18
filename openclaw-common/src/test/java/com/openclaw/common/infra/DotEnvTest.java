package com.openclaw.common.infra;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DotEnvTest {

    @TempDir
    Path tempDir;

    @BeforeEach
    void setUp() throws IOException {
        // Create a .env file in tempDir
        Path envFile = tempDir.resolve(".env");
        Files.writeString(envFile, """
                # comment line
                FOO=bar
                BAZ="quoted value"
                SINGLE='single quoted'
                export EXPORTED=yes
                EMPTY=
                """);
    }

    @Test
    void parseEnvFile_basicParsing() {
        Path envFile = tempDir.resolve(".env");
        Map<String, String> parsed = DotEnv.parseEnvFile(envFile);

        assertEquals("bar", parsed.get("FOO"));
        assertEquals("quoted value", parsed.get("BAZ"));
        assertEquals("single quoted", parsed.get("SINGLE"));
        assertEquals("yes", parsed.get("EXPORTED"));
        assertEquals("", parsed.get("EMPTY"));
        assertFalse(parsed.containsKey("#"));
    }

    @Test
    void parseEnvFile_nonExistentFile() {
        Map<String, String> parsed = DotEnv.parseEnvFile(tempDir.resolve("missing.env"));
        assertTrue(parsed.isEmpty());
    }

    @Test
    void loadDotEnv_doesNotOverrideExisting() {
        Map<String, String> target = new HashMap<>();
        target.put("FOO", "original");

        DotEnv.loadFile(tempDir.resolve(".env"), target, false, true);

        assertEquals("original", target.get("FOO"), "existing value should not be overridden");
        assertEquals("quoted value", target.get("BAZ"), "new value should be added");
    }

    @Test
    void loadDotEnv_overrideMode() {
        Map<String, String> target = new HashMap<>();
        target.put("FOO", "original");

        DotEnv.loadFile(tempDir.resolve(".env"), target, true, true);

        assertEquals("bar", target.get("FOO"), "existing value should be overridden");
    }

    @Test
    void loadDotEnv_globalFallback() throws IOException {
        Path stateDir = tempDir.resolve("state");
        Files.createDirectories(stateDir);
        Files.writeString(stateDir.resolve(".env"), "GLOBAL_KEY=global_value\n");

        Map<String, String> target = new HashMap<>();
        DotEnv.loadDotEnv(stateDir, target, true);

        assertEquals("global_value", target.get("GLOBAL_KEY"));
    }

    @Test
    void upsertSharedEnvVar_createsNewFile() {
        Path stateDir = tempDir.resolve("upsert-test");
        DotEnv.UpsertResult result = DotEnv.upsertSharedEnvVar(stateDir, "NEW_KEY", "new_value");

        assertTrue(result.created());
        assertTrue(result.updated());
        assertTrue(Files.exists(result.path()));
    }

    @Test
    void upsertSharedEnvVar_updatesExisting() throws IOException {
        Path stateDir = tempDir.resolve("upsert-update");
        Files.createDirectories(stateDir);
        Files.writeString(stateDir.resolve(".env"), "MY_KEY=old\nOTHER=keep\n");

        DotEnv.UpsertResult result = DotEnv.upsertSharedEnvVar(stateDir, "MY_KEY", "new");

        assertFalse(result.created());
        assertTrue(result.updated());

        String content = Files.readString(result.path());
        assertTrue(content.contains("MY_KEY=new"));
        assertTrue(content.contains("OTHER=keep"));
    }
}

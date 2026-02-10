package com.openclaw.common.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

class ConfigServiceTest {

    @TempDir
    Path tempDir;
    private Path configPath;

    @BeforeEach
    void setUp() {
        configPath = tempDir.resolve("config.json");
    }

    @Test
    void loadConfig_validJson_returnsConfig() throws IOException {
        String json = """
                {
                  "gateway": {
                    "port": 4000,
                    "password": "test-pass"
                  },
                  "logging": {
                    "level": "debug"
                  }
                }
                """;
        Files.writeString(configPath, json);

        ConfigService service = new ConfigService(configPath);
        OpenClawConfig config = service.loadConfig();

        assertNotNull(config);
        assertNotNull(config.getGateway());
        assertEquals(4000, config.getGateway().getPort());
        assertEquals("test-pass", config.getGateway().getPassword());
    }

    @Test
    void loadConfig_missingFile_returnsDefault() {
        ConfigService service = new ConfigService(tempDir.resolve("nonexistent.json"));
        OpenClawConfig config = service.loadConfig();

        assertNotNull(config);
    }

    @Test
    void substituteEnvVars_plainString_noChange() {
        ConfigService service = new ConfigService(configPath);
        assertEquals("hello", service.substituteEnvVars("hello"));
    }

    @Test
    void substituteEnvVars_withDefault_usesDefault() {
        ConfigService service = new ConfigService(configPath);
        // Using a var that almost certainly doesn't exist
        String result = service.substituteEnvVars("${__UNLIKELY_VAR_XYZ:-fallback}");
        assertEquals("fallback", result);
    }

    @Test
    void loadConfig_isCached() throws IOException {
        String json = """
                { "gateway": { "port": 5000 } }
                """;
        Files.writeString(configPath, json);

        ConfigService service = new ConfigService(configPath);
        OpenClawConfig first = service.loadConfig();
        OpenClawConfig second = service.loadConfig();

        // Should return same cached instance
        assertSame(first, second);
    }
}

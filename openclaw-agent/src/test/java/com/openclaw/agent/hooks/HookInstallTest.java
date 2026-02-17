package com.openclaw.agent.hooks;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class HookInstallTest {

    @Nested
    class ValidateHookId {
        @Test
        void validId() {
            assertNull(HookInstall.validateHookId("my-hook"));
        }

        @Test
        void nullOrBlank() {
            assertNotNull(HookInstall.validateHookId(null));
            assertNotNull(HookInstall.validateHookId(""));
            assertNotNull(HookInstall.validateHookId("  "));
        }

        @Test
        void reservedPaths() {
            assertNotNull(HookInstall.validateHookId("."));
            assertNotNull(HookInstall.validateHookId(".."));
        }

        @Test
        void pathSeparators() {
            assertNotNull(HookInstall.validateHookId("foo/bar"));
            assertNotNull(HookInstall.validateHookId("foo\\bar"));
        }
    }

    @Nested
    class UnscopedPackageName {
        @Test
        void scopedName() {
            assertEquals("hook-test", HookInstall.unscopedPackageName("@openclaw/hook-test"));
        }

        @Test
        void unscopedName() {
            assertEquals("my-hook", HookInstall.unscopedPackageName("my-hook"));
        }

        @Test
        void nullOrEmpty() {
            assertEquals("", HookInstall.unscopedPackageName(null));
            assertEquals("", HookInstall.unscopedPackageName(""));
        }
    }

    @Nested
    class SafeDirName {
        @Test
        void slashesReplaced() {
            assertEquals("foo__bar", HookInstall.safeDirName("foo/bar"));
            assertEquals("foo__bar", HookInstall.safeDirName("foo\\bar"));
        }
    }

    @Nested
    class ResolveSafeInstallDir {
        @Test
        void normalPath(@TempDir Path tmp) {
            String result = HookInstall.resolveSafeInstallDir(tmp.toString(), "my-hook");
            assertTrue(result.endsWith("my-hook"));
        }

        @Test
        void pathTraversal_throws(@TempDir Path tmp) {
            assertThrows(IllegalArgumentException.class, () -> HookInstall.resolveSafeInstallDir(tmp.toString(), ".."));
        }
    }

    @Nested
    class ValidateHookDir {
        @Test
        void missingDir() {
            var errors = HookInstall.validateHookDir("/nonexistent/path/12345");
            assertFalse(errors.isEmpty());
        }

        @Test
        void validHookDir(@TempDir Path tmp) throws IOException {
            Files.writeString(tmp.resolve("HOOK.md"), "---\nname: test\n---\n# Hook");
            Files.writeString(tmp.resolve("handler.sh"), "#!/bin/sh\necho ok");
            var errors = HookInstall.validateHookDir(tmp.toString());
            assertTrue(errors.isEmpty());
        }

        @Test
        void missingHookMd(@TempDir Path tmp) throws IOException {
            Files.writeString(tmp.resolve("handler.sh"), "#!/bin/sh");
            var errors = HookInstall.validateHookDir(tmp.toString());
            assertTrue(errors.stream().anyMatch(e -> e.contains("HOOK.md")));
        }

        @Test
        void missingHandler(@TempDir Path tmp) throws IOException {
            Files.writeString(tmp.resolve("HOOK.md"), "---\nname: test\n---\n");
            var errors = HookInstall.validateHookDir(tmp.toString());
            assertTrue(errors.stream().anyMatch(e -> e.contains("handler")));
        }
    }

    @Nested
    class RecordHookInstall {
        @Test
        void newInstall() {
            var record = new HookInstall.HookInstallRecord(
                    "my-hook", "local", "1.0.0", "2026-02-18T00:00:00Z");
            var result = HookInstall.recordHookInstall(null, record);

            assertTrue(result.containsKey("my-hook"));
            assertEquals("local", result.get("my-hook").get("source"));
            assertEquals("1.0.0", result.get("my-hook").get("version"));
        }

        @Test
        void updateExisting() {
            Map<String, Map<String, String>> existing = new LinkedHashMap<>();
            existing.put("my-hook", new LinkedHashMap<>(Map.of("source", "old", "version", "0.9")));

            var record = new HookInstall.HookInstallRecord(
                    "my-hook", "local", "1.0.0", null);
            var result = HookInstall.recordHookInstall(existing, record);

            assertEquals("local", result.get("my-hook").get("source"));
            assertEquals("1.0.0", result.get("my-hook").get("version"));
            assertNotNull(result.get("my-hook").get("installedAt"));
        }
    }
}

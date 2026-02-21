package com.openclaw.hooks;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class HookConfigTest {

    @Nested
    class IsTruthy {
        @Test
        void nullIsFalsy() {
            assertFalse(HookConfig.isTruthy(null));
        }

        @Test
        void booleanValues() {
            assertTrue(HookConfig.isTruthy(true));
            assertFalse(HookConfig.isTruthy(false));
        }

        @Test
        void numericValues() {
            assertTrue(HookConfig.isTruthy(1));
            assertTrue(HookConfig.isTruthy(-1));
            assertFalse(HookConfig.isTruthy(0));
        }

        @Test
        void stringValues() {
            assertTrue(HookConfig.isTruthy("hello"));
            assertFalse(HookConfig.isTruthy(""));
            assertFalse(HookConfig.isTruthy("   "));
        }

        @Test
        void objectIsTruthy() {
            assertTrue(HookConfig.isTruthy(new Object()));
        }
    }

    @Nested
    class HasBinary {
        @Test
        void findsCommonBinary() {
            // "ls" should exist on macOS/Linux
            assertTrue(HookConfig.hasBinary("ls"));
        }

        @Test
        void missingBinaryReturnsFalse() {
            assertFalse(HookConfig.hasBinary("nonexistent_binary_xyz_12345"));
        }

        @Test
        void nullReturnsFalse() {
            assertFalse(HookConfig.hasBinary(null));
        }
    }

    @Nested
    class ResolveRuntimePlatform {
        @Test
        void returnsKnownPlatform() {
            String platform = HookConfig.resolveRuntimePlatform();
            assertTrue(List.of("darwin", "linux", "win32").contains(platform));
        }
    }

    @Nested
    class ShouldIncludeHook {
        @Test
        void nullEntry_returnsFalse() {
            assertFalse(HookConfig.shouldIncludeHook(null, null));
        }

        @Test
        void simpleEntry_noMetadata_returnsTrue() {
            var hook = HookTypes.Hook.builder()
                    .name("test-hook")
                    .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                    .build();
            var entry = HookTypes.HookEntry.builder().hook(hook).build();

            assertTrue(HookConfig.shouldIncludeHook(entry, null));
        }

        @Test
        void alwaysHook_bypassesChecks() {
            var hook = HookTypes.Hook.builder()
                    .name("always-hook")
                    .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                    .build();
            var metadata = HookTypes.HookMetadata.builder()
                    .always(true)
                    .requires(HookTypes.HookRequirements.builder()
                            .bins(List.of("nonexistent_binary_xyz"))
                            .build())
                    .build();
            var entry = HookTypes.HookEntry.builder()
                    .hook(hook).metadata(metadata).build();

            assertTrue(HookConfig.shouldIncludeHook(entry, null));
        }

        @Test
        void missingBinary_excluded() {
            var hook = HookTypes.Hook.builder()
                    .name("needs-binary")
                    .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                    .build();
            var metadata = HookTypes.HookMetadata.builder()
                    .requires(HookTypes.HookRequirements.builder()
                            .bins(List.of("nonexistent_binary_xyz_999"))
                            .build())
                    .build();
            var entry = HookTypes.HookEntry.builder()
                    .hook(hook).metadata(metadata).build();

            assertFalse(HookConfig.shouldIncludeHook(entry, null));
        }

        @Test
        void wrongOs_excluded() {
            var hook = HookTypes.Hook.builder()
                    .name("os-hook")
                    .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                    .build();
            var metadata = HookTypes.HookMetadata.builder()
                    .os(List.of("windows_99_fake"))
                    .build();
            var entry = HookTypes.HookEntry.builder()
                    .hook(hook).metadata(metadata).build();

            assertFalse(HookConfig.shouldIncludeHook(entry, null));
        }
    }
}

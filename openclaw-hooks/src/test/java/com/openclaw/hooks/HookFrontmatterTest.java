package com.openclaw.hooks;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class HookFrontmatterTest {

    @Nested
    class ParseFrontmatter {
        @Test
        void emptyContent_returnsEmptyMap() {
            assertEquals(Map.of(), HookFrontmatter.parseFrontmatter(null));
            assertEquals(Map.of(), HookFrontmatter.parseFrontmatter(""));
            assertEquals(Map.of(), HookFrontmatter.parseFrontmatter("   "));
        }

        @Test
        void validFrontmatter_extractsKeyValues() {
            String content = """
                    ---
                    name: test-hook
                    enabled: true
                    ---
                    # Hook body
                    """;
            Map<String, String> result = HookFrontmatter.parseFrontmatter(content);
            assertEquals("test-hook", result.get("name"));
            assertEquals("true", result.get("enabled"));
        }

        @Test
        void noFrontmatter_returnsEmptyMap() {
            String content = "# Just a markdown file\nNo frontmatter here.";
            Map<String, String> result = HookFrontmatter.parseFrontmatter(content);
            assertTrue(result.isEmpty());
        }
    }

    @Nested
    class ResolveOpenClawMetadata {
        @Test
        void nullFrontmatter_returnsNull() {
            assertNull(HookFrontmatter.resolveOpenClawMetadata(null));
        }

        @Test
        void emptyFrontmatter_returnsNull() {
            assertNull(HookFrontmatter.resolveOpenClawMetadata(Map.of()));
        }

        @Test
        void noMetadataKey_returnsNull() {
            assertNull(HookFrontmatter.resolveOpenClawMetadata(
                    Map.of("name", "test")));
        }

        @Test
        void invalidJson_returnsNull() {
            assertNull(HookFrontmatter.resolveOpenClawMetadata(
                    Map.of("metadata", "not valid json {")));
        }

        @Test
        void validMetadata_parsesFields() {
            String json = """
                    {"openclaw": {"always": true, "emoji": "🎭", "hookKey": "my-hook", "events": ["agent:bootstrap", "tool:before"]}}
                    """;
            var result = HookFrontmatter.resolveOpenClawMetadata(
                    Map.of("metadata", json));
            assertNotNull(result);
            assertTrue(result.getAlways());
            assertEquals("🎭", result.getEmoji());
            assertEquals("my-hook", result.getHookKey());
            assertEquals(List.of("agent:bootstrap", "tool:before"), result.getEvents());
        }

        @Test
        void validMetadata_parsesRequirements() {
            String json = """
                    {"openclaw": {"requires": {"bins": ["docker", "git"], "env": ["OPENAI_API_KEY"]}}}
                    """;
            var result = HookFrontmatter.resolveOpenClawMetadata(
                    Map.of("metadata", json));
            assertNotNull(result);
            assertNotNull(result.getRequires());
            assertEquals(List.of("docker", "git"), result.getRequires().getBins());
            assertEquals(List.of("OPENAI_API_KEY"), result.getRequires().getEnv());
        }

        @Test
        void validMetadata_parsesInstallSpecs() {
            String json = """
                    {"openclaw": {"install": [{"kind": "npm", "package": "@openclaw/hook-test", "bins": ["test-bin"]}]}}
                    """;
            var result = HookFrontmatter.resolveOpenClawMetadata(
                    Map.of("metadata", json));
            assertNotNull(result);
            assertNotNull(result.getInstall());
            assertEquals(1, result.getInstall().size());
            assertEquals(HookTypes.InstallKind.NPM, result.getInstall().get(0).getKind());
            assertEquals("@openclaw/hook-test", result.getInstall().get(0).getPackageName());
        }

        @Test
        void legacyManifestKey_alsoWorks() {
            String json = """
                    {"pi": {"emoji": "🤖", "hookKey": "legacy-hook"}}
                    """;
            var result = HookFrontmatter.resolveOpenClawMetadata(
                    Map.of("metadata", json));
            assertNotNull(result);
            assertEquals("🤖", result.getEmoji());
            assertEquals("legacy-hook", result.getHookKey());
        }
    }

    @Nested
    class ResolveHookInvocationPolicy {
        @Test
        void nullFrontmatter_defaultsEnabled() {
            var policy = HookFrontmatter.resolveHookInvocationPolicy(null);
            assertTrue(policy.isEnabled());
        }

        @Test
        void noEnabledKey_defaultsEnabled() {
            var policy = HookFrontmatter.resolveHookInvocationPolicy(Map.of("name", "test"));
            assertTrue(policy.isEnabled());
        }

        @Test
        void enabledFalse_returnsDisabled() {
            var policy = HookFrontmatter.resolveHookInvocationPolicy(
                    Map.of("enabled", "false"));
            assertFalse(policy.isEnabled());
        }

        @Test
        void enabledTrue_returnsEnabled() {
            var policy = HookFrontmatter.resolveHookInvocationPolicy(
                    Map.of("enabled", "true"));
            assertTrue(policy.isEnabled());
        }
    }

    @Nested
    class ResolveHookKey {
        @Test
        void noEntry_returnsHookName() {
            assertEquals("my-hook", HookFrontmatter.resolveHookKey("my-hook", null));
        }

        @Test
        void noMetadata_returnsHookName() {
            var entry = HookTypes.HookEntry.builder()
                    .hook(HookTypes.Hook.builder().name("test").build())
                    .build();
            assertEquals("test-hook", HookFrontmatter.resolveHookKey("test-hook", entry));
        }

        @Test
        void metadataWithHookKey_returnsOverride() {
            var metadata = HookTypes.HookMetadata.builder()
                    .hookKey("custom-key")
                    .build();
            var entry = HookTypes.HookEntry.builder()
                    .hook(HookTypes.Hook.builder().name("test").build())
                    .metadata(metadata)
                    .build();
            assertEquals("custom-key", HookFrontmatter.resolveHookKey("test-hook", entry));
        }
    }

    @Nested
    class NormalizeStringList {
        @Test
        void nullInput_returnsEmpty() {
            assertEquals(List.of(), HookFrontmatter.normalizeStringList(null));
        }

        @Test
        void listInput_normalizes() {
            assertEquals(List.of("a", "b"),
                    HookFrontmatter.normalizeStringList(List.of("a", " b ")));
        }

        @Test
        void csvInput_splits() {
            assertEquals(List.of("darwin", "linux"),
                    HookFrontmatter.normalizeStringList("darwin, linux"));
        }

        @Test
        void emptyEntries_filtered() {
            assertEquals(List.of("a"),
                    HookFrontmatter.normalizeStringList("a, , "));
        }
    }
}

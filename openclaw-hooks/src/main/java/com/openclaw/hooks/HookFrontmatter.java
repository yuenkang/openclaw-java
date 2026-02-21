package com.openclaw.hooks;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.common.markdown.MarkdownFrontmatter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Hook frontmatter resolver — parses HOOK.md frontmatter into structured
 * hook metadata, invocation policy, and hook keys.
 * Corresponds to TypeScript's hooks/frontmatter.ts.
 */
@Slf4j
public final class HookFrontmatter {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Known manifest keys (current + legacy). */
    private static final List<String> MANIFEST_KEYS = List.of(
            "openclaw", "pi", "openclaw-hook");

    private HookFrontmatter() {
    }

    /**
     * Parse frontmatter from a HOOK.md file.
     *
     * @return parsed frontmatter key-value map, or empty map if none found
     */
    public static Map<String, String> parseFrontmatter(String content) {
        if (content == null || content.isBlank()) {
            return Map.of();
        }
        MarkdownFrontmatter.FrontmatterResult result = MarkdownFrontmatter.extract(content);
        return result.data();
    }

    /**
     * Resolve structured hook metadata from a HOOK.md frontmatter.
     * Reads the "metadata" field as JSON5/JSON, then extracts the
     * openclaw-specific manifest block.
     *
     * @param frontmatter parsed key-value frontmatter map
     * @return structured metadata, or null if not found
     */
    @SuppressWarnings("unchecked")
    public static HookTypes.HookMetadata resolveOpenClawMetadata(
            Map<String, String> frontmatter) {
        if (frontmatter == null || frontmatter.isEmpty()) {
            return null;
        }

        String raw = frontmatter.get("metadata");
        if (raw == null || raw.isBlank()) {
            return null;
        }

        try {
            Map<String, Object> parsed = MAPPER.readValue(raw, Map.class);
            if (parsed == null) {
                return null;
            }

            // Look for manifest block under known keys
            Map<String, Object> metadataObj = null;
            for (String key : MANIFEST_KEYS) {
                Object candidate = parsed.get(key);
                if (candidate instanceof Map<?, ?> m) {
                    metadataObj = (Map<String, Object>) m;
                    break;
                }
            }
            if (metadataObj == null) {
                return null;
            }

            // Build HookMetadata
            HookTypes.HookMetadata.HookMetadataBuilder builder = HookTypes.HookMetadata.builder();

            if (metadataObj.get("always") instanceof Boolean b) {
                builder.always(b);
            }
            if (metadataObj.get("hookKey") instanceof String s) {
                builder.hookKey(s);
            }
            if (metadataObj.get("emoji") instanceof String s) {
                builder.emoji(s);
            }
            if (metadataObj.get("homepage") instanceof String s) {
                builder.homepage(s);
            }
            if (metadataObj.get("export") instanceof String s) {
                builder.exportName(s);
            }

            builder.os(normalizeStringList(metadataObj.get("os")));
            builder.events(normalizeStringList(metadataObj.get("events")));

            // Requirements
            Object reqRaw = metadataObj.get("requires");
            if (reqRaw instanceof Map<?, ?> reqMap) {
                Map<String, Object> reqObj = (Map<String, Object>) reqMap;
                builder.requires(HookTypes.HookRequirements.builder()
                        .bins(normalizeStringList(reqObj.get("bins")))
                        .anyBins(normalizeStringList(reqObj.get("anyBins")))
                        .env(normalizeStringList(reqObj.get("env")))
                        .config(normalizeStringList(reqObj.get("config")))
                        .build());
            }

            // Install specs
            if (metadataObj.get("install") instanceof List<?> installList) {
                List<HookTypes.HookInstallSpec> installSpecs = new ArrayList<>();
                for (Object item : installList) {
                    HookTypes.HookInstallSpec spec = parseInstallSpec(item);
                    if (spec != null) {
                        installSpecs.add(spec);
                    }
                }
                if (!installSpecs.isEmpty()) {
                    builder.install(installSpecs);
                }
            }

            return builder.build();
        } catch (Exception e) {
            log.debug("Failed to parse hook metadata: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Resolve hook invocation policy from frontmatter.
     *
     * @return invocation policy (defaults to enabled=true)
     */
    public static HookTypes.HookInvocationPolicy resolveHookInvocationPolicy(
            Map<String, String> frontmatter) {
        boolean enabled = true;
        if (frontmatter != null) {
            String enabledStr = frontmatter.get("enabled");
            if (enabledStr != null) {
                enabled = parseBooleanValue(enabledStr);
            }
        }
        return HookTypes.HookInvocationPolicy.builder()
                .enabled(enabled)
                .build();
    }

    /**
     * Resolve the hook key for config lookup.
     * Uses metadata.hookKey if available, otherwise the hook name.
     *
     * @param hookName default hook name
     * @param entry    hook entry (may contain metadata with hookKey override)
     * @return resolved hook key
     */
    public static String resolveHookKey(String hookName, HookTypes.HookEntry entry) {
        if (entry != null && entry.getMetadata() != null
                && entry.getMetadata().getHookKey() != null) {
            return entry.getMetadata().getHookKey();
        }
        return hookName;
    }

    // -----------------------------------------------------------------------
    // Internal helpers
    // -----------------------------------------------------------------------

    /**
     * Normalize an input value to a List<String>.
     * Handles: arrays, comma-separated strings, null → empty list.
     */
    @SuppressWarnings("unchecked")
    static List<String> normalizeStringList(Object input) {
        if (input == null) {
            return List.of();
        }
        if (input instanceof List<?> list) {
            return list.stream()
                    .map(v -> String.valueOf(v).trim())
                    .filter(s -> !s.isEmpty())
                    .toList();
        }
        if (input instanceof String str) {
            return Arrays.stream(str.split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .toList();
        }
        return List.of();
    }

    /**
     * Parse an install spec from a raw object.
     */
    @SuppressWarnings("unchecked")
    private static HookTypes.HookInstallSpec parseInstallSpec(Object input) {
        if (!(input instanceof Map<?, ?> rawMap)) {
            return null;
        }
        Map<String, Object> raw = (Map<String, Object>) rawMap;

        // Resolve kind
        String kindRaw = null;
        if (raw.get("kind") instanceof String s)
            kindRaw = s;
        else if (raw.get("type") instanceof String s)
            kindRaw = s;
        if (kindRaw == null)
            return null;

        HookTypes.InstallKind kind = switch (kindRaw.trim().toLowerCase()) {
            case "bundled" -> HookTypes.InstallKind.BUNDLED;
            case "npm" -> HookTypes.InstallKind.NPM;
            case "git" -> HookTypes.InstallKind.GIT;
            default -> null;
        };
        if (kind == null)
            return null;

        HookTypes.HookInstallSpec.HookInstallSpecBuilder builder = HookTypes.HookInstallSpec.builder().kind(kind);

        if (raw.get("id") instanceof String s)
            builder.id(s);
        if (raw.get("label") instanceof String s)
            builder.label(s);
        if (raw.get("package") instanceof String s)
            builder.packageName(s);
        if (raw.get("repository") instanceof String s)
            builder.repository(s);

        List<String> bins = normalizeStringList(raw.get("bins"));
        if (!bins.isEmpty())
            builder.bins(bins);

        return builder.build();
    }

    /**
     * Parse a string to boolean (true/false/yes/no/1/0).
     */
    private static boolean parseBooleanValue(String value) {
        if (value == null)
            return true;
        String lower = value.trim().toLowerCase();
        return switch (lower) {
            case "false", "no", "0", "off", "disabled" -> false;
            default -> true;
        };
    }
}

package com.openclaw.media;

import java.util.*;

/**
 * Resolve media understanding configuration — timeouts, prompts, max sizes,
 * model entries, and concurrency.
 * Corresponds to TypeScript's media-understanding/resolve.ts.
 */
public final class MediaResolver {

    private MediaResolver() {
    }

    // =========================================================================
    // Default constants
    // =========================================================================

    public static final int DEFAULT_MEDIA_CONCURRENCY = 3;
    public static final Map<MediaTypes.Capability, String> DEFAULT_PROMPT = Map.of(
            MediaTypes.Capability.AUDIO, "Transcribe this audio. Return only the transcription.",
            MediaTypes.Capability.IMAGE, "Describe this image concisely.",
            MediaTypes.Capability.VIDEO, "Describe this video concisely.");
    public static final Map<MediaTypes.Capability, Integer> DEFAULT_MAX_CHARS = Map.of(
            MediaTypes.Capability.IMAGE, 2000,
            MediaTypes.Capability.VIDEO, 4000);
    public static final Map<MediaTypes.Capability, Long> DEFAULT_MAX_BYTES = Map.of(
            MediaTypes.Capability.IMAGE, 20L * 1024 * 1024,
            MediaTypes.Capability.AUDIO, 25L * 1024 * 1024,
            MediaTypes.Capability.VIDEO, 100L * 1024 * 1024);
    public static final int DEFAULT_TIMEOUT_SECONDS = 30;

    // =========================================================================
    // Resolution methods
    // =========================================================================

    /**
     * Resolve timeout in milliseconds.
     */
    public static int resolveTimeoutMs(Integer seconds, int fallbackSeconds) {
        int value = (seconds != null && seconds > 0) ? seconds : fallbackSeconds;
        return Math.max(1000, value * 1000);
    }

    /**
     * Resolve the prompt for a capability.
     */
    public static String resolvePrompt(MediaTypes.Capability capability,
            String prompt, Integer maxChars) {
        String base = (prompt != null && !prompt.isBlank())
                ? prompt.trim()
                : DEFAULT_PROMPT.getOrDefault(capability, "Describe this media.");
        if (maxChars == null || capability == MediaTypes.Capability.AUDIO) {
            return base;
        }
        return base + " Respond in at most " + maxChars + " characters.";
    }

    /**
     * Resolve max characters for a capability output.
     */
    public static Integer resolveMaxChars(MediaTypes.Capability capability) {
        return DEFAULT_MAX_CHARS.get(capability);
    }

    /**
     * Resolve max bytes for a capability input.
     */
    public static long resolveMaxBytes(MediaTypes.Capability capability) {
        return DEFAULT_MAX_BYTES.getOrDefault(capability, 20L * 1024 * 1024);
    }

    /**
     * Resolve concurrency setting.
     */
    public static int resolveConcurrency() {
        return DEFAULT_MEDIA_CONCURRENCY;
    }

    // =========================================================================
    // Model entry resolution
    // =========================================================================

    /**
     * Model entry configuration — which provider/model to use for media
     * understanding.
     */
    public record ModelEntry(
            String type, // "provider" or "cli"
            String provider,
            String model,
            String command,
            List<MediaTypes.Capability> capabilities,
            Integer maxChars,
            Long maxBytes,
            String prompt,
            Integer timeoutSeconds) {

        public ModelEntry {
            if (type == null) {
                type = (command != null && !command.isBlank()) ? "cli" : "provider";
            }
        }
    }

    /**
     * Resolve model entries for a capability, filtering by capability support.
     *
     * @param models           configured model entries
     * @param capability       target capability
     * @param providerRegistry registry to look up provider capabilities
     * @return filtered and ordered model entries
     */
    public static List<ModelEntry> resolveModelEntries(
            List<ModelEntry> models,
            MediaTypes.Capability capability,
            Map<String, List<MediaTypes.Capability>> providerRegistry) {

        if (models == null || models.isEmpty()) {
            return List.of();
        }

        return models.stream()
                .filter(entry -> {
                    List<MediaTypes.Capability> caps = entry.capabilities();
                    if (caps != null && !caps.isEmpty()) {
                        return caps.contains(capability);
                    }
                    // For provider entries, check provider registry
                    if ("provider".equals(entry.type()) && entry.provider() != null) {
                        String providerId = normalizeMediaProviderId(entry.provider());
                        List<MediaTypes.Capability> providerCaps = providerRegistry.get(providerId);
                        if (providerCaps != null && !providerCaps.isEmpty()) {
                            return providerCaps.contains(capability);
                        }
                    }
                    // CLI entries or unknown capabilities — allow
                    return true;
                })
                .toList();
    }

    // =========================================================================
    // Provider ID normalization
    // =========================================================================

    /**
     * Normalize a media provider ID.
     */
    public static String normalizeMediaProviderId(String id) {
        if (id == null)
            return "";
        String normalized = id.trim().toLowerCase();
        if ("gemini".equals(normalized)) {
            return "google";
        }
        return normalized;
    }
}

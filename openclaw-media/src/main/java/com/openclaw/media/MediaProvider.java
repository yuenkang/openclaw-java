package com.openclaw.media;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Media understanding provider registry — manages available providers
 * and their capabilities.
 * Corresponds to TypeScript's media-understanding/providers/index.ts.
 */
public final class MediaProvider {

    private MediaProvider() {
    }

    // =========================================================================
    // Provider interface
    // =========================================================================

    /**
     * A media understanding provider that can handle audio/image/video.
     */
    public interface Provider {
        String id();

        default List<MediaTypes.Capability> capabilities() {
            return List.of();
        }

        default MediaTypes.AudioTranscriptionResult transcribeAudio(
                MediaTypes.AudioTranscriptionRequest request) {
            throw new UnsupportedOperationException(
                    "Provider " + id() + " does not support audio transcription");
        }

        default MediaTypes.VideoDescriptionResult describeVideo(
                MediaTypes.VideoDescriptionRequest request) {
            throw new UnsupportedOperationException(
                    "Provider " + id() + " does not support video description");
        }

        default MediaTypes.ImageDescriptionResult describeImage(
                MediaTypes.ImageDescriptionRequest request) {
            throw new UnsupportedOperationException(
                    "Provider " + id() + " does not support image description");
        }
    }

    // =========================================================================
    // Registry
    // =========================================================================

    private static final Map<String, Provider> REGISTRY = new ConcurrentHashMap<>();

    /**
     * Register a media understanding provider.
     */
    public static void register(Provider provider) {
        String normalizedId = MediaResolver.normalizeMediaProviderId(provider.id());
        REGISTRY.put(normalizedId, provider);
    }

    /**
     * Get a provider by ID.
     */
    public static Optional<Provider> get(String id) {
        String normalizedId = MediaResolver.normalizeMediaProviderId(id);
        return Optional.ofNullable(REGISTRY.get(normalizedId));
    }

    /**
     * Build a capability registry from registered providers.
     *
     * @return map from normalized provider ID to supported capabilities
     */
    public static Map<String, List<MediaTypes.Capability>> buildCapabilityRegistry() {
        Map<String, List<MediaTypes.Capability>> result = new LinkedHashMap<>();
        for (var entry : REGISTRY.entrySet()) {
            result.put(entry.getKey(), entry.getValue().capabilities());
        }
        return result;
    }

    /**
     * List all registered provider IDs.
     */
    public static List<String> listProviderIds() {
        return List.copyOf(REGISTRY.keySet());
    }

    /**
     * Clear all registered providers (for testing).
     */
    public static void clearAll() {
        REGISTRY.clear();
    }

    /**
     * Get total number of registered providers.
     */
    public static int count() {
        return REGISTRY.size();
    }
}

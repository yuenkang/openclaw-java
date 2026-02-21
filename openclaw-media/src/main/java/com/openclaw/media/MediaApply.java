package com.openclaw.media;

import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Apply media understanding — orchestrates scope checking, capability running,
 * and output formatting across all media types.
 * Corresponds to TypeScript's media-understanding/apply.ts.
 */
@Slf4j
public final class MediaApply {

    private MediaApply() {
    }

    private static final List<MediaTypes.Capability> CAPABILITY_ORDER = List.of(
            MediaTypes.Capability.IMAGE,
            MediaTypes.Capability.AUDIO,
            MediaTypes.Capability.VIDEO);

    // Common MIME prefixes for each capability
    private static final Map<MediaTypes.Capability, List<String>> MIME_PREFIXES = Map.of(
            MediaTypes.Capability.IMAGE, List.of("image/"),
            MediaTypes.Capability.AUDIO, List.of("audio/", "application/ogg"),
            MediaTypes.Capability.VIDEO, List.of("video/"));

    // =========================================================================
    // Apply
    // =========================================================================

    /**
     * Apply media understanding to a set of attachments.
     *
     * @param attachments all message attachments
     * @param scope       scope configuration (may be null)
     * @param sessionKey  current session key
     * @param channel     current channel
     * @param chatType    current chat type
     * @param models      configured model entries
     * @return apply result with outputs and decisions
     */
    public static MediaTypes.ApplyResult apply(
            List<MediaTypes.MediaAttachment> attachments,
            MediaScope.ScopeConfig scope,
            String sessionKey, String channel, String chatType,
            List<MediaResolver.ModelEntry> models) {

        if (attachments == null || attachments.isEmpty()) {
            return MediaTypes.ApplyResult.builder().build();
        }

        // Check scope
        MediaScope.ScopeDecision scopeDecision = MediaScope.resolve(scope, sessionKey, channel, chatType);
        if (scopeDecision == MediaScope.ScopeDecision.DENY) {
            List<MediaTypes.MediaDecision> decisions = CAPABILITY_ORDER.stream()
                    .map(cap -> MediaTypes.MediaDecision.builder()
                            .capability(cap)
                            .outcome(MediaTypes.DecisionOutcome.SCOPE_DENY)
                            .build())
                    .toList();
            return MediaTypes.ApplyResult.builder()
                    .decisions(decisions)
                    .build();
        }

        // Build provider capability registry
        Map<String, List<MediaTypes.Capability>> capabilityRegistry = MediaProvider.buildCapabilityRegistry();

        // Process each capability
        List<MediaTypes.MediaOutput> allOutputs = new ArrayList<>();
        List<MediaTypes.MediaDecision> allDecisions = new ArrayList<>();
        boolean appliedImage = false;
        boolean appliedAudio = false;
        boolean appliedVideo = false;

        int concurrency = MediaResolver.resolveConcurrency();

        for (MediaTypes.Capability capability : CAPABILITY_ORDER) {
            // Filter attachments by MIME type for this capability
            List<MediaTypes.MediaAttachment> capAttachments = filterAttachmentsByCapability(attachments, capability);

            // Resolve model entries for this capability
            List<MediaResolver.ModelEntry> capEntries = MediaResolver.resolveModelEntries(models, capability,
                    capabilityRegistry);

            // Run
            MediaRunner.CapabilityResult result = MediaRunner.runCapability(capability, capAttachments, capEntries,
                    concurrency);

            allOutputs.addAll(result.outputs());
            allDecisions.add(result.decision());

            if (result.decision().getOutcome() == MediaTypes.DecisionOutcome.SUCCESS) {
                switch (capability) {
                    case IMAGE -> appliedImage = true;
                    case AUDIO -> appliedAudio = true;
                    case VIDEO -> appliedVideo = true;
                }
            }
        }

        return MediaTypes.ApplyResult.builder()
                .outputs(allOutputs)
                .decisions(allDecisions)
                .appliedImage(appliedImage)
                .appliedAudio(appliedAudio)
                .appliedVideo(appliedVideo)
                .build();
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Filter attachments by MIME type matching a capability.
     */
    static List<MediaTypes.MediaAttachment> filterAttachmentsByCapability(
            List<MediaTypes.MediaAttachment> attachments,
            MediaTypes.Capability capability) {

        List<String> prefixes = MIME_PREFIXES.getOrDefault(capability, List.of());
        return attachments.stream()
                .filter(a -> {
                    String mime = a.getMime();
                    if (mime == null || mime.isBlank()) {
                        return false;
                    }
                    String lower = mime.toLowerCase();
                    return prefixes.stream().anyMatch(lower::startsWith);
                })
                .toList();
    }
}

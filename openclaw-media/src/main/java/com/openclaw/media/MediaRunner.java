package com.openclaw.media;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.*;

/**
 * Media understanding runner — executes provider-based media processing
 * (audio transcription, image/video description) with concurrency control.
 * Corresponds to TypeScript's media-understanding/runner.ts.
 *
 * <p>
 * Note: CLI-based providers (whisper, gemini-cli, sherpa-onnx) are omitted;
 * those are Node.js-specific. Java uses provider SPI instead.
 * </p>
 */
@Slf4j
public final class MediaRunner {

    private MediaRunner() {
    }

    // =========================================================================
    // Run capability
    // =========================================================================

    /**
     * Result of running a media capability against attachments.
     */
    public record CapabilityResult(
            List<MediaTypes.MediaOutput> outputs,
            MediaTypes.MediaDecision decision) {
    }

    /**
     * Run a media understanding capability against the given attachments.
     *
     * @param capability   the media capability to run
     * @param attachments  filtered attachments for this capability
     * @param modelEntries configured model entries to try
     * @param concurrency  max concurrent processing tasks
     * @return outputs and decision
     */
    public static CapabilityResult runCapability(
            MediaTypes.Capability capability,
            List<MediaTypes.MediaAttachment> attachments,
            List<MediaResolver.ModelEntry> modelEntries,
            int concurrency) {

        if (attachments.isEmpty()) {
            return new CapabilityResult(
                    List.of(),
                    MediaTypes.MediaDecision.builder()
                            .capability(capability)
                            .outcome(MediaTypes.DecisionOutcome.NO_ATTACHMENT)
                            .build());
        }

        if (modelEntries.isEmpty()) {
            return new CapabilityResult(
                    List.of(),
                    MediaTypes.MediaDecision.builder()
                            .capability(capability)
                            .outcome(MediaTypes.DecisionOutcome.SKIPPED)
                            .build());
        }

        ExecutorService executor = Executors.newFixedThreadPool(
                Math.min(concurrency, attachments.size()));
        try {
            List<Future<AttachmentProcessResult>> futures = new ArrayList<>();

            for (MediaTypes.MediaAttachment attachment : attachments) {
                futures.add(executor.submit(
                        () -> processAttachment(capability, attachment, modelEntries)));
            }

            List<MediaTypes.MediaOutput> allOutputs = new ArrayList<>();
            List<MediaTypes.AttachmentDecision> attachmentDecisions = new ArrayList<>();
            boolean anySuccess = false;

            for (Future<AttachmentProcessResult> future : futures) {
                try {
                    AttachmentProcessResult result = future.get(120, TimeUnit.SECONDS);
                    if (result.output() != null) {
                        allOutputs.add(result.output());
                        anySuccess = true;
                    }
                    attachmentDecisions.add(result.decision());
                } catch (TimeoutException e) {
                    log.warn("Media processing timed out for attachment");
                } catch (Exception e) {
                    log.error("Media processing failed for attachment", e);
                }
            }

            MediaTypes.DecisionOutcome outcome = anySuccess
                    ? MediaTypes.DecisionOutcome.SUCCESS
                    : MediaTypes.DecisionOutcome.SKIPPED;

            return new CapabilityResult(
                    allOutputs,
                    MediaTypes.MediaDecision.builder()
                            .capability(capability)
                            .outcome(outcome)
                            .attachments(attachmentDecisions)
                            .build());
        } finally {
            executor.shutdown();
        }
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private record AttachmentProcessResult(
            MediaTypes.MediaOutput output,
            MediaTypes.AttachmentDecision decision) {
    }

    /**
     * Process a single attachment with the ordered list of model entries.
     * Tries each entry in order; returns on first success.
     */
    private static AttachmentProcessResult processAttachment(
            MediaTypes.Capability capability,
            MediaTypes.MediaAttachment attachment,
            List<MediaResolver.ModelEntry> entries) {

        List<MediaTypes.ModelDecision> attempts = new ArrayList<>();

        for (MediaResolver.ModelEntry entry : entries) {
            if (!"provider".equals(entry.type())) {
                attempts.add(MediaTypes.ModelDecision.builder()
                        .type("cli")
                        .outcome("skipped")
                        .reason("CLI providers not supported in Java")
                        .build());
                continue;
            }

            String providerId = MediaResolver.normalizeMediaProviderId(
                    entry.provider() != null ? entry.provider() : "");
            Optional<MediaProvider.Provider> provider = MediaProvider.get(providerId);

            if (provider.isEmpty()) {
                attempts.add(MediaTypes.ModelDecision.builder()
                        .provider(providerId)
                        .model(entry.model())
                        .type("provider")
                        .outcome("skipped")
                        .reason("provider not registered: " + providerId)
                        .build());
                continue;
            }

            try {
                MediaTypes.MediaOutput output = callProvider(
                        capability, provider.get(), entry, attachment);
                MediaTypes.ModelDecision decision = MediaTypes.ModelDecision.builder()
                        .provider(providerId)
                        .model(entry.model())
                        .type("provider")
                        .outcome("success")
                        .build();
                attempts.add(decision);

                return new AttachmentProcessResult(
                        output,
                        MediaTypes.AttachmentDecision.builder()
                                .attachmentIndex(attachment.getIndex())
                                .attempts(attempts)
                                .chosen(decision)
                                .build());
            } catch (Exception e) {
                log.warn("Provider {} failed: {}", providerId, e.getMessage());
                attempts.add(MediaTypes.ModelDecision.builder()
                        .provider(providerId)
                        .model(entry.model())
                        .type("provider")
                        .outcome("failed")
                        .reason(e.getMessage())
                        .build());
            }
        }

        return new AttachmentProcessResult(
                null,
                MediaTypes.AttachmentDecision.builder()
                        .attachmentIndex(attachment.getIndex())
                        .attempts(attempts)
                        .build());
    }

    /**
     * Call a provider to process an attachment.
     */
    private static MediaTypes.MediaOutput callProvider(
            MediaTypes.Capability capability,
            MediaProvider.Provider provider,
            MediaResolver.ModelEntry entry,
            MediaTypes.MediaAttachment attachment) {

        String prompt = MediaResolver.resolvePrompt(
                capability, entry.prompt(),
                MediaResolver.resolveMaxChars(capability));
        int timeoutMs = MediaResolver.resolveTimeoutMs(
                entry.timeoutSeconds(), MediaResolver.DEFAULT_TIMEOUT_SECONDS);

        // Note: In a real implementation, the attachment buffer would be
        // loaded from path/url here. For now we pass null buffer.

        return switch (capability) {
            case AUDIO -> {
                var result = provider.transcribeAudio(
                        MediaTypes.AudioTranscriptionRequest.builder()
                                .fileName(attachment.getPath())
                                .mime(attachment.getMime())
                                .model(entry.model())
                                .prompt(prompt)
                                .timeoutMs(timeoutMs)
                                .build());
                yield MediaTypes.MediaOutput.builder()
                        .kind(MediaTypes.MediaKind.AUDIO_TRANSCRIPTION)
                        .attachmentIndex(attachment.getIndex())
                        .text(result.getText())
                        .provider(provider.id())
                        .model(result.getModel())
                        .build();
            }
            case IMAGE -> {
                var result = provider.describeImage(
                        MediaTypes.ImageDescriptionRequest.builder()
                                .fileName(attachment.getPath())
                                .mime(attachment.getMime())
                                .model(entry.model())
                                .provider(provider.id())
                                .prompt(prompt)
                                .timeoutMs(timeoutMs)
                                .build());
                yield MediaTypes.MediaOutput.builder()
                        .kind(MediaTypes.MediaKind.IMAGE_DESCRIPTION)
                        .attachmentIndex(attachment.getIndex())
                        .text(result.getText())
                        .provider(provider.id())
                        .model(result.getModel())
                        .build();
            }
            case VIDEO -> {
                var result = provider.describeVideo(
                        MediaTypes.VideoDescriptionRequest.builder()
                                .fileName(attachment.getPath())
                                .mime(attachment.getMime())
                                .model(entry.model())
                                .prompt(prompt)
                                .timeoutMs(timeoutMs)
                                .build());
                yield MediaTypes.MediaOutput.builder()
                        .kind(MediaTypes.MediaKind.VIDEO_DESCRIPTION)
                        .attachmentIndex(attachment.getIndex())
                        .text(result.getText())
                        .provider(provider.id())
                        .model(result.getModel())
                        .build();
            }
        };
    }
}

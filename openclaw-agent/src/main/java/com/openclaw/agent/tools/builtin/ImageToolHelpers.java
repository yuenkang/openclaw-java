package com.openclaw.agent.tools.builtin;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.Base64;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Image tool helpers: data URL decoding, model config, vision model resolution.
 * Corresponds to TypeScript agents/tools/image-tool.helpers.ts.
 */
@Slf4j
public final class ImageToolHelpers {

    private static final Pattern DATA_URL_RE = Pattern.compile("^data:([^;,]+);base64,([a-zA-Z0-9+/=\\r\\n]+)$");

    private ImageToolHelpers() {
    }

    /**
     * Image model configuration.
     */
    public record ImageModelConfig(String primary, List<String> fallbacks) {
    }

    /**
     * Decoded data URL result.
     */
    public record DecodedDataUrl(byte[] data, String mimeType) {
    }

    /**
     * Decode a base64 data URL.
     *
     * @param dataUrl The data URL string
     * @return Decoded data and mime type
     * @throws IllegalArgumentException if the URL is invalid or not an image
     */
    public static DecodedDataUrl decodeDataUrl(String dataUrl) {
        String trimmed = dataUrl.trim();
        Matcher m = DATA_URL_RE.matcher(trimmed);
        if (!m.matches()) {
            throw new IllegalArgumentException("Invalid data URL (expected base64 data: URL).");
        }
        String mimeType = m.group(1).trim().toLowerCase();
        if (!mimeType.startsWith("image/")) {
            throw new IllegalArgumentException("Unsupported data URL type: " + mimeType);
        }
        String b64 = m.group(2).trim();
        byte[] data = Base64.getDecoder().decode(b64);
        if (data.length == 0) {
            throw new IllegalArgumentException("Invalid data URL: empty payload.");
        }
        return new DecodedDataUrl(data, mimeType);
    }

    /**
     * Extract image model configuration from OpenClaw config.
     */
    public static ImageModelConfig coerceImageModelConfig(OpenClawConfig cfg) {
        // TODO: once OpenClawConfig has agents.defaults.imageModel, extract it
        log.debug("coerceImageModelConfig: stub");
        return new ImageModelConfig(null, List.of());
    }

    /**
     * Coerce assistant message text for image generation, throwing on errors.
     *
     * @param text         Assistant text
     * @param stopReason   Stop reason from the model
     * @param errorMessage Error message from the model
     * @param provider     Provider id
     * @param model        Model id
     * @return Trimmed text
     * @throws RuntimeException if the model errored or returned empty text
     */
    public static String coerceImageAssistantText(
            String text, String stopReason, String errorMessage,
            String provider, String model) {
        String trimmedError = (errorMessage != null) ? errorMessage.trim() : null;
        if ("error".equals(stopReason) || "aborted".equals(stopReason)) {
            String msg = (trimmedError != null && !trimmedError.isEmpty())
                    ? String.format("Image model failed (%s/%s): %s", provider, model, trimmedError)
                    : String.format("Image model failed (%s/%s)", provider, model);
            throw new RuntimeException(msg);
        }
        if (trimmedError != null && !trimmedError.isEmpty()) {
            throw new RuntimeException(
                    String.format("Image model failed (%s/%s): %s", provider, model, trimmedError));
        }
        if (text != null && !text.trim().isEmpty()) {
            return text.trim();
        }
        throw new RuntimeException(
                String.format("Image model returned no text (%s/%s).", provider, model));
    }

    /**
     * Resolve a vision-capable model from provider config.
     *
     * @return provider/modelId string or null if none found
     */
    public static String resolveProviderVisionModel(OpenClawConfig cfg, String provider) {
        // TODO: once OpenClawConfig has models.providers with model lists, look up
        // vision models
        log.debug("resolveProviderVisionModel: stub for provider={}", provider);
        return null;
    }
}

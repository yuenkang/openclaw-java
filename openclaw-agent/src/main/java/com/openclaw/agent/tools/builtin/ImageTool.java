package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Image tool — analyze images using vision-capable LLMs.
 * Reads local image files, encodes to base64, and sends to a
 * vision model for analysis.
 * Corresponds to TypeScript's tools/image-tool.ts.
 */
@Slf4j
public class ImageTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final long MAX_IMAGE_BYTES = 20 * 1024 * 1024;

    private final ModelProviderRegistry modelProviderRegistry;

    /**
     * Create an ImageTool with a model provider registry for vision LLM calls.
     */
    public ImageTool(ModelProviderRegistry modelProviderRegistry) {
        this.modelProviderRegistry = modelProviderRegistry;
    }

    @Override
    public String getName() {
        return "image";
    }

    @Override
    public String getDescription() {
        return "Analyze or describe a LOCAL image file using a vision model. " +
                "IMPORTANT: Always use 'imagePath' to specify the file path. " +
                "Do NOT attempt to pass base64 image data from conversation context — " +
                "LLMs cannot reliably reproduce binary data. " +
                "If you already see an image in the conversation, describe it directly without calling this tool.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        addStr(properties, "imagePath",
                "Absolute or relative path to the image file on disk. This is the preferred way to provide an image.");
        addStr(properties, "imageBase64",
                "Base64-encoded image data or data: URL. Only use if the image is NOT on disk. " +
                        "NEVER fabricate or reproduce base64 data from conversation context.");
        addStr(properties, "prompt",
                "Question or instruction about the image (default: 'Describe the image')");
        addStr(properties, "model", "Model override for image analysis (optional)");

        ObjectNode maxBytes = properties.putObject("maxBytesMb");
        maxBytes.put("type", "number");
        maxBytes.put("description", "Max image size in MB (default: 20)");

        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        try {
            JsonNode params = context.getParameters();
            String imagePath = ToolParamUtils.readStringParam(params, "imagePath");
            String imageBase64 = ToolParamUtils.readStringParam(params, "imageBase64");
            String prompt = ToolParamUtils.readStringParam(params, "prompt");
            String modelOverride = ToolParamUtils.readStringParam(params, "model");
            if (prompt == null || prompt.isBlank())
                prompt = "Describe the image.";

            String base64Data;
            String mimeType = "image/png";

            if (imagePath != null && !imagePath.isBlank()) {
                Path path = resolvePath(imagePath, context.getCwd());
                if (!Files.exists(path))
                    return ToolResult.fail("Image not found: " + path);
                long size = Files.size(path);
                if (size > MAX_IMAGE_BYTES) {
                    return ToolResult.fail(String.format(
                            "Image too large: %d bytes (max %d MB)",
                            size, MAX_IMAGE_BYTES / (1024 * 1024)));
                }
                base64Data = Base64.getEncoder().encodeToString(Files.readAllBytes(path));
                mimeType = guessMimeType(path.toString());
            } else if (imageBase64 != null && !imageBase64.isBlank()) {
                if (imageBase64.startsWith("data:")) {
                    var decoded = decodeDataUrl(imageBase64);
                    base64Data = decoded.data;
                    mimeType = decoded.mimeType;
                } else {
                    base64Data = imageBase64;
                }
            } else {
                return ToolResult.fail("Either 'imagePath' or 'imageBase64' is required");
            }

            // Verify/correct MIME type from actual file magic bytes
            try {
                byte[] imageBytes = Base64.getDecoder().decode(base64Data);
                String detectedMime = detectMimeFromBytes(imageBytes);
                if (detectedMime != null) {
                    if (!detectedMime.equals(mimeType)) {
                        log.info("image-tool: correcting mime from {} to {} (magic bytes)", mimeType, detectedMime);
                    }
                    mimeType = detectedMime;
                }
            } catch (IllegalArgumentException e) {
                log.warn("image-tool: invalid base64 data, cannot verify mime type");
            }

            log.info("image-tool: prompt='{}' mime={} dataLen={}", prompt, mimeType, base64Data.length());

            // Build data URI for the image
            String dataUri = "data:" + mimeType + ";base64," + base64Data;

            // Construct multimodal message with image + text
            List<ModelProvider.ContentPart> contentParts = new ArrayList<>();
            contentParts.add(ModelProvider.ContentPart.builder()
                    .type("image_url")
                    .imageUrl(ModelProvider.ImageUrl.builder().url(dataUri).build())
                    .build());
            contentParts.add(ModelProvider.ContentPart.builder()
                    .type("text")
                    .text(prompt)
                    .build());

            ModelProvider.ChatMessage userMsg = ModelProvider.ChatMessage.builder()
                    .role("user")
                    .contentParts(contentParts)
                    .content(prompt) // fallback text
                    .build();

            // Resolve model — use override, config, or default
            String modelId = modelOverride;
            if (modelId == null || modelId.isBlank()) {
                if (context.getConfig() != null && context.getConfig().getModel() != null) {
                    modelId = context.getConfig().getModel();
                } else {
                    modelId = "anthropic/claude-sonnet-4-5";
                }
            }

            ModelProvider provider = modelProviderRegistry.resolve(modelId);
            if (provider == null) {
                return ToolResult.fail("No model provider found for: " + modelId);
            }

            // Call the LLM with the image
            ModelProvider.ChatRequest request = ModelProvider.ChatRequest.builder()
                    .model(modelId)
                    .messages(List.of(userMsg))
                    .maxTokens(4096)
                    .temperature(0.3)
                    .build();

            ModelProvider.ChatResponse response = provider.chat(request).join();

            String result = response.getMessage() != null ? response.getMessage().getContent() : "";
            if (result == null || result.isBlank()) {
                return ToolResult.fail("Vision model returned empty response");
            }

            log.info("image-tool: vision response length={}", result.length());
            return ToolResult.ok(result);

        } catch (IOException e) {
            return ToolResult.fail("Image read failed: " + e.getMessage());
        } catch (Exception e) {
            log.error("image-tool error: {}", e.getMessage(), e);
            return ToolResult.fail("Image tool error: " + e.getMessage());
        }
    }

    private Path resolvePath(String imagePath, String cwd) {
        Path path = Path.of(imagePath);
        if (path.isAbsolute())
            return path;
        return Path.of(cwd != null ? cwd : System.getProperty("user.dir")).resolve(path);
    }

    private String guessMimeType(String path) {
        String lower = path.toLowerCase();
        if (lower.endsWith(".jpg") || lower.endsWith(".jpeg"))
            return "image/jpeg";
        if (lower.endsWith(".gif"))
            return "image/gif";
        if (lower.endsWith(".webp"))
            return "image/webp";
        if (lower.endsWith(".svg"))
            return "image/svg+xml";
        return "image/png";
    }

    static DataUrl decodeDataUrl(String dataUrl) {
        int commaIdx = dataUrl.indexOf(',');
        if (commaIdx < 0)
            return new DataUrl("image/png", dataUrl);
        String header = dataUrl.substring(5, commaIdx);
        String data = dataUrl.substring(commaIdx + 1);
        String mime = header.contains(";") ? header.substring(0, header.indexOf(';')) : header;
        if (mime.isBlank())
            mime = "image/png";
        return new DataUrl(mime, data);
    }

    static class DataUrl {
        final String mimeType;
        final String data;

        DataUrl(String mimeType, String data) {
            this.mimeType = mimeType;
            this.data = data;
        }
    }

    /**
     * Detect image MIME type from file magic bytes.
     * Returns null if format is unrecognized.
     */
    private static String detectMimeFromBytes(byte[] data) {
        if (data == null || data.length < 4)
            return null;
        // JPEG: FF D8 FF
        if (data[0] == (byte) 0xFF && data[1] == (byte) 0xD8 && data[2] == (byte) 0xFF) {
            return "image/jpeg";
        }
        // PNG: 89 50 4E 47
        if (data[0] == (byte) 0x89 && data[1] == 0x50 && data[2] == 0x4E && data[3] == 0x47) {
            return "image/png";
        }
        // GIF: 47 49 46
        if (data[0] == 0x47 && data[1] == 0x49 && data[2] == 0x46) {
            return "image/gif";
        }
        // WebP: RIFF....WEBP
        if (data.length >= 12 && data[0] == 0x52 && data[1] == 0x49 && data[2] == 0x46 && data[3] == 0x46
                && data[8] == 0x57 && data[9] == 0x45 && data[10] == 0x42 && data[11] == 0x50) {
            return "image/webp";
        }
        // BMP: 42 4D
        if (data[0] == 0x42 && data[1] == 0x4D) {
            return "image/bmp";
        }
        return null;
    }

    private static void addStr(ObjectNode props, String key, String desc) {
        ObjectNode p = props.putObject(key);
        p.put("type", "string");
        p.put("description", desc);
    }
}

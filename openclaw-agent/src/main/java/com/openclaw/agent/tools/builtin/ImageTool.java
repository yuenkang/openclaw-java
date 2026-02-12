package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.concurrent.CompletableFuture;

/**
 * Image tool — analyze images using vision-capable LLMs.
 * Corresponds to TypeScript's tools/image-tool.ts.
 */
@Slf4j
public class ImageTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final long MAX_IMAGE_BYTES = 20 * 1024 * 1024;

    @Override
    public String getName() {
        return "image";
    }

    @Override
    public String getDescription() {
        return "Analyze or describe an image using a vision model. " +
                "Provide an image via file path or base64 data URL, and an optional prompt.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode properties = schema.putObject("properties");

        addStr(properties, "imagePath", "Path to the image file");
        addStr(properties, "imageBase64", "Base64-encoded image data (or data: URL)");
        addStr(properties, "prompt", "Prompt about the image (default: 'Describe the image')");
        addStr(properties, "model", "Model override for image analysis");

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

            // TODO: Wire up to vision-capable LLM provider
            ObjectNode result = MAPPER.createObjectNode();
            result.put("prompt", prompt);
            result.put("mimeType", mimeType);
            result.put("imageSize", base64Data.length());
            result.put("status", "received");
            result.put("note", "Image tool stub — wire up vision model provider");

            log.info("image-tool: prompt='{}' mime={} size={}", prompt, mimeType, base64Data.length());
            return ToolResult.ok(ToolParamUtils.toJsonString(result), result);

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

    private static void addStr(ObjectNode props, String key, String desc) {
        ObjectNode p = props.putObject(key);
        p.put("type", "string");
        p.put("description", desc);
    }
}

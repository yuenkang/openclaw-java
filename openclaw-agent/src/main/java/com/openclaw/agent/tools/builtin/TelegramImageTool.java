package com.openclaw.agent.tools.builtin;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.openclaw.agent.tools.AgentTool;
import com.openclaw.agent.tools.ToolParamUtils;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.concurrent.CompletableFuture;

/**
 * Telegram-specific tool that allows the Agent to send images
 * directly to the current chat conversation.
 *
 * <p>
 * Accepts images from:
 * <ul>
 * <li>{@code filePath} — a local file path to an image</li>
 * <li>{@code base64} — raw base64-encoded image data</li>
 * <li>{@code url} — a public image URL</li>
 * </ul>
 * The chatId is derived from the ToolContext's sessionKey at runtime,
 * since for Telegram the sessionKey is the chatId.
 * </p>
 */
@Slf4j
public class TelegramImageTool implements AgentTool {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Functional interface for the actual image sending logic,
     * injected at wiring time to decouple from the channel module.
     */
    @FunctionalInterface
    public interface ImageSender {
        /**
         * Send an image to a Telegram chat.
         *
         * @param chatId     target chat ID
         * @param imageBytes image content bytes
         * @param fileName   suggested filename (e.g. "photo.png")
         * @param caption    optional caption text
         */
        void sendImage(String chatId, byte[] imageBytes, String fileName, String caption);
    }

    /** Global image sender — set by TelegramAgentWiring at startup. */
    private static volatile ImageSender imageSender;

    /** Set the image sender implementation (called once at wiring time). */
    public static void setImageSender(ImageSender sender) {
        imageSender = sender;
    }

    @Override
    public String getName() {
        return "send_image";
    }

    @Override
    public String getDescription() {
        return "Send an image to the current Telegram chat. "
                + "Provide image via: filePath (local file), base64 (raw image data), or url (public URL). "
                + "Optionally include a caption. "
                + "Use this tool when you need to show images to the user in the chat.";
    }

    @Override
    public JsonNode getParameterSchema() {
        ObjectNode schema = MAPPER.createObjectNode();
        schema.put("type", "object");
        ObjectNode props = schema.putObject("properties");

        ObjectNode filePath = props.putObject("filePath");
        filePath.put("type", "string");
        filePath.put("description", "Local file path to an image (png/jpg/gif/webp)");

        ObjectNode base64 = props.putObject("base64");
        base64.put("type", "string");
        base64.put("description", "Base64-encoded image data (without data URI prefix)");

        ObjectNode format = props.putObject("format");
        format.put("type", "string");
        format.put("description", "Image format when using base64 (default: png)");
        format.putArray("enum").add("png").add("jpg").add("jpeg").add("gif").add("webp");

        ObjectNode url = props.putObject("url");
        url.put("type", "string");
        url.put("description", "Public URL of an image to send");

        ObjectNode caption = props.putObject("caption");
        caption.put("type", "string");
        caption.put("description", "Optional caption text for the image");

        return schema;
    }

    @Override
    public CompletableFuture<ToolResult> execute(ToolContext context) {
        return CompletableFuture.supplyAsync(() -> doExecute(context));
    }

    private ToolResult doExecute(ToolContext context) {
        if (imageSender == null) {
            return ToolResult.fail("Image sending is not configured. "
                    + "This tool is only available when running via Telegram channel.");
        }

        // Derive chatId from sessionKey (format: "tg:CHAT_ID" or
        // "tg:CHAT_ID:THREAD_ID")
        String chatId = context.getSessionKey();
        if (chatId == null || chatId.isBlank()) {
            return ToolResult.fail("No target chat ID available.");
        }
        if (chatId.startsWith("tg:")) {
            String[] parts = chatId.split(":");
            chatId = parts[1]; // extract CHAT_ID
        }

        try {
            JsonNode params = context.getParameters();
            String filePath = ToolParamUtils.readStringParam(params, "filePath");
            String base64Data = ToolParamUtils.readStringParam(params, "base64");
            String imageUrl = ToolParamUtils.readStringParam(params, "url");
            String caption = ToolParamUtils.readStringParam(params, "caption");
            String format = ToolParamUtils.readStringParam(params, "format");
            if (format == null || format.isBlank())
                format = "png";

            byte[] imageBytes;
            String fileName;

            if (filePath != null && !filePath.isBlank()) {
                // --- Local file ---
                Path path = Path.of(filePath);
                if (!Files.exists(path) || !Files.isRegularFile(path)) {
                    return ToolResult.fail("File not found: " + filePath);
                }
                long size = Files.size(path);
                if (size > 10_000_000) {
                    return ToolResult.fail("File too large (max 10MB): " + size + " bytes");
                }
                imageBytes = Files.readAllBytes(path);
                fileName = path.getFileName().toString();

            } else if (base64Data != null && !base64Data.isBlank()) {
                // --- Base64 data ---
                if (base64Data.contains(",")) {
                    base64Data = base64Data.substring(base64Data.indexOf(',') + 1);
                }
                imageBytes = Base64.getDecoder().decode(base64Data.replaceAll("\\s+", ""));
                fileName = "image." + format;

            } else if (imageUrl != null && !imageUrl.isBlank()) {
                // --- URL: download and forward ---
                try {
                    var httpClient = java.net.http.HttpClient.newBuilder()
                            .connectTimeout(java.time.Duration.ofSeconds(10))
                            .build();
                    var request = java.net.http.HttpRequest.newBuilder()
                            .uri(java.net.URI.create(imageUrl))
                            .timeout(java.time.Duration.ofSeconds(30))
                            .GET().build();
                    var response = httpClient.send(request,
                            java.net.http.HttpResponse.BodyHandlers.ofByteArray());

                    if (response.statusCode() != 200) {
                        return ToolResult.fail("Failed to download image: HTTP " + response.statusCode());
                    }
                    imageBytes = response.body();
                    String urlPath = java.net.URI.create(imageUrl).getPath();
                    fileName = urlPath.contains("/")
                            ? urlPath.substring(urlPath.lastIndexOf('/') + 1)
                            : "image." + format;
                    if (!fileName.contains("."))
                        fileName += "." + format;
                } catch (Exception e) {
                    return ToolResult.fail("Failed to download image from URL: " + e.getMessage());
                }
            } else {
                return ToolResult.fail("Provide one of: filePath, base64, or url");
            }

            log.info("send_image: chatId={} fileName={} size={}", chatId, fileName, imageBytes.length);
            imageSender.sendImage(chatId, imageBytes, fileName, caption);

            ObjectNode result = MAPPER.createObjectNode();
            result.put("ok", true);
            result.put("chatId", chatId);
            result.put("fileName", fileName);
            result.put("size", imageBytes.length);
            return ToolResult.ok("Image sent successfully to chat " + chatId, result);

        } catch (Exception e) {
            log.error("send_image error: {}", e.getMessage(), e);
            return ToolResult.fail("Failed to send image: " + e.getMessage());
        }
    }
}

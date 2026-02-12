package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Image detection and loading for prompts and conversation history.
 * Corresponds to TypeScript pi-embedded-runner/run/images.ts.
 *
 * <p>
 * Detects image references in user prompts (file paths, file:// URLs),
 * loads them as base64-encoded image content for vision-capable models.
 * </p>
 */
@Slf4j
public final class RunImages {

    private static final Set<String> IMAGE_EXTENSIONS = Set.of(
            ".png", ".jpg", ".jpeg", ".gif", ".webp",
            ".bmp", ".tiff", ".tif", ".heic", ".heif");

    private static final Pattern MEDIA_ATTACHED_RE = Pattern.compile(
            "\\[media attached(?:\\s+\\d+/\\d+)?:\\s*([^\\]]+)\\]",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern IMAGE_EXT_PATH_RE = Pattern.compile(
            "^\\s*(.+?\\.(?:png|jpe?g|gif|webp|bmp|tiff?|heic|heif))\\s*(?:\\(|$|\\|)",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern MESSAGE_IMAGE_RE = Pattern.compile(
            "\\[Image:\\s*source:\\s*([^\\]]+\\.(?:png|jpe?g|gif|webp|bmp|tiff?|heic|heif))\\]",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern FILE_URL_RE = Pattern.compile(
            "file://[^\\s<>\"'`\\]]+\\.(?:png|jpe?g|gif|webp|bmp|tiff?|heic|heif)",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern FILE_PATH_RE = Pattern.compile(
            "(?:^|\\s|[\"'`(])((\\.\\.?/|[~/])[^\\s\"'`()\\[\\]]*\\.(?:png|jpe?g|gif|webp|bmp|tiff?|heic|heif))",
            Pattern.CASE_INSENSITIVE);

    private RunImages() {
    }

    /**
     * Detected image reference from a prompt or history message.
     */
    public record DetectedImageRef(
            String raw,
            RefType type,
            String resolved,
            /** Index of the message in history (null for current prompt). */
            Integer messageIndex) {
        public enum RefType {
            PATH, URL
        }
    }

    /**
     * Loaded image content for the LLM.
     */
    public record ImageContent(
            String data,
            String mimeType) {
    }

    /**
     * Result of image detection and loading.
     */
    public record ImageDetectionResult(
            List<ImageContent> promptImages,
            Map<Integer, List<ImageContent>> historyImagesByIndex,
            List<DetectedImageRef> detectedRefs,
            int loadedCount,
            int skippedCount) {
    }

    /**
     * Check if a file extension is an image extension.
     */
    public static boolean isImageExtension(String filePath) {
        if (filePath == null)
            return false;
        int dot = filePath.lastIndexOf('.');
        if (dot < 0)
            return false;
        return IMAGE_EXTENSIONS.contains(filePath.substring(dot).toLowerCase());
    }

    /**
     * Check if a model supports image input.
     */
    public static boolean modelSupportsImages(List<String> inputCapabilities) {
        return inputCapabilities != null && inputCapabilities.contains("image");
    }

    /**
     * Detect image references in a user prompt.
     *
     * <p>
     * Patterns detected:
     * </p>
     * <ul>
     * <li>Absolute paths: /path/to/image.png</li>
     * <li>Relative paths: ./image.png, ../images/photo.jpg</li>
     * <li>Home paths: ~/Pictures/screenshot.png</li>
     * <li>file:// URLs: file:///path/to/image.png</li>
     * <li>Message attachments: [Image: source: /path/to/image.jpg]</li>
     * </ul>
     */
    public static List<DetectedImageRef> detectImageReferences(String prompt) {
        if (prompt == null || prompt.isBlank())
            return List.of();

        List<DetectedImageRef> refs = new ArrayList<>();
        Set<String> seen = new HashSet<>();

        // Helper lambda for adding path refs
        java.util.function.Consumer<String> addPathRef = raw -> {
            String trimmed = raw.trim();
            if (trimmed.isEmpty() || seen.contains(trimmed.toLowerCase()))
                return;
            if (trimmed.startsWith("http://") || trimmed.startsWith("https://"))
                return;
            if (!isImageExtension(trimmed))
                return;
            seen.add(trimmed.toLowerCase());

            String resolved = trimmed.startsWith("~")
                    ? System.getProperty("user.home") + trimmed.substring(1)
                    : trimmed;
            refs.add(new DetectedImageRef(trimmed, DetectedImageRef.RefType.PATH, resolved, null));
        };

        // [media attached: path (type) | url]
        Matcher m = MEDIA_ATTACHED_RE.matcher(prompt);
        while (m.find()) {
            String content = m.group(1);
            if (content.matches("(?i)^\\d+\\s+files?$"))
                continue;

            Matcher pathMatch = IMAGE_EXT_PATH_RE.matcher(content);
            if (pathMatch.find() && pathMatch.group(1) != null) {
                addPathRef.accept(pathMatch.group(1).trim());
            }
        }

        // [Image: source: /path/...]
        m = MESSAGE_IMAGE_RE.matcher(prompt);
        while (m.find()) {
            String raw = m.group(1);
            if (raw != null)
                addPathRef.accept(raw.trim());
        }

        // file:// URLs
        m = FILE_URL_RE.matcher(prompt);
        while (m.find()) {
            String raw = m.group(0);
            if (seen.contains(raw.toLowerCase()))
                continue;
            seen.add(raw.toLowerCase());
            try {
                String resolved = java.net.URI.create(raw).getPath();
                if (resolved != null) {
                    refs.add(new DetectedImageRef(raw, DetectedImageRef.RefType.PATH, resolved, null));
                }
            } catch (Exception e) {
                // Skip malformed file:// URLs
            }
        }

        // File paths (absolute, relative, home)
        m = FILE_PATH_RE.matcher(prompt);
        while (m.find()) {
            if (m.group(1) != null) {
                addPathRef.accept(m.group(1));
            }
        }

        return refs;
    }

    /**
     * Load an image from a file path and return as ImageContent.
     *
     * @param ref          The detected image reference
     * @param workspaceDir Workspace directory for resolving relative paths
     * @param sandboxRoot  Optional sandbox root for path restriction
     * @return Loaded image content or null if loading failed
     */
    public static ImageContent loadImageFromRef(
            DetectedImageRef ref, String workspaceDir, String sandboxRoot) {
        try {
            if (ref.type() == DetectedImageRef.RefType.URL) {
                log.debug("Native image: rejecting remote URL (local-only): {}", ref.resolved());
                return null;
            }

            Path targetPath = Path.of(ref.resolved());
            if (!targetPath.isAbsolute()) {
                String root = sandboxRoot != null ? sandboxRoot : workspaceDir;
                targetPath = Path.of(root).resolve(targetPath).normalize();
            }

            // Sandbox enforcement
            if (sandboxRoot != null) {
                Path normalizedTarget = targetPath.normalize();
                Path normalizedRoot = Path.of(sandboxRoot).normalize();
                if (!normalizedTarget.startsWith(normalizedRoot)) {
                    log.debug("Native image: sandbox violation: {}", ref.resolved());
                    return null;
                }
            }

            if (!Files.exists(targetPath)) {
                log.debug("Native image: file not found: {}", targetPath);
                return null;
            }

            byte[] bytes = Files.readAllBytes(targetPath);
            String base64 = Base64.getEncoder().encodeToString(bytes);

            String mimeType = Files.probeContentType(targetPath);
            if (mimeType == null || !mimeType.startsWith("image/")) {
                mimeType = "image/jpeg"; // default
            }

            return new ImageContent(base64, mimeType);
        } catch (IOException e) {
            log.debug("Native image: failed to load {}: {}", ref.resolved(), e.getMessage());
            return null;
        }
    }

    /**
     * Detect and load images from a prompt and conversation history.
     */
    public static ImageDetectionResult detectAndLoadPromptImages(
            String prompt, String workspaceDir,
            List<String> modelInputCapabilities,
            List<ImageContent> existingImages,
            String sandboxRoot) {

        if (!modelSupportsImages(modelInputCapabilities)) {
            return new ImageDetectionResult(List.of(), Map.of(), List.of(), 0, 0);
        }

        List<DetectedImageRef> promptRefs = detectImageReferences(prompt);

        if (promptRefs.isEmpty()) {
            return new ImageDetectionResult(
                    existingImages != null ? existingImages : List.of(),
                    Map.of(), List.of(), 0, 0);
        }

        log.debug("Native image: detected {} image refs in prompt", promptRefs.size());

        List<ImageContent> images = new ArrayList<>(existingImages != null ? existingImages : List.of());
        int loaded = 0;
        int skipped = 0;

        for (DetectedImageRef ref : promptRefs) {
            ImageContent img = loadImageFromRef(ref, workspaceDir, sandboxRoot);
            if (img != null) {
                images.add(img);
                loaded++;
                log.debug("Native image: loaded {} {}", ref.type(), ref.resolved());
            } else {
                skipped++;
            }
        }

        return new ImageDetectionResult(images, Map.of(), promptRefs, loaded, skipped);
    }
}

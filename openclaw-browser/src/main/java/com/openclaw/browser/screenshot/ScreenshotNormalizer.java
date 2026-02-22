package com.openclaw.browser.screenshot;

import lombok.extern.slf4j.Slf4j;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.MemoryCacheImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

/**
 * Screenshot normalizer — adaptive compression and resizing.
 * Corresponds to TypeScript's screenshot.ts.
 *
 * <p>Ensures screenshots meet size and byte limits by:
 * <ol>
 *   <li>Checking if already under limits → return as-is</li>
 *   <li>Resizing if dimensions exceed max</li>
 *   <li>Converting to JPEG with adaptive quality if byte size exceeds max</li>
 * </ol>
 */
@Slf4j
public final class ScreenshotNormalizer {

    private ScreenshotNormalizer() {
    }

    /** Default max width for screenshots. */
    public static final int DEFAULT_MAX_WIDTH = 1280;
    /** Default max height for screenshots. */
    public static final int DEFAULT_MAX_HEIGHT = 720;
    /** Default max byte size (1.5 MB). */
    public static final int DEFAULT_MAX_BYTES = 1_500_000;
    /** Default JPEG quality (0-100). */
    public static final int DEFAULT_JPEG_QUALITY = 75;

    /**
     * Normalize a screenshot to fit within size and byte limits.
     *
     * @param imageData Raw image bytes (PNG or JPEG)
     * @param maxWidth  Max allowed width (0 = no limit)
     * @param maxHeight Max allowed height (0 = no limit)
     * @param maxBytes  Max allowed byte count (0 = no limit)
     * @return Normalized image bytes
     */
    public static byte[] normalize(byte[] imageData, int maxWidth, int maxHeight, int maxBytes) {
        if (imageData == null || imageData.length == 0) return imageData;

        // If already small enough, return as-is
        if (maxBytes <= 0 || imageData.length <= maxBytes) {
            return imageData;
        }

        try {
            BufferedImage img = ImageIO.read(new ByteArrayInputStream(imageData));
            if (img == null) {
                log.debug("Failed to decode image for normalization");
                return imageData;
            }

            int w = img.getWidth();
            int h = img.getHeight();

            int targetW = maxWidth > 0 ? maxWidth : DEFAULT_MAX_WIDTH;
            int targetH = maxHeight > 0 ? maxHeight : DEFAULT_MAX_HEIGHT;

            // Resize if needed
            if (w > targetW || h > targetH) {
                double scale = Math.min((double) targetW / w, (double) targetH / h);
                int newW = Math.max(1, (int) (w * scale));
                int newH = Math.max(1, (int) (h * scale));

                BufferedImage resized = new BufferedImage(newW, newH, BufferedImage.TYPE_INT_RGB);
                Graphics2D g = resized.createGraphics();
                g.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                        RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                g.setRenderingHint(RenderingHints.KEY_RENDERING,
                        RenderingHints.VALUE_RENDER_QUALITY);
                g.drawImage(img, 0, 0, newW, newH, null);
                g.dispose();
                img = resized;
            }

            // Try JPEG at decreasing quality until under maxBytes
            int quality = DEFAULT_JPEG_QUALITY;
            byte[] result = encodeJpeg(img, quality);

            while (result.length > maxBytes && quality > 10) {
                quality -= 10;
                result = encodeJpeg(img, quality);
            }

            if (result.length > maxBytes && quality > 5) {
                result = encodeJpeg(img, 5);
            }

            log.debug("Screenshot normalized: {}x{} {}KB → {}KB (q={})",
                    w, h, imageData.length / 1024, result.length / 1024, quality);
            return result;

        } catch (Exception e) {
            log.warn("Screenshot normalization failed: {}", e.getMessage());
            return imageData;
        }
    }

    /**
     * Normalize with default limits.
     */
    public static byte[] normalize(byte[] imageData) {
        return normalize(imageData, DEFAULT_MAX_WIDTH, DEFAULT_MAX_HEIGHT, DEFAULT_MAX_BYTES);
    }

    /**
     * Encode a BufferedImage as JPEG with the given quality.
     */
    private static byte[] encodeJpeg(BufferedImage img, int quality) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageWriter writer = ImageIO.getImageWritersByFormatName("jpeg").next();
        ImageWriteParam param = writer.getDefaultWriteParam();
        param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
        param.setCompressionQuality(quality / 100.0f);

        try (MemoryCacheImageOutputStream out = new MemoryCacheImageOutputStream(baos)) {
            writer.setOutput(out);
            writer.write(null, new IIOImage(img, null, null), param);
        }
        writer.dispose();
        return baos.toByteArray();
    }
}

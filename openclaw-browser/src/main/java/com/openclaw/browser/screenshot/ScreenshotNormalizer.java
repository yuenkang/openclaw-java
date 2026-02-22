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
 * <p>Ensures screenshots meet size and byte limits using a multi-pass approach:
 * <ol>
 *   <li>Check if already under limits → return as-is</li>
 *   <li>Try decreasing max-side / quality grid until under byte limit</li>
 * </ol>
 */
@Slf4j
public final class ScreenshotNormalizer {

    private ScreenshotNormalizer() {
    }

    /** Default max side (longest edge) for screenshots. */
    public static final int DEFAULT_MAX_SIDE = 2000;
    /** Default max byte size (5 MB). */
    public static final int DEFAULT_MAX_BYTES = 5 * 1024 * 1024;

    /** Quality steps for multi-pass compression. */
    private static final int[] QUALITIES = {85, 75, 65, 55, 45, 35};
    /** Side steps for multi-pass resizing. */
    private static final int[] SIDES = {2000, 1800, 1600, 1400, 1200, 1000, 800};

    /**
     * Result of normalization, including content type info.
     */
    public static class NormalizeResult {
        private final byte[] buffer;
        private final String contentType;

        public NormalizeResult(byte[] buffer, String contentType) {
            this.buffer = buffer;
            this.contentType = contentType;
        }

        public byte[] getBuffer() { return buffer; }
        public String getContentType() { return contentType; }
    }

    /**
     * Normalize a screenshot to fit within maxSide and maxBytes.
     * Uses multi-pass approach: try (side × quality) grid, keep smallest under limit.
     *
     * @param imageData Raw image bytes (PNG or JPEG)
     * @param maxSide   Max longest edge (default 2000)
     * @param maxBytes  Max byte size (default 5MB)
     * @return Normalized result with buffer and contentType
     */
    public static NormalizeResult normalize(byte[] imageData, int maxSide, int maxBytes) {
        if (imageData == null || imageData.length == 0) {
            return new NormalizeResult(imageData, null);
        }

        maxSide = Math.max(1, maxSide > 0 ? maxSide : DEFAULT_MAX_SIDE);
        maxBytes = Math.max(1, maxBytes > 0 ? maxBytes : DEFAULT_MAX_BYTES);

        try {
            BufferedImage img = ImageIO.read(new ByteArrayInputStream(imageData));
            if (img == null) {
                log.debug("Failed to decode image for normalization");
                return new NormalizeResult(imageData, null);
            }

            int w = img.getWidth();
            int h = img.getHeight();
            int maxDim = Math.max(w, h);

            // If already under limits, return as-is
            if (imageData.length <= maxBytes && (maxDim == 0 || (w <= maxSide && h <= maxSide))) {
                return new NormalizeResult(imageData, null);
            }

            // Build side grid
            int sideStart = maxDim > 0 ? Math.min(maxSide, maxDim) : maxSide;
            int[] sideGrid = buildSideGrid(sideStart, maxSide);

            byte[] smallest = null;
            int smallestSize = Integer.MAX_VALUE;

            for (int side : sideGrid) {
                BufferedImage resized = resizeToFit(img, w, h, side);
                for (int quality : QUALITIES) {
                    byte[] out = encodeJpeg(resized, quality);

                    if (out.length < smallestSize) {
                        smallest = out;
                        smallestSize = out.length;
                    }

                    if (out.length <= maxBytes) {
                        log.debug("Screenshot normalized: {}x{} {}KB → {}KB (side={}, q={})",
                                w, h, imageData.length / 1024, out.length / 1024, side, quality);
                        return new NormalizeResult(out, "image/jpeg");
                    }
                }
            }

            // Return smallest found even if over limit
            if (smallest != null) {
                log.warn("Screenshot could not be reduced below {}MB (got {}MB)",
                        maxBytes / (1024 * 1024), smallestSize / (1024 * 1024));
                return new NormalizeResult(smallest, "image/jpeg");
            }

            return new NormalizeResult(imageData, null);

        } catch (Exception e) {
            log.warn("Screenshot normalization failed: {}", e.getMessage());
            return new NormalizeResult(imageData, null);
        }
    }

    /**
     * Normalize with default limits.
     */
    public static NormalizeResult normalize(byte[] imageData) {
        return normalize(imageData, DEFAULT_MAX_SIDE, DEFAULT_MAX_BYTES);
    }

    /**
     * Simple normalize that returns just the bytes (backward-compatible overload).
     */
    public static byte[] normalizeToBytes(byte[] imageData) {
        return normalize(imageData).getBuffer();
    }

    public static byte[] normalizeToBytes(byte[] imageData, int maxWidth, int maxHeight, int maxBytes) {
        int maxSide = Math.max(maxWidth, maxHeight);
        return normalize(imageData, maxSide, maxBytes).getBuffer();
    }

    // ===== Internal helpers =====

    private static int[] buildSideGrid(int sideStart, int maxSide) {
        java.util.TreeSet<Integer> set = new java.util.TreeSet<>(java.util.Comparator.reverseOrder());
        set.add(sideStart);
        for (int s : SIDES) {
            int clamped = Math.min(maxSide, s);
            if (clamped > 0) set.add(clamped);
        }
        return set.stream().mapToInt(Integer::intValue).toArray();
    }

    private static BufferedImage resizeToFit(BufferedImage img, int w, int h, int maxSide) {
        int maxDim = Math.max(w, h);
        if (maxDim <= maxSide) return img;

        double scale = (double) maxSide / maxDim;
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
        return resized;
    }

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

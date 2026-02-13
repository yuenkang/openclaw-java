package com.openclaw.agent.autoreply.reply;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parse LINE-specific directives within message text — quick replies,
 * location sharing, media cards, stickers, etc.
 * Mirrors {@code auto-reply/reply/line-directives.ts}.
 */
public final class LineDirectives {

    private LineDirectives() {
    }

    /* ── constants ──────────────────────────────────────────── */

    /**
     * Quick reply button extracted from [[qr:label]] tags.
     */
    public record QuickReplyButton(String label, String data) {
    }

    /**
     * Location share extracted from [[location:lat,lng,name]] tags.
     */
    public record LocationShare(double latitude, double longitude, String title, String address) {
    }

    /* ── patterns ───────────────────────────────────────────── */

    private static final Pattern QR_TAG_RE = Pattern.compile(
            "\\[\\[qr:([^\\]]+?)\\]\\]", Pattern.CASE_INSENSITIVE);
    private static final Pattern LOCATION_TAG_RE = Pattern.compile(
            "\\[\\[location:([^\\]]+?)\\]\\]", Pattern.CASE_INSENSITIVE);
    private static final Pattern STICKER_TAG_RE = Pattern.compile(
            "\\[\\[sticker:([^\\]]+?)\\]\\]", Pattern.CASE_INSENSITIVE);
    private static final Pattern FLEX_TAG_RE = Pattern.compile(
            "\\[\\[flex:([^\\]]+?)\\]\\]", Pattern.CASE_INSENSITIVE);
    private static final Pattern IMAGEMAP_TAG_RE = Pattern.compile(
            "\\[\\[imagemap:([^\\]]+?)\\]\\]", Pattern.CASE_INSENSITIVE);

    /* ── quick replies ─────────────────────────────────────── */

    /**
     * Extract quick reply buttons from text.
     */
    public static List<QuickReplyButton> extractQuickReplies(String text) {
        if (text == null)
            return List.of();
        List<QuickReplyButton> buttons = new ArrayList<>();
        Matcher m = QR_TAG_RE.matcher(text);
        while (m.find()) {
            String raw = m.group(1).trim();
            if (raw.isEmpty())
                continue;
            String[] parts = raw.split("\\|", 2);
            String label = parts[0].trim();
            String data = parts.length > 1 ? parts[1].trim() : label;
            buttons.add(new QuickReplyButton(label, data));
        }
        return buttons;
    }

    /**
     * Remove quick reply tags from text.
     */
    public static String stripQuickReplies(String text) {
        if (text == null)
            return "";
        return QR_TAG_RE.matcher(text).replaceAll("").replaceAll("\\s+", " ").trim();
    }

    /* ── location sharing ──────────────────────────────────── */

    /**
     * Extract location shares from text.
     */
    public static List<LocationShare> extractLocations(String text) {
        if (text == null)
            return List.of();
        List<LocationShare> locations = new ArrayList<>();
        Matcher m = LOCATION_TAG_RE.matcher(text);
        while (m.find()) {
            String raw = m.group(1).trim();
            String[] parts = raw.split(",");
            if (parts.length < 2)
                continue;
            try {
                double lat = Double.parseDouble(parts[0].trim());
                double lng = Double.parseDouble(parts[1].trim());
                String title = parts.length > 2 ? parts[2].trim() : null;
                String address = parts.length > 3 ? parts[3].trim() : null;
                locations.add(new LocationShare(lat, lng, title, address));
            } catch (NumberFormatException ignored) {
                // Skip malformed locations
            }
        }
        return locations;
    }

    /**
     * Remove location tags from text.
     */
    public static String stripLocations(String text) {
        if (text == null)
            return "";
        return LOCATION_TAG_RE.matcher(text).replaceAll("").replaceAll("\\s+", " ").trim();
    }

    /* ── sticker ───────────────────────────────────────────── */

    /**
     * Extract sticker IDs from text.
     */
    public static List<String> extractStickerIds(String text) {
        if (text == null)
            return List.of();
        List<String> ids = new ArrayList<>();
        Matcher m = STICKER_TAG_RE.matcher(text);
        while (m.find()) {
            String raw = m.group(1).trim();
            if (!raw.isEmpty())
                ids.add(raw);
        }
        return ids;
    }

    /**
     * Remove sticker tags from text.
     */
    public static String stripStickers(String text) {
        if (text == null)
            return "";
        return STICKER_TAG_RE.matcher(text).replaceAll("").replaceAll("\\s+", " ").trim();
    }

    /* ── flex / imagemap ───────────────────────────────────── */

    /**
     * Extract flex JSON payloads from text.
     */
    public static List<String> extractFlexPayloads(String text) {
        if (text == null)
            return List.of();
        List<String> payloads = new ArrayList<>();
        Matcher m = FLEX_TAG_RE.matcher(text);
        while (m.find()) {
            String raw = m.group(1).trim();
            if (!raw.isEmpty())
                payloads.add(raw);
        }
        return payloads;
    }

    /**
     * Extract imagemap payloads from text.
     */
    public static List<String> extractImagemapPayloads(String text) {
        if (text == null)
            return List.of();
        List<String> payloads = new ArrayList<>();
        Matcher m = IMAGEMAP_TAG_RE.matcher(text);
        while (m.find()) {
            String raw = m.group(1).trim();
            if (!raw.isEmpty())
                payloads.add(raw);
        }
        return payloads;
    }

    /* ── composite ─────────────────────────────────────────── */

    /** Result of applying all LINE directives. */
    public record LineDirectiveResult(
            String cleanedText,
            List<QuickReplyButton> quickReplies,
            List<LocationShare> locations,
            List<String> stickerIds,
            List<String> flexPayloads,
            List<String> imagemapPayloads,
            Map<String, Object> channelData) {
    }

    /**
     * Apply all LINE directives to text — extract quick replies, locations,
     * stickers, flex, and imagemap payloads; clean the text.
     */
    public static LineDirectiveResult applyLineDirectives(String text) {
        if (text == null || text.isBlank()) {
            return new LineDirectiveResult("", List.of(), List.of(), List.of(), List.of(), List.of(), Map.of());
        }

        List<QuickReplyButton> qr = extractQuickReplies(text);
        List<LocationShare> locations = extractLocations(text);
        List<String> stickers = extractStickerIds(text);
        List<String> flex = extractFlexPayloads(text);
        List<String> imagemaps = extractImagemapPayloads(text);

        String cleaned = text;
        cleaned = stripQuickReplies(cleaned);
        cleaned = stripLocations(cleaned);
        cleaned = stripStickers(cleaned);
        cleaned = FLEX_TAG_RE.matcher(cleaned).replaceAll("").trim();
        cleaned = IMAGEMAP_TAG_RE.matcher(cleaned).replaceAll("").trim();
        cleaned = cleaned.replaceAll("\\s+", " ").trim();

        Map<String, Object> channelData = new LinkedHashMap<>();
        if (!qr.isEmpty())
            channelData.put("quickReplies", qr);
        if (!locations.isEmpty())
            channelData.put("locations", locations);
        if (!stickers.isEmpty())
            channelData.put("stickers", stickers);
        if (!flex.isEmpty())
            channelData.put("flex", flex);
        if (!imagemaps.isEmpty())
            channelData.put("imagemaps", imagemaps);

        return new LineDirectiveResult(cleaned, qr, locations, stickers, flex, imagemaps, channelData);
    }
}

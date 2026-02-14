package com.openclaw.gateway.outbound;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Data;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Outbound payload types and normalization.
 * Corresponds to TypeScript's payloads.ts.
 */
public final class OutboundPayloads {

    private OutboundPayloads() {
    }

    /**
     * Normalized outbound payload ready for channel delivery.
     */
    @Data
    @Builder
    public static class NormalizedOutboundPayload {
        private String text;
        @Builder.Default
        private List<String> mediaUrls = new ArrayList<>();
        private Map<String, Object> channelData;
    }

    /**
     * JSON-serializable outbound payload.
     */
    @Data
    @Builder
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class OutboundPayloadJson {
        private String text;
        private String mediaUrl;
        private List<String> mediaUrls;
        private Map<String, Object> channelData;
    }

    /**
     * Merge multiple media URL lists, deduplicating and trimming.
     */
    @SafeVarargs
    public static List<String> mergeMediaUrls(List<String>... lists) {
        Set<String> seen = new LinkedHashSet<>();
        for (List<String> list : lists) {
            if (list == null)
                continue;
            for (String entry : list) {
                if (entry == null)
                    continue;
                String trimmed = entry.trim();
                if (!trimmed.isEmpty()) {
                    seen.add(trimmed);
                }
            }
        }
        return new ArrayList<>(seen);
    }

    /**
     * Normalize reply payloads into NormalizedOutboundPayload list.
     * Filters out empty/non-renderable payloads.
     */
    public static List<NormalizedOutboundPayload> normalizeOutboundPayloads(
            List<Map<String, Object>> payloads) {
        if (payloads == null || payloads.isEmpty())
            return Collections.emptyList();

        return payloads.stream()
                .map(OutboundPayloads::normalizeOne)
                .filter(p -> p != null &&
                        (p.getText() != null && !p.getText().isEmpty()
                                || !p.getMediaUrls().isEmpty()
                                || (p.getChannelData() != null && !p.getChannelData().isEmpty())))
                .collect(Collectors.toList());
    }

    private static NormalizedOutboundPayload normalizeOne(Map<String, Object> payload) {
        if (payload == null)
            return null;

        String text = payload.containsKey("text")
                ? String.valueOf(payload.get("text"))
                : "";

        List<String> mediaUrls = new ArrayList<>();
        if (payload.containsKey("mediaUrls") && payload.get("mediaUrls") instanceof List) {
            @SuppressWarnings("unchecked")
            List<String> urls = (List<String>) payload.get("mediaUrls");
            mediaUrls.addAll(urls);
        }
        if (payload.containsKey("mediaUrl") && payload.get("mediaUrl") != null) {
            String singleUrl = String.valueOf(payload.get("mediaUrl")).trim();
            if (!singleUrl.isEmpty() && mediaUrls.isEmpty()) {
                mediaUrls.add(singleUrl);
            }
        }
        mediaUrls = mergeMediaUrls(mediaUrls);

        @SuppressWarnings("unchecked")
        Map<String, Object> channelData = payload.containsKey("channelData")
                ? (Map<String, Object>) payload.get("channelData")
                : null;

        return NormalizedOutboundPayload.builder()
                .text(text)
                .mediaUrls(mediaUrls)
                .channelData(channelData != null && !channelData.isEmpty() ? channelData : null)
                .build();
    }

    /**
     * Convert payloads to JSON-serializable form.
     */
    public static List<OutboundPayloadJson> normalizeForJson(
            List<Map<String, Object>> payloads) {
        if (payloads == null || payloads.isEmpty())
            return Collections.emptyList();

        return payloads.stream()
                .map(payload -> {
                    String text = payload.containsKey("text")
                            ? String.valueOf(payload.get("text"))
                            : "";
                    String mediaUrl = payload.containsKey("mediaUrl")
                            ? (payload.get("mediaUrl") != null
                                    ? String.valueOf(payload.get("mediaUrl"))
                                    : null)
                            : null;
                    @SuppressWarnings("unchecked")
                    List<String> mediaUrls = payload.containsKey("mediaUrls")
                            ? (List<String>) payload.get("mediaUrls")
                            : null;
                    @SuppressWarnings("unchecked")
                    Map<String, Object> channelData = payload.containsKey("channelData")
                            ? (Map<String, Object>) payload.get("channelData")
                            : null;

                    return OutboundPayloadJson.builder()
                            .text(text)
                            .mediaUrl(mediaUrl)
                            .mediaUrls(mediaUrls)
                            .channelData(channelData)
                            .build();
                })
                .collect(Collectors.toList());
    }

    /**
     * Format payload for logging.
     */
    public static String formatPayloadLog(NormalizedOutboundPayload payload) {
        StringBuilder sb = new StringBuilder();
        if (payload.getText() != null && !payload.getText().isEmpty()) {
            sb.append(payload.getText().stripTrailing());
        }
        for (String url : payload.getMediaUrls()) {
            if (sb.length() > 0)
                sb.append("\n");
            sb.append("MEDIA:").append(url);
        }
        return sb.toString();
    }
}

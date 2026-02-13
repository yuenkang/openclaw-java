package com.openclaw.gateway.cron;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Cron delivery plan resolution.
 * Determines whether & how to deliver an agent's cron job response.
 * Corresponds to TypeScript's cron/delivery.ts.
 */
public final class CronDeliveryResolver {

    private CronDeliveryResolver() {
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DeliveryPlan {
        private CronTypes.DeliveryMode mode;
        private String channel;
        private String to;
        /** Where the delivery config came from: "delivery" or "payload". */
        private String source;
        private boolean requested;
    }

    /**
     * Resolve the delivery plan for a rich cron job.
     */
    public static DeliveryPlan resolve(CronTypes.RichCronJob job) {
        CronTypes.CronPayload payload = job.getPayload();
        CronTypes.CronDelivery delivery = job.getDelivery();
        boolean hasDelivery = delivery != null;

        // Resolve mode from delivery block
        CronTypes.DeliveryMode mode = null;
        if (hasDelivery && delivery.getMode() != null) {
            mode = delivery.getMode();
        }

        // Resolve channel/to (prefer delivery > payload)
        String deliveryChannel = hasDelivery ? normalizeChannel(delivery.getChannel()) : null;
        String deliveryTo = hasDelivery ? normalizeTo(delivery.getTo()) : null;

        String payloadChannel = null;
        String payloadTo = null;
        // Only agentTurn payloads carry delivery hints
        // (In the simplified model, we skip payload-level channel/to inspection)

        String channel = deliveryChannel != null ? deliveryChannel
                : payloadChannel != null ? payloadChannel : "last";
        String to = deliveryTo != null ? deliveryTo : payloadTo;

        if (hasDelivery) {
            CronTypes.DeliveryMode resolvedMode = mode != null ? mode : CronTypes.DeliveryMode.NONE;
            return DeliveryPlan.builder()
                    .mode(resolvedMode)
                    .channel(channel)
                    .to(to)
                    .source("delivery")
                    .requested(resolvedMode == CronTypes.DeliveryMode.ANNOUNCE)
                    .build();
        }

        // Legacy fallback: infer from payload
        boolean hasExplicitTarget = to != null && !to.isEmpty();
        boolean requested = hasExplicitTarget;

        return DeliveryPlan.builder()
                .mode(requested ? CronTypes.DeliveryMode.ANNOUNCE : CronTypes.DeliveryMode.NONE)
                .channel(channel)
                .to(to)
                .source("payload")
                .requested(requested)
                .build();
    }

    private static String normalizeChannel(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim().toLowerCase();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private static String normalizeTo(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }
}

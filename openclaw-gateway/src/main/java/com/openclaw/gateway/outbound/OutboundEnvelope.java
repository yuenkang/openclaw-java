package com.openclaw.gateway.outbound;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * Result envelope wrapping outbound delivery results, payloads, and metadata.
 * Corresponds to TypeScript's OutboundResultEnvelope (envelope.ts).
 */
@Data
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class OutboundEnvelope {

    /** Normalized payloads (if any). */
    private List<OutboundPayloads.OutboundPayloadJson> payloads;

    /** Arbitrary metadata. */
    private Object meta;

    /** Delivery JSON details. */
    private OutboundDeliveryJson delivery;

    /**
     * Build an envelope from delivery result, payloads, and metadata.
     * If flattenDelivery is true and only delivery is present (no payloads/meta),
     * a standalone OutboundDeliveryJson is returned instead.
     */
    public static Object buildResultEnvelope(
            List<OutboundPayloads.OutboundPayloadJson> payloads,
            Object meta,
            OutboundDeliveryJson delivery,
            boolean flattenDelivery) {

        boolean hasPayloads = payloads != null;

        if (flattenDelivery && delivery != null && meta == null && !hasPayloads) {
            return delivery;
        }

        return OutboundEnvelope.builder()
                .payloads(hasPayloads ? payloads : null)
                .meta(meta)
                .delivery(delivery)
                .build();
    }

    /**
     * Build an envelope using default flattening (true).
     */
    public static Object buildResultEnvelope(
            List<OutboundPayloads.OutboundPayloadJson> payloads,
            Object meta,
            OutboundDeliveryJson delivery) {
        return buildResultEnvelope(payloads, meta, delivery, true);
    }
}

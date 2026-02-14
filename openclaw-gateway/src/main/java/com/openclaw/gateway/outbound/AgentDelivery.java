package com.openclaw.gateway.outbound;

import com.openclaw.common.config.OpenClawConfig;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * Resolves agent delivery plans from session state and channel configuration.
 * Coordinates session-based target resolution with outbound target validation.
 * Corresponds to TypeScript's agent-delivery.ts.
 */
@Slf4j
public class AgentDelivery {

    private static final String INTERNAL_CHANNEL = "internal";
    private static final String DEFAULT_CHAT_CHANNEL = "telegram";

    /**
     * Agent delivery plan describing what channel and target to use.
     */
    @Data
    @Builder
    public static class AgentDeliveryPlan {
        private SessionDeliveryTarget baseDelivery;
        private String resolvedChannel;
        private String resolvedTo;
        private String resolvedAccountId;
        private String resolvedThreadId;
        /** "explicit", "implicit", or "broadcast" */
        private String deliveryTargetMode;
    }

    /**
     * Resolve an agent delivery plan from session and request context.
     */
    public static AgentDeliveryPlan resolveDeliveryPlan(
            SessionDeliveryTarget sessionTarget,
            String requestedChannel,
            String explicitTo,
            String explicitThreadId,
            String accountId,
            boolean wantsDelivery) {

        // Normalize the requested channel
        String normalizedRequested = requestedChannel != null && !requestedChannel.isBlank()
                ? ChannelSelection.normalizeChannel(requestedChannel.trim())
                : null;
        String effChannel = normalizedRequested != null ? normalizedRequested : "last";

        // Resolve explicit target
        String effectiveTo = explicitTo != null && !explicitTo.isBlank()
                ? explicitTo.trim()
                : null;

        // Resolve channel from session or defaults
        String resolvedChannel;
        if (INTERNAL_CHANNEL.equals(effChannel)) {
            resolvedChannel = INTERNAL_CHANNEL;
        } else if ("last".equals(effChannel)) {
            if (sessionTarget != null && sessionTarget.getChannel() != null
                    && !INTERNAL_CHANNEL.equals(sessionTarget.getChannel())) {
                resolvedChannel = sessionTarget.getChannel();
            } else {
                resolvedChannel = wantsDelivery ? DEFAULT_CHAT_CHANNEL : INTERNAL_CHANNEL;
            }
        } else {
            if (ChannelSelection.isKnownChannel(effChannel)) {
                resolvedChannel = effChannel;
            } else if (sessionTarget != null && sessionTarget.getChannel() != null
                    && !INTERNAL_CHANNEL.equals(sessionTarget.getChannel())) {
                resolvedChannel = sessionTarget.getChannel();
            } else {
                resolvedChannel = wantsDelivery ? DEFAULT_CHAT_CHANNEL : INTERNAL_CHANNEL;
            }
        }

        // Determine target mode
        String deliveryTargetMode = effectiveTo != null ? "explicit"
                : ChannelSelection.isKnownChannel(resolvedChannel) ? "implicit" : null;

        // Resolve account ID
        String resolvedAccountId = normalizeAccountId(accountId);
        if (resolvedAccountId == null && "implicit".equals(deliveryTargetMode)
                && sessionTarget != null) {
            resolvedAccountId = sessionTarget.getAccountId();
        }

        // Resolve "to" from session last-used if not explicit
        String resolvedTo = effectiveTo;
        if (resolvedTo == null && sessionTarget != null
                && resolvedChannel.equals(sessionTarget.getLastChannel())) {
            resolvedTo = sessionTarget.getLastTo();
        }

        // Resolve thread ID
        String resolvedThreadId = explicitThreadId;
        if (resolvedThreadId == null && sessionTarget != null) {
            resolvedThreadId = sessionTarget.getThreadId();
        }

        return AgentDeliveryPlan.builder()
                .baseDelivery(sessionTarget)
                .resolvedChannel(resolvedChannel)
                .resolvedTo(resolvedTo)
                .resolvedAccountId(resolvedAccountId)
                .resolvedThreadId(resolvedThreadId)
                .deliveryTargetMode(deliveryTargetMode)
                .build();
    }

    /**
     * Resolve an outbound target from an agent delivery plan.
     */
    public static AgentOutboundResult resolveOutboundTarget(
            OpenClawConfig cfg,
            AgentDeliveryPlan plan,
            String targetMode,
            boolean validateExplicitTarget) {

        String effectiveMode = targetMode != null ? targetMode
                : plan.getDeliveryTargetMode() != null ? plan.getDeliveryTargetMode()
                        : plan.getResolvedTo() != null ? "explicit" : "implicit";

        // If channel is not deliverable, skip outbound resolution
        if (!ChannelSelection.isKnownChannel(plan.getResolvedChannel())) {
            return new AgentOutboundResult(null, plan.getResolvedTo(), effectiveMode);
        }

        // If not validating explicit targets and we have a resolved "to", use it
        // directly
        if (!validateExplicitTarget && plan.getResolvedTo() != null) {
            return new AgentOutboundResult(null, plan.getResolvedTo(), effectiveMode);
        }

        // Resolve via outbound target
        OutboundTargetResolution resolved = resolveOutbound(cfg, plan);
        String resolvedTo = resolved.isOk() ? resolved.getTo() : plan.getResolvedTo();

        return new AgentOutboundResult(resolved, resolvedTo, effectiveMode);
    }

    private static OutboundTargetResolution resolveOutbound(OpenClawConfig cfg,
            AgentDeliveryPlan plan) {
        if (plan.getResolvedTo() == null || plan.getResolvedTo().isBlank()) {
            return OutboundTargetResolution.failure("No target specified");
        }
        return OutboundTargetResolution.success(plan.getResolvedTo());
    }

    private static String normalizeAccountId(String accountId) {
        if (accountId == null || accountId.isBlank())
            return null;
        String trimmed = accountId.trim();
        return "default".equals(trimmed) ? null : trimmed;
    }

    /**
     * Result of outbound target resolution for agent delivery.
     */
    public record AgentOutboundResult(
            OutboundTargetResolution resolvedTarget,
            String resolvedTo,
            String targetMode) {
    }
}

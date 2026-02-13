package com.openclaw.channel;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * Channel onboarding types — wizard/setup flow types.
 * Corresponds to TypeScript's channels/plugins/onboarding-types.ts.
 */
public final class OnboardingTypes {

    private OnboardingTypes() {
    }

    @Data
    public static class SetupChannelsOptions {
        private Boolean allowDisable;
        private Boolean allowSignalInstall;
        private Map<String, String> accountIds;
        private Boolean promptAccountIds;
        private String whatsappAccountId;
        private Boolean promptWhatsAppAccountId;
        private List<String> forceAllowFromChannels;
        private Boolean skipStatusNote;
        private Boolean skipDmPolicyPrompt;
        private Boolean skipConfirm;
        private Boolean quickstartDefaults;
        private List<String> initialSelection;
    }

    @Data
    public static class ChannelOnboardingStatus {
        private String channel;
        private boolean configured;
        private List<String> statusLines;
        private String selectionHint;
        private Integer quickstartScore;
    }

    @Data
    public static class ChannelOnboardingResult {
        private String accountId;
    }

    @Data
    public static class ChannelOnboardingDmPolicy {
        private String label;
        private String channel;
        private String policyKey;
        private String allowFromKey;
    }
}

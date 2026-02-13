package com.openclaw.gateway.protocol.schema;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * Channel status and login protocol schemas.
 * Mirrors {@code protocol/schema/channels.ts}.
 */
public final class ChannelsSchemas {

    private ChannelsSchemas() {
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class TalkModeParams {
        private boolean enabled;
        private String phase;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChannelsStatusParams {
        private Boolean probe;
        private Integer timeoutMs;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChannelAccountSnapshot {
        private String accountId;
        private String name;
        private Boolean enabled;
        private Boolean configured;
        private Boolean linked;
        private Boolean running;
        private Boolean connected;
        private Integer reconnectAttempts;
        private Long lastConnectedAt;
        private String lastError;
        private Long lastStartAt;
        private Long lastStopAt;
        private Long lastInboundAt;
        private Long lastOutboundAt;
        private Long lastProbeAt;
        private String mode;
        private String dmPolicy;
        private List<String> allowFrom;
        private String tokenSource;
        private String botTokenSource;
        private String appTokenSource;
        private String baseUrl;
        private Boolean allowUnmentionedGroups;
        private String cliPath;
        private String dbPath;
        private Integer port;
        private Object probe;
        private Object audit;
        private Object application;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChannelUiMeta {
        private String id;
        private String label;
        private String detailLabel;
        private String systemImage;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChannelsStatusResult {
        private long ts;
        private List<String> channelOrder;
        private Map<String, String> channelLabels;
        private Map<String, String> channelDetailLabels;
        private Map<String, String> channelSystemImages;
        private List<ChannelUiMeta> channelMeta;
        private Map<String, Object> channels;
        private Map<String, List<ChannelAccountSnapshot>> channelAccounts;
        private Map<String, String> channelDefaultAccountId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ChannelsLogoutParams {
        private String channel;
        private String accountId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WebLoginStartParams {
        private Boolean force;
        private Integer timeoutMs;
        private Boolean verbose;
        private String accountId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class WebLoginWaitParams {
        private Integer timeoutMs;
        private String accountId;
    }
}

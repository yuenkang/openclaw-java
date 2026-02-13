package com.openclaw.channel;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * Core channel type definitions.
 * Corresponds to TypeScript's channels/plugins/types.core.ts.
 */
public final class ChannelTypes {

    private ChannelTypes() {
    }

    // =========================================================================
    // Enums / Constants
    // =========================================================================

    /** Outbound target mode. */
    public static final String TARGET_EXPLICIT = "explicit";
    public static final String TARGET_IMPLICIT = "implicit";
    public static final String TARGET_HEARTBEAT = "heartbeat";

    /** Channel account state. */
    public static final String STATE_LINKED = "linked";
    public static final String STATE_NOT_LINKED = "not linked";
    public static final String STATE_CONFIGURED = "configured";
    public static final String STATE_NOT_CONFIGURED = "not configured";
    public static final String STATE_ENABLED = "enabled";
    public static final String STATE_DISABLED = "disabled";

    /** Directory entry kind. */
    public static final String ENTRY_USER = "user";
    public static final String ENTRY_GROUP = "group";
    public static final String ENTRY_CHANNEL = "channel";

    // =========================================================================
    // Data classes
    // =========================================================================

    @Data
    public static class ChannelSetupInput {
        private String name;
        private String token;
        private String tokenFile;
        private String botToken;
        private String appToken;
        private String signalNumber;
        private String cliPath;
        private String dbPath;
        private String service; // "imessage" | "sms" | "auto"
        private String region;
        private String authDir;
        private String httpUrl;
        private String httpHost;
        private String httpPort;
        private String webhookPath;
        private String webhookUrl;
        private String audienceType;
        private String audience;
        private Boolean useEnv;
        private String homeserver;
        private String userId;
        private String accessToken;
        private String password;
        private String deviceName;
        private Integer initialSyncLimit;
        private String ship;
        private String url;
        private String code;
        private List<String> groupChannels;
        private List<String> dmAllowlist;
        private Boolean autoDiscoverChannels;
    }

    @Data
    public static class ChannelStatusIssue {
        private String channel;
        private String accountId;
        private String kind; // "intent" | "permissions" | "config" | "auth" | "runtime"
        private String message;
        private String fix;
    }

    @Data
    public static class ChannelMeta {
        private String id;
        private String label;
        private String selectionLabel;
        private String docsPath;
        private String docsLabel;
        private String blurb;
        private Integer order;
        private List<String> aliases;
        private String selectionDocsPrefix;
        private Boolean selectionDocsOmitLabel;
        private List<String> selectionExtras;
        private String detailLabel;
        private String systemImage;
        private Boolean showConfigured;
        private Boolean quickstartAllowFrom;
        private Boolean forceAccountBinding;
        private Boolean preferSessionLookupForAnnounceTarget;
        private List<String> preferOver;
    }

    @Data
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
        private Object lastDisconnect;
        private Long lastMessageAt;
        private Long lastEventAt;
        private String lastError;
        private Long lastStartAt;
        private Long lastStopAt;
        private Long lastInboundAt;
        private Long lastOutboundAt;
        private String mode;
        private String dmPolicy;
        private List<String> allowFrom;
        private String tokenSource;
        private String botTokenSource;
        private String appTokenSource;
        private String credentialSource;
        private String audienceType;
        private String audience;
        private String webhookPath;
        private String webhookUrl;
        private String baseUrl;
        private Boolean allowUnmentionedGroups;
        private String cliPath;
        private String dbPath;
        private Integer port;
        private Object probe;
        private Long lastProbeAt;
        private Object audit;
        private Object application;
        private Object bot;
    }

    @Data
    public static class ChannelCapabilities {
        private List<String> chatTypes; // "direct" | "group" | "channel" | "thread"
        private Boolean polls;
        private Boolean reactions;
        private Boolean edit;
        private Boolean unsend;
        private Boolean reply;
        private Boolean effects;
        private Boolean groupManagement;
        private Boolean threads;
        private Boolean media;
        private Boolean nativeCommands;
        private Boolean blockStreaming;
    }

    @Data
    public static class ChannelGroupContext {
        private String groupId;
        private String groupChannel;
        private String groupSpace;
        private String accountId;
        private String senderId;
        private String senderName;
        private String senderUsername;
        private String senderE164;
    }

    @Data
    public static class ChannelSecurityDmPolicy {
        private String policy;
        private List<String> allowFrom;
        private String policyPath;
        private String allowFromPath;
        private String approveHint;
    }

    @Data
    public static class ChannelThreadingContext {
        private String channel;
        private String from;
        private String to;
        private String chatType;
        private String replyToId;
        private String replyToIdFull;
        private String threadLabel;
        private String messageThreadId;
    }

    @Data
    public static class ChannelThreadingToolContext {
        private String currentChannelId;
        private String currentChannelProvider;
        private String currentThreadTs;
        private String replyToMode; // "off" | "first" | "all"
        private Boolean skipCrossContextDecoration;
    }

    @Data
    public static class ChannelDirectoryEntry {
        private String kind; // "user" | "group" | "channel"
        private String id;
        private String name;
        private String handle;
        private String avatarUrl;
        private Integer rank;
        private Object raw;
    }

    @Data
    public static class ChannelMessageActionContext {
        private String channel;
        private String action;
        private Map<String, Object> params;
        private String accountId;
        private Boolean dryRun;
    }

    @Data
    public static class ChannelToolSend {
        private String to;
        private String accountId;
    }

    @Data
    public static class ChannelPollResult {
        private String messageId;
        private String toJid;
        private String channelId;
        private String conversationId;
        private String pollId;
    }

    @Data
    public static class ChannelResolveResult {
        private String input;
        private boolean resolved;
        private String id;
        private String name;
        private String note;
    }

    @Data
    public static class ChannelLoginWithQrStartResult {
        private String qrDataUrl;
        private String message;
    }

    @Data
    public static class ChannelLoginWithQrWaitResult {
        private boolean connected;
        private String message;
    }

    @Data
    public static class ChannelLogoutResult {
        private boolean cleared;
        private Boolean loggedOut;
    }

    /** Streaming adapter defaults. */
    @Data
    public static class BlockStreamingCoalesceDefaults {
        private int minChars;
        private int idleMs;
    }

    /** Command adapter flags. */
    @Data
    public static class ChannelCommandConfig {
        private Boolean enforceOwnerForCommands;
        private Boolean skipWhenConfigEmpty;
    }
}

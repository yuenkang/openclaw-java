package com.openclaw.channel;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * Channel plugin definition — the contract that each channel must implement.
 * Corresponds to TypeScript's channels/plugins/types.plugin.ts.
 */
@Data
public class ChannelPluginDef {

    private String id;
    private ChannelTypes.ChannelMeta meta;
    private ChannelTypes.ChannelCapabilities capabilities;

    // Optional defaults
    private PluginDefaults defaults;

    // Reload behavior
    private ReloadConfig reload;

    // Optional gatewayMethods
    private List<String> gatewayMethods;

    // =========================================================================
    // Nested types
    // =========================================================================

    @Data
    public static class PluginDefaults {
        private QueueDefaults queue;
    }

    @Data
    public static class QueueDefaults {
        private Integer debounceMs;
    }

    @Data
    public static class ReloadConfig {
        private List<String> configPrefixes;
        private List<String> noopPrefixes;
    }

    @Data
    public static class ChannelConfigUiHint {
        private String label;
        private String help;
        private Boolean advanced;
        private Boolean sensitive;
        private String placeholder;
        private Object itemTemplate;
    }

    @Data
    public static class ChannelConfigSchema {
        private Map<String, Object> schema;
        private Map<String, ChannelConfigUiHint> uiHints;
    }
}

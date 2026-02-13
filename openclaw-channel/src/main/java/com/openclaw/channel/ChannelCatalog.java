package com.openclaw.channel;

import lombok.Data;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Channel plugin catalog — UI metadata and plugin discovery results.
 * Corresponds to TypeScript's channels/plugins/catalog.ts.
 */
public final class ChannelCatalog {

    private ChannelCatalog() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    public static class UiMetaEntry {
        private String id;
        private String label;
        private String detailLabel;
        private String systemImage;
    }

    @Data
    public static class UiCatalog {
        private List<UiMetaEntry> entries = new ArrayList<>();
        private List<String> order = new ArrayList<>();
        private Map<String, String> labels = new LinkedHashMap<>();
        private Map<String, String> detailLabels = new LinkedHashMap<>();
        private Map<String, String> systemImages = new LinkedHashMap<>();
        private Map<String, UiMetaEntry> byId = new LinkedHashMap<>();
    }

    @Data
    public static class CatalogInstall {
        private String npmSpec;
        private String localPath;
        private String defaultChoice; // "npm" | "local"
    }

    @Data
    public static class CatalogEntry {
        private String id;
        private ChannelTypes.ChannelMeta meta;
        private CatalogInstall install;
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Build a UI catalog from a list of channel plugins.
     */
    public static UiCatalog buildUiCatalog(List<ChannelPluginDef> plugins) {
        UiCatalog catalog = new UiCatalog();
        for (ChannelPluginDef plugin : plugins) {
            ChannelTypes.ChannelMeta meta = plugin.getMeta();
            UiMetaEntry entry = new UiMetaEntry();
            entry.setId(plugin.getId());
            entry.setLabel(meta.getLabel());
            String detail = meta.getDetailLabel() != null ? meta.getDetailLabel()
                    : (meta.getSelectionLabel() != null ? meta.getSelectionLabel() : meta.getLabel());
            entry.setDetailLabel(detail);
            entry.setSystemImage(meta.getSystemImage());

            catalog.getEntries().add(entry);
            catalog.getOrder().add(entry.getId());
            catalog.getLabels().put(entry.getId(), entry.getLabel());
            catalog.getDetailLabels().put(entry.getId(), entry.getDetailLabel());
            if (entry.getSystemImage() != null) {
                catalog.getSystemImages().put(entry.getId(), entry.getSystemImage());
            }
            catalog.getById().put(entry.getId(), entry);
        }
        return catalog;
    }

    /**
     * Sort catalog entries by order, then by label.
     */
    public static List<CatalogEntry> sortEntries(List<CatalogEntry> entries) {
        List<CatalogEntry> sorted = new ArrayList<>(entries);
        sorted.sort(Comparator
                .comparingInt((CatalogEntry e) -> e.getMeta().getOrder() != null ? e.getMeta().getOrder() : 999)
                .thenComparing(e -> e.getMeta().getLabel()));
        return sorted;
    }
}

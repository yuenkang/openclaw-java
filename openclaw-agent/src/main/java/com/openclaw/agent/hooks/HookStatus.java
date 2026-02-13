package com.openclaw.agent.hooks;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * Hook status reporting.
 * Corresponds to TypeScript's hooks/hooks-status.ts.
 */
public final class HookStatus {

    private HookStatus() {
    }

    // =========================================================================
    // Status types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookStatusEntry {
        private String name;
        private String description;
        private String source;
        private String pluginId;
        private String filePath;
        private String baseDir;
        private String handlerPath;
        private String hookKey;
        private String emoji;
        private String homepage;
        private List<String> events;
        private boolean always;
        private boolean disabled;
        private boolean eligible;
        private boolean managedByPlugin;
        private Requirements requirements;
        private MissingRequirements missing;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Requirements {
        private List<String> bins;
        private List<String> anyBins;
        private List<String> env;
        private List<String> config;
        private List<String> os;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MissingRequirements {
        private List<String> bins;
        private List<String> anyBins;
        private List<String> env;
        private List<String> config;
        private List<String> os;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HookStatusReport {
        private String workspaceDir;
        private String managedHooksDir;
        private List<HookStatusEntry> hooks;
    }

    // =========================================================================
    // Build status
    // =========================================================================

    /**
     * Build a hook status report for the given workspace.
     */
    public static HookStatusReport buildWorkspaceHookStatus(
            String workspaceDir,
            OpenClawConfig config,
            List<HookTypes.HookEntry> entries) {

        List<HookStatusEntry> statusEntries = new ArrayList<>();

        for (HookTypes.HookEntry entry : entries) {
            statusEntries.add(buildEntryStatus(entry, config));
        }

        return HookStatusReport.builder()
                .workspaceDir(workspaceDir)
                .hooks(statusEntries)
                .build();
    }

    private static HookStatusEntry buildEntryStatus(HookTypes.HookEntry entry, OpenClawConfig config) {
        HookTypes.Hook hook = entry.getHook();
        HookTypes.HookMetadata meta = entry.getMetadata();

        String hookKey = (meta != null && meta.getHookKey() != null) ? meta.getHookKey() : hook.getName();
        boolean managedByPlugin = hook.getSource() == HookTypes.HookSource.OPENCLAW_PLUGIN;
        boolean disabled = entry.getInvocation() != null && !entry.getInvocation().isEnabled();
        boolean always = meta != null && Boolean.TRUE.equals(meta.getAlways());
        List<String> events = meta != null && meta.getEvents() != null ? meta.getEvents() : List.of();

        // Gather requirements from metadata
        List<String> reqBins = List.of();
        List<String> reqAnyBins = List.of();
        List<String> reqEnv = List.of();
        List<String> reqConfig = List.of();
        List<String> reqOs = List.of();
        if (meta != null && meta.getRequires() != null) {
            HookTypes.HookRequirements req = meta.getRequires();
            reqBins = req.getBins() != null ? req.getBins() : List.of();
            reqAnyBins = req.getAnyBins() != null ? req.getAnyBins() : List.of();
            reqEnv = req.getEnv() != null ? req.getEnv() : List.of();
            reqConfig = req.getConfig() != null ? req.getConfig() : List.of();
        }
        if (meta != null && meta.getOs() != null) {
            reqOs = meta.getOs();
        }

        // Simplified: assume all local requirements satisfied for now
        MissingRequirements missing = MissingRequirements.builder()
                .bins(List.of())
                .anyBins(List.of())
                .env(List.of())
                .config(List.of())
                .os(List.of())
                .build();

        boolean eligible = !disabled && (always || true /* simplified: all reqs met */);

        return HookStatusEntry.builder()
                .name(hook.getName())
                .description(hook.getDescription())
                .source(hook.getSource().label())
                .pluginId(hook.getPluginId())
                .filePath(hook.getFilePath())
                .baseDir(hook.getBaseDir())
                .handlerPath(hook.getHandlerPath())
                .hookKey(hookKey)
                .emoji(meta != null ? meta.getEmoji() : null)
                .homepage(meta != null ? meta.getHomepage() : null)
                .events(events)
                .always(always)
                .disabled(disabled)
                .eligible(eligible)
                .managedByPlugin(managedByPlugin)
                .requirements(Requirements.builder()
                        .bins(reqBins)
                        .anyBins(reqAnyBins)
                        .env(reqEnv)
                        .config(reqConfig)
                        .os(reqOs)
                        .build())
                .missing(missing)
                .build();
    }
}

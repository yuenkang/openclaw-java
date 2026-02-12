package com.openclaw.agent.sandbox;

import com.openclaw.agent.sandbox.SandboxDocker.ContainerState;
import com.openclaw.agent.sandbox.SandboxRegistry.SandboxBrowserRegistryEntry;
import com.openclaw.agent.sandbox.SandboxRegistry.SandboxRegistryEntry;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.*;

/**
 * Sandbox container management — list, remove, inspect containers and browsers.
 * Corresponds to TypeScript sandbox/manage.ts + sandbox/browser-bridges.ts.
 */
@Slf4j
public final class SandboxManager {

    private SandboxManager() {
    }

    // ── Browser bridge cache (in-memory) ────────────────────────────

    @Data
    @Builder
    public static class BrowserBridgeEntry {
        private String bridgeUrl;
        private String containerName;
    }

    private static final Map<String, BrowserBridgeEntry> BROWSER_BRIDGES = new LinkedHashMap<>();

    public static Map<String, BrowserBridgeEntry> getBrowserBridges() {
        return BROWSER_BRIDGES;
    }

    // ── Container info types ────────────────────────────────────────

    @Data
    @Builder
    public static class SandboxContainerInfo {
        private String containerName;
        private String sessionKey;
        private long createdAtMs;
        private long lastUsedAtMs;
        private String image;
        private String configHash;
        private boolean running;
        private boolean imageMatch;
    }

    @Data
    @Builder
    public static class SandboxBrowserInfo {
        private String containerName;
        private String sessionKey;
        private long createdAtMs;
        private long lastUsedAtMs;
        private String image;
        private int cdpPort;
        private Integer noVncPort;
        private boolean running;
        private boolean imageMatch;
    }

    // ── List operations ─────────────────────────────────────────────

    /**
     * List all sandbox containers with their current Docker state.
     */
    public static List<SandboxContainerInfo> listSandboxContainers() throws IOException {
        List<SandboxRegistryEntry> entries = SandboxRegistry.readRegistry();
        List<SandboxContainerInfo> results = new ArrayList<>();

        for (SandboxRegistryEntry entry : entries) {
            ContainerState state = SandboxDocker.dockerContainerState(entry.getContainerName());
            String actualImage = entry.getImage();
            if (state.isExists()) {
                try {
                    var result = SandboxDocker.execDocker(
                            List.of("inspect", "-f", "{{.Config.Image}}", entry.getContainerName()), true);
                    if (result.getCode() == 0) {
                        actualImage = result.getStdout().trim();
                    }
                } catch (Exception e) {
                    // ignore
                }
            }

            results.add(SandboxContainerInfo.builder()
                    .containerName(entry.getContainerName())
                    .sessionKey(entry.getSessionKey())
                    .createdAtMs(entry.getCreatedAtMs())
                    .lastUsedAtMs(entry.getLastUsedAtMs())
                    .image(actualImage)
                    .configHash(entry.getConfigHash())
                    .running(state.isRunning())
                    .imageMatch(actualImage.equals(entry.getImage()))
                    .build());
        }

        return results;
    }

    /**
     * List all sandbox browser containers with their current Docker state.
     */
    public static List<SandboxBrowserInfo> listSandboxBrowsers() throws IOException {
        List<SandboxBrowserRegistryEntry> entries = SandboxRegistry.readBrowserRegistry();
        List<SandboxBrowserInfo> results = new ArrayList<>();

        for (SandboxBrowserRegistryEntry entry : entries) {
            ContainerState state = SandboxDocker.dockerContainerState(entry.getContainerName());
            String actualImage = entry.getImage();
            if (state.isExists()) {
                try {
                    var result = SandboxDocker.execDocker(
                            List.of("inspect", "-f", "{{.Config.Image}}", entry.getContainerName()), true);
                    if (result.getCode() == 0) {
                        actualImage = result.getStdout().trim();
                    }
                } catch (Exception e) {
                    // ignore
                }
            }

            results.add(SandboxBrowserInfo.builder()
                    .containerName(entry.getContainerName())
                    .sessionKey(entry.getSessionKey())
                    .createdAtMs(entry.getCreatedAtMs())
                    .lastUsedAtMs(entry.getLastUsedAtMs())
                    .image(actualImage)
                    .cdpPort(entry.getCdpPort())
                    .noVncPort(entry.getNoVncPort())
                    .running(state.isRunning())
                    .imageMatch(actualImage.equals(entry.getImage()))
                    .build());
        }

        return results;
    }

    // ── Remove operations ───────────────────────────────────────────

    /**
     * Remove a sandbox container and its registry entry.
     */
    public static void removeSandboxContainer(String containerName) {
        try {
            SandboxDocker.execDocker(List.of("rm", "-f", containerName), true);
        } catch (Exception e) {
            // ignore removal failures
        }
        SandboxRegistry.removeRegistryEntry(containerName);
    }

    /**
     * Remove a sandbox browser container and its registry entry.
     */
    public static void removeSandboxBrowserContainer(String containerName) {
        try {
            SandboxDocker.execDocker(List.of("rm", "-f", containerName), true);
        } catch (Exception e) {
            // ignore removal failures
        }
        SandboxRegistry.removeBrowserRegistryEntry(containerName);

        // Remove browser bridge if active
        BROWSER_BRIDGES.entrySet().removeIf(entry -> containerName.equals(entry.getValue().getContainerName()));
    }
}

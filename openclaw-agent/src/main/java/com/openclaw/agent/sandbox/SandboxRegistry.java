package com.openclaw.agent.sandbox;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;

/**
 * Sandbox container and browser registry — JSON file persistence.
 * Corresponds to TypeScript sandbox/registry.ts.
 */
@Slf4j
public final class SandboxRegistry {

    private SandboxRegistry() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();

    // ── Registry entry types ────────────────────────────────────────

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxRegistryEntry {
        private String containerName;
        private String sessionKey;
        private long createdAtMs;
        private long lastUsedAtMs;
        private String image;
        private String configHash;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SandboxBrowserRegistryEntry {
        private String containerName;
        private String sessionKey;
        private long createdAtMs;
        private long lastUsedAtMs;
        private String image;
        private int cdpPort;
        private Integer noVncPort;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    private static class Registry<T> {
        private List<T> entries = new ArrayList<>();
    }

    // ── Container registry ──────────────────────────────────────────

    public static List<SandboxRegistryEntry> readRegistry() {
        return readJsonList(SandboxConstants.SANDBOX_REGISTRY_PATH.toString(),
                new TypeReference<Registry<SandboxRegistryEntry>>() {
                });
    }

    public static void updateRegistry(SandboxRegistryEntry entry) {
        List<SandboxRegistryEntry> entries = readRegistry();
        SandboxRegistryEntry existing = entries.stream()
                .filter(e -> e.getContainerName().equals(entry.getContainerName()))
                .findFirst().orElse(null);

        entries.removeIf(e -> e.getContainerName().equals(entry.getContainerName()));
        entries.add(SandboxRegistryEntry.builder()
                .containerName(entry.getContainerName())
                .sessionKey(entry.getSessionKey())
                .createdAtMs(existing != null ? existing.getCreatedAtMs() : entry.getCreatedAtMs())
                .lastUsedAtMs(entry.getLastUsedAtMs())
                .image(existing != null ? existing.getImage() : entry.getImage())
                .configHash(entry.getConfigHash() != null ? entry.getConfigHash()
                        : existing != null ? existing.getConfigHash() : null)
                .build());

        writeJsonList(SandboxConstants.SANDBOX_REGISTRY_PATH.toString(), entries, "entries");
    }

    public static void removeRegistryEntry(String containerName) {
        List<SandboxRegistryEntry> entries = readRegistry();
        int before = entries.size();
        entries.removeIf(e -> e.getContainerName().equals(containerName));
        if (entries.size() < before) {
            writeJsonList(SandboxConstants.SANDBOX_REGISTRY_PATH.toString(), entries, "entries");
        }
    }

    // ── Browser registry ────────────────────────────────────────────

    public static List<SandboxBrowserRegistryEntry> readBrowserRegistry() {
        return readJsonList(SandboxConstants.SANDBOX_BROWSER_REGISTRY_PATH.toString(),
                new TypeReference<Registry<SandboxBrowserRegistryEntry>>() {
                });
    }

    public static void updateBrowserRegistry(SandboxBrowserRegistryEntry entry) {
        List<SandboxBrowserRegistryEntry> entries = readBrowserRegistry();
        SandboxBrowserRegistryEntry existing = entries.stream()
                .filter(e -> e.getContainerName().equals(entry.getContainerName()))
                .findFirst().orElse(null);

        entries.removeIf(e -> e.getContainerName().equals(entry.getContainerName()));
        entries.add(SandboxBrowserRegistryEntry.builder()
                .containerName(entry.getContainerName())
                .sessionKey(entry.getSessionKey())
                .createdAtMs(existing != null ? existing.getCreatedAtMs() : entry.getCreatedAtMs())
                .lastUsedAtMs(entry.getLastUsedAtMs())
                .image(existing != null ? existing.getImage() : entry.getImage())
                .cdpPort(entry.getCdpPort())
                .noVncPort(entry.getNoVncPort())
                .build());

        writeJsonList(SandboxConstants.SANDBOX_BROWSER_REGISTRY_PATH.toString(), entries, "entries");
    }

    public static void removeBrowserRegistryEntry(String containerName) {
        List<SandboxBrowserRegistryEntry> entries = readBrowserRegistry();
        int before = entries.size();
        entries.removeIf(e -> e.getContainerName().equals(containerName));
        if (entries.size() < before) {
            writeJsonList(SandboxConstants.SANDBOX_BROWSER_REGISTRY_PATH.toString(), entries, "entries");
        }
    }

    // ── JSON helpers ────────────────────────────────────────────────

    @SuppressWarnings("unchecked")
    private static <T> List<T> readJsonList(String path, TypeReference<Registry<T>> type) {
        try {
            String raw = Files.readString(Path.of(path));
            Registry<T> registry = MAPPER.readValue(raw, type);
            if (registry != null && registry.getEntries() != null) {
                return new ArrayList<>(registry.getEntries());
            }
        } catch (IOException e) {
            // File doesn't exist or is corrupted — return empty
        }
        return new ArrayList<>();
    }

    private static <T> void writeJsonList(String path, List<T> entries, String key) {
        try {
            Path p = Path.of(path);
            Files.createDirectories(p.getParent());
            Map<String, Object> wrapper = new LinkedHashMap<>();
            wrapper.put(key, entries);
            String json = MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(wrapper) + "\n";
            Files.writeString(p, json);
        } catch (IOException e) {
            log.error("Failed to write sandbox registry: {}", e.getMessage());
        }
    }
}

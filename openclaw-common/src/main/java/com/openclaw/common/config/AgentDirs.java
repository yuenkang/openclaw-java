package com.openclaw.common.config;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Agent directory resolution and duplicate detection.
 * Corresponds to TypeScript's agent-dirs.ts.
 */
public final class AgentDirs {

    private AgentDirs() {
    }

    public static final String DEFAULT_AGENT_ID = "default";

    // =========================================================================
    // Types
    // =========================================================================

    public record DuplicateAgentDir(String agentDir, List<String> agentIds) {
    }

    public static class DuplicateAgentDirError extends RuntimeException {
        private final List<DuplicateAgentDir> duplicates;

        public DuplicateAgentDirError(List<DuplicateAgentDir> duplicates) {
            super(formatDuplicateAgentDirError(duplicates));
            this.duplicates = duplicates;
        }

        public List<DuplicateAgentDir> getDuplicates() {
            return duplicates;
        }
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Canonicalize an agent directory path for case-insensitive comparison on
     * macOS/Windows.
     */
    public static String canonicalizeAgentDir(String agentDir) {
        String resolved = Path.of(agentDir).toAbsolutePath().normalize().toString();
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac") || os.contains("win")) {
            return resolved.toLowerCase();
        }
        return resolved;
    }

    /**
     * Collect all referenced agent IDs from configuration.
     */
    public static List<String> collectReferencedAgentIds(OpenClawConfig cfg) {
        Set<String> ids = new HashSet<>();

        var agents = cfg.getAgents();
        List<OpenClawConfig.AgentEntry> list = (agents != null && agents.getEntries() != null)
                ? agents.getEntries()
                : List.of();

        String defaultId = list.stream()
                .filter(a -> a != null && Boolean.TRUE.equals(a.getDefaultAgent()))
                .map(OpenClawConfig.AgentEntry::getId)
                .findFirst()
                .orElse(list.isEmpty() ? DEFAULT_AGENT_ID
                        : (list.get(0).getId() != null ? list.get(0).getId() : DEFAULT_AGENT_ID));

        ids.add(normalizeAgentId(defaultId));

        for (var entry : list) {
            if (entry != null && entry.getId() != null) {
                ids.add(normalizeAgentId(entry.getId()));
            }
        }

        var bindings = cfg.getBindings();
        if (bindings != null) {
            for (var binding : bindings) {
                String id = binding != null ? binding.getAgentId() : null;
                if (id != null && !id.isBlank()) {
                    ids.add(normalizeAgentId(id));
                }
            }
        }

        return new ArrayList<>(ids);
    }

    /**
     * Resolve the effective agent directory for the given agent ID.
     */
    public static String resolveEffectiveAgentDir(OpenClawConfig cfg, String agentId) {
        String id = normalizeAgentId(agentId);
        var agents = cfg.getAgents();
        List<OpenClawConfig.AgentEntry> list = (agents != null && agents.getEntries() != null)
                ? agents.getEntries()
                : List.of();

        String configured = list.stream()
                .filter(a -> a != null && id.equals(normalizeAgentId(a.getId())))
                .map(OpenClawConfig.AgentEntry::getAgentDir)
                .filter(d -> d != null && !d.isBlank())
                .findFirst()
                .orElse(null);

        if (configured != null) {
            return resolveUserPath(configured.trim());
        }

        return ConfigPaths.resolveStateDir().resolve("agents").resolve(id).resolve("agent")
                .toAbsolutePath().normalize().toString();
    }

    /**
     * Find agent IDs that share the same effective agent directory.
     */
    public static List<DuplicateAgentDir> findDuplicateAgentDirs(OpenClawConfig cfg) {
        Map<String, DuplicateAgentDir> byDir = new LinkedHashMap<>();

        for (String agentId : collectReferencedAgentIds(cfg)) {
            String agentDir = resolveEffectiveAgentDir(cfg, agentId);
            String key = canonicalizeAgentDir(agentDir);
            byDir.compute(key, (k, existing) -> {
                if (existing == null) {
                    List<String> ids = new ArrayList<>();
                    ids.add(agentId);
                    return new DuplicateAgentDir(agentDir, ids);
                }
                existing.agentIds().add(agentId);
                return existing;
            });
        }

        return byDir.values().stream()
                .filter(d -> d.agentIds().size() > 1)
                .toList();
    }

    /**
     * Format a human-readable error message about duplicate agent dirs.
     */
    public static String formatDuplicateAgentDirError(List<DuplicateAgentDir> dups) {
        StringBuilder sb = new StringBuilder();
        sb.append("Duplicate agentDir detected (multi-agent config).\n");
        sb.append(
                "Each agent must have a unique agentDir; sharing it causes auth/session state collisions and token invalidation.\n\n");
        sb.append("Conflicts:\n");
        for (var d : dups) {
            sb.append("- ").append(d.agentDir()).append(": ");
            sb.append(String.join(", ", d.agentIds().stream().map(id -> "\"" + id + "\"").toList()));
            sb.append('\n');
        }
        sb.append("\nFix: remove the shared agents.list[].agentDir override (or give each agent its own directory).\n");
        sb.append("If you want to share credentials, copy auth-profiles.json instead of sharing the entire agentDir.");
        return sb.toString();
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    public static String normalizeAgentId(String id) {
        if (id == null)
            return DEFAULT_AGENT_ID;
        String trimmed = id.trim().toLowerCase();
        return trimmed.isEmpty() ? DEFAULT_AGENT_ID : trimmed;
    }

    private static String resolveUserPath(String p) {
        if (p.startsWith("~")) {
            return System.getProperty("user.home") + p.substring(1);
        }
        return Path.of(p).toAbsolutePath().normalize().toString();
    }
}

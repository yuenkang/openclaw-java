package com.openclaw.common.infra;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Exec approvals — manages allowlists for command execution.
 * Controls which commands the agent is allowed to execute.
 * <p>
 * Corresponds to TypeScript's {@code infra/exec-approvals.ts}.
 */
@Slf4j
public class ExecApprovals {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /** Default safe binaries that are always allowed (read-only commands). */
    public static final List<String> DEFAULT_SAFE_BINS = List.of(
            "jq", "grep", "cut", "sort", "uniq", "head", "tail", "tr", "wc");

    // =========================================================================
    // Types
    // =========================================================================

    public enum ExecSecurity {
        ALLOW, DENY
    }

    public enum ExecAsk {
        ALWAYS, ON_MISS, NEVER
    }

    @Data
    @Builder
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AllowlistEntry {
        private String id;
        private String pattern;
        private Long lastUsedAt;
        private String lastUsedCommand;
        private String lastResolvedPath;
    }

    @Data
    @Builder
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class ExecApprovalsFile {
        @Builder.Default
        private int version = 1;
        @Builder.Default
        private ExecSecurity security = ExecSecurity.DENY;
        @Builder.Default
        private ExecAsk ask = ExecAsk.ON_MISS;
        @Builder.Default
        private ExecSecurity askFallback = ExecSecurity.DENY;
        @Builder.Default
        private boolean autoAllowSkills = false;
        private Map<String, AgentApprovals> agents;
    }

    @Data
    @Builder
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class AgentApprovals {
        private List<AllowlistEntry> allowlist;
    }

    @Data
    @Builder
    public static class ResolvedApprovals {
        private ExecSecurity security;
        private ExecAsk ask;
        private ExecSecurity askFallback;
        private boolean autoAllowSkills;
        private List<AllowlistEntry> allowlist;
    }

    // =========================================================================
    // Load / Save
    // =========================================================================

    /**
     * Load exec approvals from a JSON file.
     */
    public static ExecApprovalsFile load(Path path) {
        if (!Files.exists(path)) {
            log.debug("Exec approvals file not found: {}, using defaults", path);
            return ExecApprovalsFile.builder().build();
        }
        try {
            String raw = Files.readString(path);
            return MAPPER.readValue(raw, ExecApprovalsFile.class);
        } catch (IOException e) {
            log.warn("Failed to read exec approvals from {}: {}", path, e.getMessage());
            return ExecApprovalsFile.builder().build();
        }
    }

    /**
     * Save exec approvals to a JSON file.
     */
    public static void save(Path path, ExecApprovalsFile file) throws IOException {
        Files.createDirectories(path.getParent());
        String json = MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(file);
        Files.writeString(path, json);
    }

    // =========================================================================
    // Resolution
    // =========================================================================

    /**
     * Resolve approvals for a specific agent, merging defaults with per-agent
     * overrides.
     */
    public static ResolvedApprovals resolve(ExecApprovalsFile file, String agentId) {
        List<AllowlistEntry> allowlist = new ArrayList<>();
        ExecSecurity security = file.getSecurity() != null ? file.getSecurity() : ExecSecurity.DENY;
        ExecAsk ask = file.getAsk() != null ? file.getAsk() : ExecAsk.ON_MISS;
        ExecSecurity askFallback = file.getAskFallback() != null ? file.getAskFallback() : ExecSecurity.DENY;
        boolean autoAllowSkills = file.isAutoAllowSkills();

        if (file.getAgents() != null && agentId != null) {
            AgentApprovals agentApprovals = file.getAgents().get(agentId);
            if (agentApprovals != null && agentApprovals.getAllowlist() != null) {
                allowlist.addAll(agentApprovals.getAllowlist());
            }
        }

        // Also load "default" agent allowlist
        if (file.getAgents() != null) {
            AgentApprovals defaultApprovals = file.getAgents().get("default");
            if (defaultApprovals != null && defaultApprovals.getAllowlist() != null) {
                for (AllowlistEntry entry : defaultApprovals.getAllowlist()) {
                    if (allowlist.stream().noneMatch(e -> Objects.equals(e.getPattern(), entry.getPattern()))) {
                        allowlist.add(entry);
                    }
                }
            }
        }

        return ResolvedApprovals.builder()
                .security(security)
                .ask(ask)
                .askFallback(askFallback)
                .autoAllowSkills(autoAllowSkills)
                .allowlist(allowlist)
                .build();
    }

    // =========================================================================
    // Matching
    // =========================================================================

    /**
     * Check if an executable is in the allowlist.
     * Supports glob-style patterns (* matches any character sequence).
     */
    public static boolean isAllowed(List<AllowlistEntry> allowlist, String executable) {
        if (allowlist == null || allowlist.isEmpty())
            return false;
        String normalized = executable.trim().toLowerCase();

        for (AllowlistEntry entry : allowlist) {
            if (entry.getPattern() == null)
                continue;
            String pattern = entry.getPattern().trim().toLowerCase();
            if (pattern.isEmpty())
                continue;

            if (pattern.equals(normalized) || pattern.equals("*")) {
                return true;
            }

            // Simple glob: convert * to regex .*
            String regex = "^" + Pattern.quote(pattern)
                    .replace("\\*", "\\E.*\\Q") + "$";
            try {
                if (normalized.matches(regex)) {
                    return true;
                }
            } catch (Exception e) {
                // Invalid pattern, skip
                log.debug("Invalid allowlist pattern: {}", pattern);
            }
        }
        return false;
    }

    /**
     * Check if a command is in the default safe bins list.
     */
    public static boolean isSafeBin(String executable) {
        if (executable == null)
            return false;
        String name = Path.of(executable).getFileName().toString().toLowerCase();
        return DEFAULT_SAFE_BINS.contains(name);
    }

    /**
     * Add an entry to the agent allowlist.
     */
    public static void addToAllowlist(ExecApprovalsFile file, String agentId, AllowlistEntry entry) {
        if (file.getAgents() == null) {
            file.setAgents(new LinkedHashMap<>());
        }
        String key = agentId != null ? agentId : "default";
        file.getAgents().computeIfAbsent(key,
                k -> AgentApprovals.builder().allowlist(new ArrayList<>()).build());

        AgentApprovals agentApprovals = file.getAgents().get(key);
        if (agentApprovals.getAllowlist() == null) {
            agentApprovals.setAllowlist(new ArrayList<>());
        }

        // Ensure unique ID
        if (entry.getId() == null || entry.getId().isEmpty()) {
            entry.setId(UUID.randomUUID().toString().substring(0, 8));
        }

        agentApprovals.getAllowlist().add(entry);
    }
}

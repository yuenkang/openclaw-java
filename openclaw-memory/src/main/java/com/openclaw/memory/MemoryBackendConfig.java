package com.openclaw.memory;

import com.openclaw.common.config.OpenClawConfig;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Memory backend configuration resolution.
 * Corresponds to TypeScript's memory/backend-config.ts.
 *
 * <p>
 * Currently only supports the "builtin" backend. QMD backend
 * resolution is a stub for future implementation.
 * </p>
 */
@Slf4j
public final class MemoryBackendConfig {

    private MemoryBackendConfig() {
    }

    public enum Backend {
        BUILTIN, QMD
    }

    public enum CitationsMode {
        AUTO, ALWAYS, NEVER
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ResolvedConfig {
        private Backend backend;
        private CitationsMode citations;
        private String workspaceDir;
    }

    /**
     * Resolve the memory backend configuration for a given agent.
     */
    public static ResolvedConfig resolve(OpenClawConfig cfg, String agentId) {
        // Default: builtin with auto citations
        Backend backend = Backend.BUILTIN;
        CitationsMode citations = CitationsMode.AUTO;

        // Try to read from config
        if (cfg != null && cfg.getMemory() != null) {
            String backendStr = cfg.getMemory().getBackend();
            if ("qmd".equalsIgnoreCase(backendStr)) {
                // QMD backend is not yet implemented — fall back to builtin
                log.info("QMD memory backend requested but not implemented in Java; using builtin");
                backend = Backend.BUILTIN;
            }
            String citStr = cfg.getMemory().getCitations();
            if ("always".equalsIgnoreCase(citStr))
                citations = CitationsMode.ALWAYS;
            else if ("never".equalsIgnoreCase(citStr))
                citations = CitationsMode.NEVER;
        }

        // Resolve workspace dir
        String wsDir = resolveWorkspaceDir(cfg, agentId);

        return ResolvedConfig.builder()
                .backend(backend)
                .citations(citations)
                .workspaceDir(wsDir)
                .build();
    }

    private static String resolveWorkspaceDir(OpenClawConfig cfg, String agentId) {
        if (cfg != null && cfg.getAgents() != null && cfg.getAgents().getDefaults() != null) {
            String ws = cfg.getAgents().getDefaults().getWorkspace();
            if (ws != null && !ws.isBlank()) {
                return ws.startsWith("~") ? ws.replace("~", System.getProperty("user.home")) : ws;
            }
        }
        return System.getProperty("user.home");
    }
}

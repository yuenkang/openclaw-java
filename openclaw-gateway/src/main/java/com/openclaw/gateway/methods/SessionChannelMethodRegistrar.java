package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import com.openclaw.common.model.AcpSession;
import com.openclaw.gateway.session.SessionStore;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Registers session-store, channel, agent-files, exec-approval,
 * and skills RPC method handlers.
 *
 * <p>
 * Corresponds to TypeScript's sessions.ts (patch/reset/delete),
 * channels.ts, agents.ts (files), exec-approval.ts, exec-approvals.ts,
 * skills.ts.
 */
@Slf4j
@Component
public class SessionChannelMethodRegistrar {

    private final GatewayMethodRouter methodRouter;
    private final ConfigService configService;
    private final SessionStore sessionStore;

    /** Well-known agent workspace file names. */
    private static final Set<String> ALLOWED_FILE_NAMES = Set.of(
            "agents.md", "soul.md", "tools.md", "identity.md",
            "user.md", "heartbeat.md", "bootstrap.md",
            "memory.md", "memories.md");

    public SessionChannelMethodRegistrar(GatewayMethodRouter methodRouter,
            ConfigService configService,
            SessionStore sessionStore) {
        this.methodRouter = methodRouter;
        this.configService = configService;
        this.sessionStore = sessionStore;
    }

    @PostConstruct
    public void registerMethods() {
        // Session store-level operations keyed by sessionKey
        methodRouter.registerMethod("sessions.patch", this::handleSessionsPatch);
        methodRouter.registerMethod("sessions.reset", this::handleSessionsReset);
        methodRouter.registerMethod("sessions.delete", this::handleSessionsDelete);

        // Channel status and lifecycle
        methodRouter.registerMethod("channels.status", this::handleChannelsStatus);
        methodRouter.registerMethod("channels.logout", this::handleChannelsLogout);

        // Agent workspace files
        methodRouter.registerMethod("agents.files.list", this::handleAgentsFilesList);
        methodRouter.registerMethod("agents.files.get", this::handleAgentsFilesGet);
        methodRouter.registerMethod("agents.files.set", this::handleAgentsFilesSet);

        // Exec approvals
        methodRouter.registerMethod("exec.approval.request", this::handleExecApprovalRequest);
        methodRouter.registerMethod("exec.approval.resolve", this::handleExecApprovalResolve);
        methodRouter.registerMethod("exec.approvals.get", this::handleExecApprovalsGet);
        methodRouter.registerMethod("exec.approvals.set", this::handleExecApprovalsSet);
        methodRouter.registerMethod("exec.approvals.node.get", this::handleExecApprovalsNodeGet);
        methodRouter.registerMethod("exec.approvals.node.set", this::handleExecApprovalsNodeSet);

        // Skills
        methodRouter.registerMethod("skills.status", this::handleSkillsStatus);
        methodRouter.registerMethod("skills.bins", this::handleSkillsBins);

        // Agent wait
        methodRouter.registerMethod("agent.wait", this::handleAgentWait);

        log.info("Registered session/channel/agent-files/exec-approval/skills methods");
    }

    // =========================================================================
    // sessions.patch
    // =========================================================================
    private CompletableFuture<Object> handleSessionsPatch(JsonNode params, GatewayConnection conn) {
        String key = textParam(params, "key", "");
        if (key.isEmpty()) {
            return fail("key required");
        }
        // Try to find existing session, auto-create if not found
        AcpSession session = sessionStore.findBySessionKey(key)
                .orElseGet(() -> sessionStore.createSession(key,
                        System.getProperty("user.dir")));
        sessionStore.updateSession(session.getSessionId(), s -> {
            if (params.has("label") && !params.get("label").isNull()) {
                s.setLabel(params.get("label").asText());
            }
            if (params.has("model") && !params.get("model").isNull()) {
                s.setModel(params.get("model").asText());
            }
            if (params.has("thinkingLevel") && !params.get("thinkingLevel").isNull()) {
                s.setThinkingLevel(params.get("thinkingLevel").asText());
            }
            if (params.has("contextTokens") && params.get("contextTokens").isNumber()) {
                s.setContextTokens(params.get("contextTokens").asInt());
            }
        });

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("key", key);
        result.put("entry", sessionStore.findBySessionKey(key).orElse(null));
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // sessions.reset
    // =========================================================================
    private CompletableFuture<Object> handleSessionsReset(JsonNode params, GatewayConnection conn) {
        String key = textParam(params, "key", "");
        if (key.isEmpty()) {
            return fail("key required");
        }
        Optional<AcpSession> opt = sessionStore.findBySessionKey(key);
        if (opt.isEmpty()) {
            return fail("session not found for key: " + key);
        }
        AcpSession session = opt.get();
        sessionStore.updateSession(session.getSessionId(), s -> {
            // New session ID = new transcript, but keep metadata
            s.setSessionId(UUID.randomUUID().toString());
            s.setInputTokens(0);
            s.setOutputTokens(0);
            s.setTotalTokens(0);
        });

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("key", key);
        result.put("entry", sessionStore.findBySessionKey(key).orElse(null));
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // sessions.delete
    // =========================================================================
    private CompletableFuture<Object> handleSessionsDelete(JsonNode params, GatewayConnection conn) {
        String key = textParam(params, "key", "");
        if (key.isEmpty()) {
            return fail("key required");
        }
        // Protect main session
        if ("main".equals(key)) {
            return fail("Cannot delete the main session.");
        }
        Optional<AcpSession> opt = sessionStore.findBySessionKey(key);
        boolean existed = opt.isPresent();
        if (existed) {
            sessionStore.removeSession(opt.get().getSessionId());
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("key", key);
        result.put("deleted", existed);
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // channels.status
    // =========================================================================
    private CompletableFuture<Object> handleChannelsStatus(JsonNode params, GatewayConnection conn) {
        OpenClawConfig cfg = configService.loadConfig();
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("ts", System.currentTimeMillis());

        Map<String, Object> channels = new LinkedHashMap<>();
        if (cfg.getChannels() != null && cfg.getChannels().getProviders() != null) {
            for (var entry : cfg.getChannels().getProviders().entrySet()) {
                Map<String, Object> ch = new LinkedHashMap<>();
                ch.put("configured", entry.getValue() != null);
                ch.put("enabled", entry.getValue() != null);
                channels.put(entry.getKey(), ch);
            }
        }
        payload.put("channels", channels);
        payload.put("channelAccounts", Map.of());
        return CompletableFuture.completedFuture(payload);
    }

    // =========================================================================
    // channels.logout
    // =========================================================================
    private CompletableFuture<Object> handleChannelsLogout(JsonNode params, GatewayConnection conn) {
        String channel = textParam(params, "channel", "");
        if (channel.isEmpty()) {
            return fail("channel parameter required");
        }
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("channel", channel);
        result.put("cleared", false);
        result.put("loggedOut", false);
        result.put("message", "Channel logout not fully implemented yet");
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // agents.files.list
    // =========================================================================
    private CompletableFuture<Object> handleAgentsFilesList(JsonNode params, GatewayConnection conn) {
        String agentId = textParam(params, "agentId", "");
        if (agentId.isEmpty()) {
            return fail("agentId required");
        }
        OpenClawConfig cfg = configService.loadConfig();
        String workspaceDir = resolveAgentWorkspaceDir(cfg, agentId);
        if (workspaceDir == null) {
            return fail("unknown agent id: " + agentId);
        }

        List<Map<String, Object>> files = new ArrayList<>();
        for (String name : ALLOWED_FILE_NAMES) {
            Map<String, Object> fileEntry = new LinkedHashMap<>();
            fileEntry.put("name", name);
            Path filePath = Paths.get(workspaceDir, name);
            fileEntry.put("path", filePath.toString());
            if (Files.exists(filePath) && Files.isRegularFile(filePath)) {
                fileEntry.put("missing", false);
                try {
                    BasicFileAttributes attrs = Files.readAttributes(filePath, BasicFileAttributes.class);
                    fileEntry.put("size", attrs.size());
                    fileEntry.put("updatedAtMs", attrs.lastModifiedTime().toMillis());
                } catch (IOException e) {
                    fileEntry.put("size", 0L);
                }
            } else {
                fileEntry.put("missing", true);
            }
            files.add(fileEntry);
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("agentId", agentId);
        result.put("workspace", workspaceDir);
        result.put("files", files);
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // agents.files.get
    // =========================================================================
    private CompletableFuture<Object> handleAgentsFilesGet(JsonNode params, GatewayConnection conn) {
        String agentId = textParam(params, "agentId", "");
        String name = textParam(params, "name", "");
        if (agentId.isEmpty())
            return fail("agentId required");
        if (!ALLOWED_FILE_NAMES.contains(name))
            return fail("unsupported file \"" + name + "\"");

        OpenClawConfig cfg = configService.loadConfig();
        String workspaceDir = resolveAgentWorkspaceDir(cfg, agentId);
        if (workspaceDir == null)
            return fail("unknown agent id: " + agentId);

        Path filePath = Paths.get(workspaceDir, name);
        Map<String, Object> fileEntry = new LinkedHashMap<>();
        fileEntry.put("name", name);
        fileEntry.put("path", filePath.toString());

        if (Files.exists(filePath) && Files.isRegularFile(filePath)) {
            try {
                String content = Files.readString(filePath);
                BasicFileAttributes attrs = Files.readAttributes(filePath, BasicFileAttributes.class);
                fileEntry.put("missing", false);
                fileEntry.put("size", attrs.size());
                fileEntry.put("updatedAtMs", attrs.lastModifiedTime().toMillis());
                fileEntry.put("content", content);
            } catch (IOException e) {
                fileEntry.put("missing", true);
            }
        } else {
            fileEntry.put("missing", true);
        }

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("agentId", agentId);
        result.put("workspace", workspaceDir);
        result.put("file", fileEntry);
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // agents.files.set
    // =========================================================================
    private CompletableFuture<Object> handleAgentsFilesSet(JsonNode params, GatewayConnection conn) {
        String agentId = textParam(params, "agentId", "");
        String name = textParam(params, "name", "");
        if (agentId.isEmpty())
            return fail("agentId required");
        if (!ALLOWED_FILE_NAMES.contains(name))
            return fail("unsupported file \"" + name + "\"");

        OpenClawConfig cfg = configService.loadConfig();
        String workspaceDir = resolveAgentWorkspaceDir(cfg, agentId);
        if (workspaceDir == null)
            return fail("unknown agent id: " + agentId);

        String content = textParam(params, "content", "");
        Path filePath = Paths.get(workspaceDir, name);
        try {
            Files.createDirectories(filePath.getParent());
            Files.writeString(filePath, content);
            BasicFileAttributes attrs = Files.readAttributes(filePath, BasicFileAttributes.class);

            Map<String, Object> fileEntry = new LinkedHashMap<>();
            fileEntry.put("name", name);
            fileEntry.put("path", filePath.toString());
            fileEntry.put("missing", false);
            fileEntry.put("size", attrs.size());
            fileEntry.put("updatedAtMs", attrs.lastModifiedTime().toMillis());
            fileEntry.put("content", content);

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("agentId", agentId);
            result.put("workspace", workspaceDir);
            result.put("file", fileEntry);
            return CompletableFuture.completedFuture(result);
        } catch (IOException e) {
            return fail("Failed to write file: " + e.getMessage());
        }
    }

    // =========================================================================
    // exec.approval.request
    // =========================================================================
    private CompletableFuture<Object> handleExecApprovalRequest(JsonNode params, GatewayConnection conn) {
        String sessionId = textParam(params, "sessionId", "");
        String command = textParam(params, "command", "");
        if (sessionId.isEmpty() || command.isEmpty()) {
            return fail("sessionId and command required");
        }
        String approvalId = UUID.randomUUID().toString();
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("approvalId", approvalId);
        result.put("status", "pending");
        result.put("command", command);
        result.put("sessionId", sessionId);
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // exec.approval.resolve
    // =========================================================================
    private CompletableFuture<Object> handleExecApprovalResolve(JsonNode params, GatewayConnection conn) {
        String approvalId = textParam(params, "approvalId", "");
        String action = textParam(params, "action", "");
        if (approvalId.isEmpty() || action.isEmpty()) {
            return fail("approvalId and action (approve|deny) required");
        }
        boolean approved = "approve".equalsIgnoreCase(action);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("approvalId", approvalId);
        result.put("approved", approved);
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // exec.approvals.get / set / node.get / node.set
    // =========================================================================
    private CompletableFuture<Object> handleExecApprovalsGet(JsonNode params, GatewayConnection conn) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("mode", "always");
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleExecApprovalsSet(JsonNode params, GatewayConnection conn) {
        String mode = textParam(params, "mode", "always");
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("mode", mode);
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleExecApprovalsNodeGet(JsonNode params, GatewayConnection conn) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("mode", "always");
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleExecApprovalsNodeSet(JsonNode params, GatewayConnection conn) {
        String mode = textParam(params, "mode", "always");
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("mode", mode);
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // skills.status / skills.bins
    // =========================================================================
    private CompletableFuture<Object> handleSkillsStatus(JsonNode params, GatewayConnection conn) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ts", System.currentTimeMillis());
        result.put("skills", List.of());
        return CompletableFuture.completedFuture(result);
    }

    private CompletableFuture<Object> handleSkillsBins(JsonNode params, GatewayConnection conn) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("bins", List.of());
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // agent.wait
    // =========================================================================
    private CompletableFuture<Object> handleAgentWait(JsonNode params, GatewayConnection conn) {
        String jobId = textParam(params, "jobId", "");
        if (jobId.isEmpty()) {
            return fail("jobId required");
        }
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("jobId", jobId);
        result.put("status", "completed");
        return CompletableFuture.completedFuture(result);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    /**
     * Resolve the workspace directory for the given agent ID.
     * For the default agent, returns the config directory.
     * For named agents, returns configDir/agents/{agentId}.
     */
    private String resolveAgentWorkspaceDir(OpenClawConfig cfg, String agentId) {
        String normalized = agentId.toLowerCase().trim();
        Path configDir = configService.getConfigPath().getParent();
        if (configDir == null)
            return null;

        if ("default".equals(normalized) || normalized.isEmpty()) {
            return configDir.toString();
        }
        return configDir.resolve("agents").resolve(normalized).toString();
    }

    private String textParam(JsonNode params, String field, String defaultValue) {
        if (params != null && params.has(field) && !params.get(field).isNull()) {
            return params.get(field).asText(defaultValue).trim();
        }
        return defaultValue;
    }

    private static CompletableFuture<Object> fail(String message) {
        return CompletableFuture.failedFuture(new IllegalArgumentException(message));
    }
}

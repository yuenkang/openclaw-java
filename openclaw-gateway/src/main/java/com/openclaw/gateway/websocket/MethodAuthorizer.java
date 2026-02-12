package com.openclaw.gateway.websocket;

import com.openclaw.gateway.protocol.ProtocolTypes.ErrorCodes;
import com.openclaw.gateway.protocol.ProtocolTypes.ErrorShape;

import java.util.Set;

/**
 * Scope-based method authorization.
 * Aligns with TypeScript server-methods.ts authorizeGatewayMethod().
 */
public final class MethodAuthorizer {

    private MethodAuthorizer() {
    }

    private static final String ADMIN_SCOPE = "operator.admin";
    private static final String READ_SCOPE = "operator.read";
    private static final String WRITE_SCOPE = "operator.write";
    private static final String APPROVALS_SCOPE = "operator.approvals";
    private static final String PAIRING_SCOPE = "operator.pairing";

    private static final Set<String> NODE_ROLE_METHODS = Set.of(
            "node.invoke.result", "node.event", "skills.bins");

    private static final Set<String> APPROVAL_METHODS = Set.of(
            "exec.approval.request", "exec.approval.resolve");

    private static final Set<String> PAIRING_METHODS = Set.of(
            "node.pair.request", "node.pair.list", "node.pair.approve", "node.pair.reject",
            "node.pair.verify", "device.pair.list", "device.pair.approve", "device.pair.reject",
            "device.token.rotate", "device.token.revoke", "node.rename");

    private static final Set<String> READ_METHODS = Set.of(
            "health", "logs.tail", "channels.status", "status",
            "usage.status", "usage.cost", "tts.status", "tts.providers",
            "models.list", "agents.list", "agent.identity.get",
            "skills.status", "voicewake.get",
            "sessions.list", "sessions.preview",
            "cron.list", "cron.status", "cron.runs",
            "system-presence", "last-heartbeat",
            "node.list", "node.describe", "chat.history");

    private static final Set<String> WRITE_METHODS = Set.of(
            "send", "agent", "agent.wait", "wake", "talk.mode",
            "tts.enable", "tts.disable", "tts.convert", "tts.setProvider",
            "voicewake.set", "node.invoke",
            "chat.send", "chat.abort", "browser.request");

    private static final Set<String> ADMIN_ONLY_PREFIXES = Set.of(
            "config.", "wizard.", "update.", "exec.approvals.");

    private static final Set<String> ADMIN_ONLY_METHODS = Set.of(
            "channels.logout", "skills.install", "skills.update",
            "cron.add", "cron.update", "cron.remove", "cron.run",
            "sessions.patch", "sessions.reset", "sessions.delete", "sessions.compact");

    /**
     * Check if the given method is allowed for the connection's role and scopes.
     *
     * @return null if authorized, ErrorShape if unauthorized
     */
    public static ErrorShape authorize(String method, GatewayConnection conn) {
        if (conn == null || conn.getConnectParams() == null) {
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "not connected");
        }

        String role = conn.getRole() != null ? conn.getRole() : "operator";
        var scopes = conn.getScopes();

        // Node role methods
        if (NODE_ROLE_METHODS.contains(method)) {
            if ("node".equals(role)) {
                return null; // allowed
            }
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "unauthorized role: " + role);
        }

        // Only operators can call non-node methods
        if ("node".equals(role)) {
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "unauthorized role: " + role);
        }
        if (!"operator".equals(role)) {
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "unauthorized role: " + role);
        }

        // Admin scope grants everything
        if (scopes != null && scopes.contains(ADMIN_SCOPE)) {
            return null;
        }

        // Approval methods
        if (APPROVAL_METHODS.contains(method)) {
            if (scopes != null && scopes.contains(APPROVALS_SCOPE)) {
                return null;
            }
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + APPROVALS_SCOPE);
        }

        // Pairing methods
        if (PAIRING_METHODS.contains(method)) {
            if (scopes != null && scopes.contains(PAIRING_SCOPE)) {
                return null;
            }
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + PAIRING_SCOPE);
        }

        // Read methods
        if (READ_METHODS.contains(method)) {
            if (scopes != null && (scopes.contains(READ_SCOPE) || scopes.contains(WRITE_SCOPE))) {
                return null;
            }
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + READ_SCOPE);
        }

        // Write methods
        if (WRITE_METHODS.contains(method)) {
            if (scopes != null && scopes.contains(WRITE_SCOPE)) {
                return null;
            }
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + WRITE_SCOPE);
        }

        // Admin-only prefixes
        for (String prefix : ADMIN_ONLY_PREFIXES) {
            if (method.startsWith(prefix)) {
                return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + ADMIN_SCOPE);
            }
        }

        // Admin-only methods
        if (ADMIN_ONLY_METHODS.contains(method)) {
            return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + ADMIN_SCOPE);
        }

        // Default: require admin
        return ErrorShape.of(ErrorCodes.INVALID_REQUEST, "missing scope: " + ADMIN_SCOPE);
    }
}

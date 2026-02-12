package com.openclaw.agent.sandbox;

import com.openclaw.agent.sandbox.SandboxTypes.SandboxScope;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * Shared sandbox utility functions — session key slugs and scope resolution.
 * Corresponds to TypeScript sandbox/shared.ts.
 */
public final class SandboxShared {

    private SandboxShared() {
    }

    /**
     * Slugify a session key to produce a safe directory / container name suffix.
     */
    public static String slugifySessionKey(String value) {
        String trimmed = value == null || value.isBlank() ? "session" : value.trim();
        String hash = sha1Hex(trimmed).substring(0, 8);
        String safe = trimmed.toLowerCase()
                .replaceAll("[^a-z0-9._-]+", "-")
                .replaceAll("^-+|-+$", "");
        String base = safe.length() > 32 ? safe.substring(0, 32) : safe;
        if (base.isEmpty())
            base = "session";
        return base + "-" + hash;
    }

    /**
     * Resolve the sandbox workspace directory for a given session key.
     */
    public static String resolveSandboxWorkspaceDir(String root, String sessionKey) {
        String resolvedRoot = resolvePath(root);
        String slug = slugifySessionKey(sessionKey);
        return Paths.get(resolvedRoot, slug).toString();
    }

    /**
     * Resolve sandbox scope key — determines container grouping.
     */
    public static String resolveSandboxScopeKey(SandboxScope scope, String sessionKey) {
        String trimmed = sessionKey == null || sessionKey.isBlank() ? "main" : sessionKey.trim();
        if (scope == SandboxScope.SHARED) {
            return "shared";
        }
        if (scope == SandboxScope.SESSION) {
            return trimmed;
        }
        // AGENT scope — extract agent ID from session key
        String agentId = resolveAgentIdFromSessionKey(trimmed);
        return "agent:" + agentId;
    }

    /**
     * Resolve sandbox agent ID from scope key.
     */
    public static String resolveSandboxAgentId(String scopeKey) {
        if (scopeKey == null || scopeKey.isBlank() || "shared".equals(scopeKey.trim())) {
            return null;
        }
        String trimmed = scopeKey.trim();
        String[] parts = trimmed.split(":");
        if (parts.length >= 2 && "agent".equals(parts[0]) && !parts[1].isEmpty()) {
            return normalizeAgentId(parts[1]);
        }
        return resolveAgentIdFromSessionKey(trimmed);
    }

    // ── Helpers ─────────────────────────────────────────────────────

    /**
     * Extract agent ID from session key (format: "agentId/sessionName" or just
     * "sessionName").
     */
    static String resolveAgentIdFromSessionKey(String sessionKey) {
        if (sessionKey == null || sessionKey.isBlank())
            return "main";
        String trimmed = sessionKey.trim();
        int slashIdx = trimmed.indexOf('/');
        if (slashIdx > 0 && slashIdx < trimmed.length() - 1) {
            return normalizeAgentId(trimmed.substring(0, slashIdx));
        }
        return "main";
    }

    static String normalizeAgentId(String id) {
        if (id == null || id.isBlank())
            return "main";
        return id.trim().toLowerCase().replaceAll("[^a-z0-9_-]+", "-");
    }

    /**
     * Resolve a path, expanding ~ to user home.
     */
    static String resolvePath(String filePath) {
        if (filePath == null)
            return System.getProperty("user.home");
        String normalized = filePath.trim();
        if ("~".equals(normalized)) {
            return System.getProperty("user.home");
        }
        if (normalized.startsWith("~/")) {
            return System.getProperty("user.home") + normalized.substring(1);
        }
        return normalized;
    }

    private static String sha1Hex(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] digest = md.digest(input.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(digest);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-1 not available", e);
        }
    }
}

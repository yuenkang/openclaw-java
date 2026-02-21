package com.openclaw.sandbox;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.openclaw.sandbox.SandboxTypes.SandboxDockerConfig;
import com.openclaw.sandbox.SandboxTypes.SandboxWorkspaceAccess;
import lombok.Builder;
import lombok.Data;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Compute a deterministic hash of sandbox configuration for change detection.
 * Corresponds to TypeScript sandbox/config-hash.ts.
 */
public final class SandboxConfigHash {

    private SandboxConfigHash() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .configure(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS, true);

    @Data
    @Builder
    public static class SandboxHashInput {
        private SandboxDockerConfig docker;
        private SandboxWorkspaceAccess workspaceAccess;
        private String workspaceDir;
        private String agentWorkspaceDir;
    }

    /**
     * Compute SHA-1 hash of the normalized sandbox configuration input.
     */
    public static String computeSandboxConfigHash(SandboxHashInput input) {
        try {
            Map<String, Object> payload = new LinkedHashMap<>();
            payload.put("docker", MAPPER.convertValue(input.getDocker(), Map.class));
            payload.put("workspaceAccess", input.getWorkspaceAccess() != null
                    ? input.getWorkspaceAccess().name().toLowerCase()
                    : "none");
            payload.put("workspaceDir", input.getWorkspaceDir());
            payload.put("agentWorkspaceDir", input.getAgentWorkspaceDir());

            // Normalize: sort keys, remove nulls
            Object normalized = normalizeForHash(payload);
            String raw = MAPPER.writeValueAsString(normalized);

            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] digest = md.digest(raw.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(digest);
        } catch (JsonProcessingException | NoSuchAlgorithmException e) {
            throw new RuntimeException("Failed to compute sandbox config hash", e);
        }
    }

    @SuppressWarnings("unchecked")
    private static Object normalizeForHash(Object value) {
        if (value == null)
            return null;

        if (value instanceof List<?> list) {
            List<Object> normalized = new ArrayList<>();
            for (Object item : list) {
                Object n = normalizeForHash(item);
                if (n != null)
                    normalized.add(n);
            }
            // Sort primitives
            if (normalized.stream().allMatch(SandboxConfigHash::isPrimitive)) {
                normalized.sort(Comparator.comparing(Object::toString));
            }
            return normalized;
        }

        if (value instanceof Map<?, ?> map) {
            TreeMap<String, Object> sorted = new TreeMap<>();
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                Object n = normalizeForHash(entry.getValue());
                if (n != null) {
                    sorted.put(String.valueOf(entry.getKey()), n);
                }
            }
            return sorted;
        }

        return value;
    }

    private static boolean isPrimitive(Object value) {
        return value instanceof String || value instanceof Number || value instanceof Boolean;
    }
}

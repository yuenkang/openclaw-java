package com.openclaw.node;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Device pairing persistence service.
 * Stores pending/paired device state and auth tokens in a JSON file.
 * Corresponds to TypeScript's infra/device-pairing.ts.
 */
@Slf4j
public class DevicePairingService {

    private static final long PENDING_TTL_MS = 5 * 60 * 1000; // 5 minutes

    private final Path stateFile;
    private final ObjectMapper objectMapper;
    private final ReentrantLock lock = new ReentrantLock();

    public DevicePairingService(Path stateDir, ObjectMapper objectMapper) {
        this.stateFile = stateDir.resolve("device-pairing.json");
        this.objectMapper = objectMapper;
    }

    // --- Public API ---

    public PairingList listPairings() {
        lock.lock();
        try {
            StateFile state = loadState();
            pruneExpired(state);
            return PairingList.builder()
                    .pending(new ArrayList<>(state.pendingById.values()))
                    .paired(new ArrayList<>(state.pairedByDeviceId.values()))
                    .build();
        } finally {
            lock.unlock();
        }
    }

    public ApproveResult approvePairing(String requestId) {
        lock.lock();
        try {
            StateFile state = loadState();
            PendingRequest pending = state.pendingById.remove(requestId);
            if (pending == null)
                return null;

            long now = System.currentTimeMillis();
            String role = pending.getRole() != null ? pending.getRole() : "operator";
            List<String> scopes = pending.getScopes() != null ? pending.getScopes() : List.of();

            // Create initial token
            DeviceAuthToken token = DeviceAuthToken.builder()
                    .token(UUID.randomUUID().toString())
                    .role(role)
                    .scopes(scopes)
                    .createdAtMs(now)
                    .build();

            Map<String, DeviceAuthToken> tokens = new LinkedHashMap<>();
            tokens.put(role, token);

            PairedDevice device = PairedDevice.builder()
                    .deviceId(pending.getDeviceId())
                    .publicKey(pending.getPublicKey())
                    .displayName(pending.getDisplayName())
                    .platform(pending.getPlatform())
                    .clientId(pending.getClientId())
                    .clientMode(pending.getClientMode())
                    .role(role)
                    .roles(pending.getRoles())
                    .scopes(scopes)
                    .remoteIp(pending.getRemoteIp())
                    .tokens(tokens)
                    .createdAtMs(now)
                    .approvedAtMs(now)
                    .build();

            state.pairedByDeviceId.put(device.getDeviceId(), device);
            persistState(state);

            return ApproveResult.builder()
                    .requestId(requestId)
                    .device(device)
                    .build();
        } finally {
            lock.unlock();
        }
    }

    public RejectResult rejectPairing(String requestId) {
        lock.lock();
        try {
            StateFile state = loadState();
            PendingRequest pending = state.pendingById.remove(requestId);
            if (pending == null)
                return null;
            persistState(state);
            return RejectResult.builder()
                    .requestId(requestId)
                    .deviceId(pending.getDeviceId())
                    .build();
        } finally {
            lock.unlock();
        }
    }

    public DeviceAuthToken rotateToken(String deviceId, String role, List<String> scopes) {
        lock.lock();
        try {
            StateFile state = loadState();
            PairedDevice device = state.pairedByDeviceId.get(deviceId);
            if (device == null)
                return null;

            if (device.getTokens() == null) {
                device.setTokens(new LinkedHashMap<>());
            }

            DeviceAuthToken existing = device.getTokens().get(role);
            long now = System.currentTimeMillis();

            DeviceAuthToken newToken = DeviceAuthToken.builder()
                    .token(UUID.randomUUID().toString())
                    .role(role)
                    .scopes(scopes != null ? scopes
                            : (existing != null ? existing.getScopes() : List.of()))
                    .createdAtMs(existing != null ? existing.getCreatedAtMs() : now)
                    .rotatedAtMs(now)
                    .build();

            device.getTokens().put(role, newToken);
            persistState(state);
            return newToken;
        } finally {
            lock.unlock();
        }
    }

    public DeviceAuthToken revokeToken(String deviceId, String role) {
        lock.lock();
        try {
            StateFile state = loadState();
            PairedDevice device = state.pairedByDeviceId.get(deviceId);
            if (device == null || device.getTokens() == null)
                return null;

            DeviceAuthToken token = device.getTokens().get(role);
            if (token == null)
                return null;

            token.setRevokedAtMs(System.currentTimeMillis());
            persistState(state);
            return token;
        } finally {
            lock.unlock();
        }
    }

    // --- State file I/O ---

    private StateFile loadState() {
        try {
            if (Files.exists(stateFile)) {
                return objectMapper.readValue(stateFile.toFile(), StateFile.class);
            }
        } catch (IOException e) {
            log.warn("Failed to load device pairing state: {}", e.getMessage());
        }
        return new StateFile();
    }

    private void persistState(StateFile state) {
        try {
            Files.createDirectories(stateFile.getParent());
            Path tmp = stateFile.resolveSibling(stateFile.getFileName() + ".tmp");
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(tmp.toFile(), state);
            Files.move(tmp, stateFile, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException e) {
            log.error("Failed to persist device pairing state: {}", e.getMessage());
        }
    }

    private void pruneExpired(StateFile state) {
        long now = System.currentTimeMillis();
        state.pendingById.entrySet().removeIf(e -> now - e.getValue().getTs() > PENDING_TTL_MS);
    }

    // --- Redaction helper (hides raw token values) ---

    public static Map<String, Object> redactDevice(PairedDevice device) {
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("deviceId", device.getDeviceId());
        result.put("displayName", device.getDisplayName());
        result.put("platform", device.getPlatform());
        result.put("role", device.getRole());
        result.put("roles", device.getRoles());
        result.put("scopes", device.getScopes());
        result.put("remoteIp", device.getRemoteIp());
        result.put("createdAtMs", device.getCreatedAtMs());
        result.put("approvedAtMs", device.getApprovedAtMs());
        // Summarize tokens (without raw token string)
        if (device.getTokens() != null) {
            List<Map<String, Object>> summaries = new ArrayList<>();
            for (DeviceAuthToken t : device.getTokens().values()) {
                Map<String, Object> s = new LinkedHashMap<>();
                s.put("role", t.getRole());
                s.put("scopes", t.getScopes());
                s.put("createdAtMs", t.getCreatedAtMs());
                if (t.getRotatedAtMs() != null)
                    s.put("rotatedAtMs", t.getRotatedAtMs());
                if (t.getRevokedAtMs() != null)
                    s.put("revokedAtMs", t.getRevokedAtMs());
                summaries.add(s);
            }
            result.put("tokens", summaries);
        }
        return result;
    }

    // --- Types ---

    @Data
    public static class StateFile {
        private Map<String, PendingRequest> pendingById = new LinkedHashMap<>();
        private Map<String, PairedDevice> pairedByDeviceId = new LinkedHashMap<>();
    }

    @Data
    @Builder
    public static class PendingRequest {
        private String requestId;
        private String deviceId;
        private String publicKey;
        private String displayName;
        private String platform;
        private String clientId;
        private String clientMode;
        private String role;
        private List<String> roles;
        private List<String> scopes;
        private String remoteIp;
        private boolean silent;
        private long ts;
    }

    @Data
    @Builder
    public static class DeviceAuthToken {
        private String token;
        private String role;
        private List<String> scopes;
        private long createdAtMs;
        private Long rotatedAtMs;
        private Long revokedAtMs;
        private Long lastUsedAtMs;
    }

    @Data
    @Builder
    public static class PairedDevice {
        private String deviceId;
        private String publicKey;
        private String displayName;
        private String platform;
        private String clientId;
        private String clientMode;
        private String role;
        private List<String> roles;
        private List<String> scopes;
        private String remoteIp;
        private Map<String, DeviceAuthToken> tokens;
        private long createdAtMs;
        private long approvedAtMs;
    }

    @Data
    @Builder
    public static class PairingList {
        private List<PendingRequest> pending;
        private List<PairedDevice> paired;
    }

    @Data
    @Builder
    public static class ApproveResult {
        private String requestId;
        private PairedDevice device;
    }

    @Data
    @Builder
    public static class RejectResult {
        private String requestId;
        private String deviceId;
    }
}

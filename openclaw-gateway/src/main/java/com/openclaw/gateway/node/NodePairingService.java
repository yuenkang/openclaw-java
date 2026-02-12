package com.openclaw.gateway.node;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Node pairing persistence service.
 * Stores pending/paired node state in a JSON file.
 * Corresponds to TypeScript's infra/node-pairing.ts.
 */
@Slf4j
public class NodePairingService {

    private static final long PENDING_TTL_MS = 5 * 60 * 1000; // 5 minutes

    private final Path stateFile;
    private final ObjectMapper objectMapper;
    private final ReentrantLock lock = new ReentrantLock();

    public NodePairingService(Path stateDir, ObjectMapper objectMapper) {
        this.stateFile = stateDir.resolve("node-pairing.json");
        this.objectMapper = objectMapper;
    }

    // --- Public API ---

    public PairingResult requestPairing(PairingRequest req) {
        lock.lock();
        try {
            StateFile state = loadState();
            pruneExpired(state);

            // Check if already paired — auto-repair
            PairedNode existing = state.pairedByNodeId.get(req.getNodeId());
            if (existing != null) {
                return PairingResult.builder()
                        .status("paired")
                        .created(false)
                        .request(null)
                        .build();
            }

            // Check for existing pending
            for (PendingRequest p : state.pendingById.values()) {
                if (p.getNodeId().equals(req.getNodeId())) {
                    return PairingResult.builder()
                            .status("pending")
                            .created(false)
                            .request(p)
                            .build();
                }
            }

            // Create new pending request
            PendingRequest pending = PendingRequest.builder()
                    .requestId(UUID.randomUUID().toString())
                    .nodeId(req.getNodeId())
                    .displayName(req.getDisplayName())
                    .platform(req.getPlatform())
                    .version(req.getVersion())
                    .coreVersion(req.getCoreVersion())
                    .uiVersion(req.getUiVersion())
                    .deviceFamily(req.getDeviceFamily())
                    .modelIdentifier(req.getModelIdentifier())
                    .caps(req.getCaps())
                    .commands(req.getCommands())
                    .remoteIp(req.getRemoteIp())
                    .silent(req.isSilent())
                    .ts(System.currentTimeMillis())
                    .build();

            state.pendingById.put(pending.getRequestId(), pending);
            persistState(state);

            return PairingResult.builder()
                    .status("pending")
                    .created(true)
                    .request(pending)
                    .build();
        } finally {
            lock.unlock();
        }
    }

    public PairingList listPairings() {
        lock.lock();
        try {
            StateFile state = loadState();
            pruneExpired(state);
            return PairingList.builder()
                    .pending(new ArrayList<>(state.pendingById.values()))
                    .paired(new ArrayList<>(state.pairedByNodeId.values()))
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

            String token = UUID.randomUUID().toString();
            long now = System.currentTimeMillis();

            PairedNode node = PairedNode.builder()
                    .nodeId(pending.getNodeId())
                    .token(token)
                    .displayName(pending.getDisplayName())
                    .platform(pending.getPlatform())
                    .version(pending.getVersion())
                    .coreVersion(pending.getCoreVersion())
                    .uiVersion(pending.getUiVersion())
                    .deviceFamily(pending.getDeviceFamily())
                    .modelIdentifier(pending.getModelIdentifier())
                    .caps(pending.getCaps())
                    .commands(pending.getCommands())
                    .remoteIp(pending.getRemoteIp())
                    .createdAtMs(now)
                    .approvedAtMs(now)
                    .build();

            state.pairedByNodeId.put(node.getNodeId(), node);
            persistState(state);

            return ApproveResult.builder()
                    .requestId(requestId)
                    .node(node)
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
                    .nodeId(pending.getNodeId())
                    .build();
        } finally {
            lock.unlock();
        }
    }

    public VerifyResult verifyToken(String nodeId, String token) {
        lock.lock();
        try {
            StateFile state = loadState();
            PairedNode node = state.pairedByNodeId.get(nodeId);
            if (node == null || !token.equals(node.getToken())) {
                return VerifyResult.builder().ok(false).build();
            }
            return VerifyResult.builder().ok(true).node(node).build();
        } finally {
            lock.unlock();
        }
    }

    public PairedNode renamePairedNode(String nodeId, String displayName) {
        lock.lock();
        try {
            StateFile state = loadState();
            PairedNode node = state.pairedByNodeId.get(nodeId);
            if (node == null)
                return null;
            node.setDisplayName(displayName);
            persistState(state);
            return node;
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
            log.warn("Failed to load node pairing state: {}", e.getMessage());
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
            log.error("Failed to persist node pairing state: {}", e.getMessage());
        }
    }

    private void pruneExpired(StateFile state) {
        long now = System.currentTimeMillis();
        state.pendingById.entrySet().removeIf(e -> now - e.getValue().getTs() > PENDING_TTL_MS);
    }

    // --- Types ---

    @Data
    public static class StateFile {
        private Map<String, PendingRequest> pendingById = new LinkedHashMap<>();
        private Map<String, PairedNode> pairedByNodeId = new LinkedHashMap<>();
    }

    @Data
    @Builder
    public static class PairingRequest {
        private String nodeId;
        private String displayName;
        private String platform;
        private String version;
        private String coreVersion;
        private String uiVersion;
        private String deviceFamily;
        private String modelIdentifier;
        private List<String> caps;
        private List<String> commands;
        private String remoteIp;
        private boolean silent;
    }

    @Data
    @Builder
    public static class PendingRequest {
        private String requestId;
        private String nodeId;
        private String displayName;
        private String platform;
        private String version;
        private String coreVersion;
        private String uiVersion;
        private String deviceFamily;
        private String modelIdentifier;
        private List<String> caps;
        private List<String> commands;
        private String remoteIp;
        private boolean silent;
        private long ts;
    }

    @Data
    @Builder
    public static class PairedNode {
        private String nodeId;
        private String token;
        private String displayName;
        private String platform;
        private String version;
        private String coreVersion;
        private String uiVersion;
        private String deviceFamily;
        private String modelIdentifier;
        private List<String> caps;
        private List<String> commands;
        private String remoteIp;
        private long createdAtMs;
        private long approvedAtMs;
        private Long lastConnectedAtMs;
    }

    @Data
    @Builder
    public static class PairingList {
        private List<PendingRequest> pending;
        private List<PairedNode> paired;
    }

    @Data
    @Builder
    public static class PairingResult {
        private String status;
        private boolean created;
        private PendingRequest request;
    }

    @Data
    @Builder
    public static class ApproveResult {
        private String requestId;
        private PairedNode node;
    }

    @Data
    @Builder
    public static class RejectResult {
        private String requestId;
        private String nodeId;
    }

    @Data
    @Builder
    public static class VerifyResult {
        private boolean ok;
        private PairedNode node;
    }
}

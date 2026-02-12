package com.openclaw.gateway.methods;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.websocket.EventBroadcaster;
import com.openclaw.gateway.websocket.GatewayConnection;
import com.openclaw.gateway.websocket.GatewayMethodRouter;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Registers miscellaneous RPC methods:
 * <ul>
 * <li>talk.mode — broadcast talk mode changes</li>
 * <li>voicewake.get / voicewake.set — voice wake word config</li>
 * <li>send, poll — outbound message delivery (stub)</li>
 * <li>tts.* — text-to-speech (stub)</li>
 * <li>browser.request — browser control (stub)</li>
 * <li>web.login.start / web.login.wait — QR login (stub)</li>
 * <li>update.run — self-update (stub)</li>
 * <li>skills.install / skills.update — skill management</li>
 * </ul>
 */
@Slf4j
public class MiscMethodRegistrar {

    private static final List<String> DEFAULT_TRIGGERS = List.of("openclaw", "claude", "computer");

    private final GatewayMethodRouter router;
    private final EventBroadcaster broadcaster;
    private final ObjectMapper objectMapper;
    private final Path stateDir;
    private final ReentrantLock voiceLock = new ReentrantLock();

    public MiscMethodRegistrar(GatewayMethodRouter router,
            EventBroadcaster broadcaster,
            ObjectMapper objectMapper,
            Path stateDir) {
        this.router = router;
        this.broadcaster = broadcaster;
        this.objectMapper = objectMapper;
        this.stateDir = stateDir;
    }

    public void registerMethods() {
        // ── Implemented ──
        router.registerMethod("talk.mode", this::handleTalkMode);
        router.registerMethod("voicewake.get", this::handleVoicewakeGet);
        router.registerMethod("voicewake.set", this::handleVoicewakeSet);
        router.registerMethod("skills.install", this::handleSkillsInstall);
        router.registerMethod("skills.update", this::handleSkillsUpdate);

        // ── Stubs (return UNAVAILABLE) ──
        router.registerMethod("send", this::stubUnavailable);
        router.registerMethod("poll", this::stubUnavailable);
        router.registerMethod("tts.status", this::stubUnavailable);
        router.registerMethod("tts.enable", this::stubUnavailable);
        router.registerMethod("tts.disable", this::stubUnavailable);
        router.registerMethod("tts.convert", this::stubUnavailable);
        router.registerMethod("tts.setProvider", this::stubUnavailable);
        router.registerMethod("tts.providers", this::stubUnavailable);
        router.registerMethod("browser.request", this::stubUnavailable);
        router.registerMethod("web.login.start", this::stubUnavailable);
        router.registerMethod("web.login.wait", this::stubUnavailable);
        router.registerMethod("update.run", this::stubUnavailable);

        log.info("Registered 17 misc methods (5 implemented + 12 stubs)");
    }

    // ==================== talk.mode ====================

    private CompletableFuture<Object> handleTalkMode(JsonNode params, GatewayConnection conn) {
        boolean enabled = params.path("enabled").asBoolean(false);
        String phase = params.path("phase").isTextual() ? params.get("phase").asText() : null;

        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("enabled", enabled);
        payload.put("phase", phase);
        payload.put("ts", System.currentTimeMillis());

        broadcaster.broadcastToAll("talk.mode", payload);
        return CompletableFuture.completedFuture(payload);
    }

    // ==================== voicewake.get / set ====================

    private CompletableFuture<Object> handleVoicewakeGet(JsonNode params, GatewayConnection conn) {
        try {
            var cfg = loadVoicewakeConfig();
            return CompletableFuture.completedFuture(Map.of("triggers", cfg.triggers));
        } catch (Exception e) {
            return fail("voicewake.get failed: " + e.getMessage());
        }
    }

    private CompletableFuture<Object> handleVoicewakeSet(JsonNode params, GatewayConnection conn) {
        JsonNode triggersNode = params.path("triggers");
        if (!triggersNode.isArray()) {
            return fail("voicewake.set requires triggers: string[]");
        }

        List<String> triggers = new ArrayList<>();
        for (JsonNode item : triggersNode) {
            String t = item.asText("").trim();
            if (!t.isEmpty())
                triggers.add(t);
        }
        if (triggers.isEmpty())
            triggers = new ArrayList<>(DEFAULT_TRIGGERS);

        try {
            var cfg = saveVoicewakeConfig(triggers);
            broadcaster.broadcastToAll("voicewake.changed", Map.of("triggers", cfg.triggers));
            return CompletableFuture.completedFuture(Map.of("triggers", cfg.triggers));
        } catch (Exception e) {
            return fail("voicewake.set failed: " + e.getMessage());
        }
    }

    // ==================== skills.install / update ====================

    private CompletableFuture<Object> handleSkillsInstall(JsonNode params, GatewayConnection conn) {
        String name = params.path("name").asText("");
        String installId = params.path("installId").asText("");
        if (name.isBlank() || installId.isBlank()) {
            return fail("name and installId required");
        }
        // Simplified: skill install is a no-op in Java gateway.
        // The full implementation requires the skills-install infrastructure.
        return CompletableFuture.completedFuture(Map.of(
                "ok", false,
                "message", "skill install not yet supported in Java gateway",
                "name", name,
                "installId", installId));
    }

    private CompletableFuture<Object> handleSkillsUpdate(JsonNode params, GatewayConnection conn) {
        String skillKey = params.path("skillKey").asText("");
        if (skillKey.isBlank()) {
            return fail("skillKey required");
        }
        // Simplified: skill update writes to config.
        // The full implementation requires config read/write for skills entries.
        return CompletableFuture.completedFuture(Map.of(
                "ok", false,
                "message", "skill update not yet supported in Java gateway",
                "skillKey", skillKey));
    }

    // ==================== Stub ====================

    private CompletableFuture<Object> stubUnavailable(JsonNode params, GatewayConnection conn) {
        return CompletableFuture.failedFuture(
                new UnsupportedOperationException("not implemented in Java gateway"));
    }

    // ==================== Voicewake persistence ====================

    private Path voicewakePath() {
        return stateDir.resolve("settings").resolve("voicewake.json");
    }

    private VoicewakeConfig loadVoicewakeConfig() throws IOException {
        Path p = voicewakePath();
        if (!Files.exists(p)) {
            return new VoicewakeConfig(new ArrayList<>(DEFAULT_TRIGGERS), 0L);
        }
        try {
            var node = objectMapper.readTree(Files.readString(p));
            List<String> triggers = new ArrayList<>();
            if (node.has("triggers") && node.get("triggers").isArray()) {
                for (JsonNode t : node.get("triggers")) {
                    String s = t.asText("").trim();
                    if (!s.isEmpty())
                        triggers.add(s);
                }
            }
            if (triggers.isEmpty())
                triggers = new ArrayList<>(DEFAULT_TRIGGERS);
            long updatedAtMs = node.path("updatedAtMs").asLong(0);
            return new VoicewakeConfig(triggers, updatedAtMs);
        } catch (Exception e) {
            return new VoicewakeConfig(new ArrayList<>(DEFAULT_TRIGGERS), 0L);
        }
    }

    private VoicewakeConfig saveVoicewakeConfig(List<String> triggers) throws IOException {
        voiceLock.lock();
        try {
            Path p = voicewakePath();
            Files.createDirectories(p.getParent());
            long now = System.currentTimeMillis();
            var cfg = new VoicewakeConfig(triggers, now);
            Files.writeString(p, objectMapper.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(Map.of(
                            "triggers", cfg.triggers,
                            "updatedAtMs", cfg.updatedAtMs)));
            return cfg;
        } finally {
            voiceLock.unlock();
        }
    }

    private CompletableFuture<Object> fail(String message) {
        return CompletableFuture.failedFuture(new IllegalArgumentException(message));
    }

    private record VoicewakeConfig(List<String> triggers, long updatedAtMs) {
    }
}

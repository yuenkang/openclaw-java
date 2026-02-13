package com.openclaw.gateway.session;

import com.openclaw.common.config.ConfigService;
import com.openclaw.gateway.session.SessionUtils.AcpSessionEntry;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Validates and applies session patch parameters to the session store.
 * <p>
 * Corresponds to TypeScript's {@code sessions-patch.ts} (341 lines).
 */
@Slf4j
public class SessionPatchService {

    private static final Pattern MODEL_REF_RE = Pattern.compile("^[\\w./-]+$");

    private final ConfigService configService;

    public SessionPatchService(ConfigService configService) {
        this.configService = configService;
    }

    /**
     * Apply a session patch to the given store.
     *
     * @param sessionKey  the session key being patched
     * @param existing    existing entry (may be null for new sessions)
     * @param patchParams the patch parameters
     * @return result with either the updated entry or an error
     */
    public PatchResult applyPatch(String sessionKey, AcpSessionEntry existing,
            Map<String, Object> patchParams) {
        // --- Validate and extract fields ---
        String displayName = extractString(patchParams, "displayName");
        String label = extractString(patchParams, "label");
        String subject = extractString(patchParams, "subject");
        String model = extractString(patchParams, "model");
        String modelProvider = extractString(patchParams, "modelProvider");
        String thinkingLevel = extractString(patchParams, "thinkingLevel");
        String sendPolicy = extractString(patchParams, "sendPolicy");
        String channel = extractString(patchParams, "channel");
        Integer contextTokens = extractInt(patchParams, "contextTokens");

        // Validate model reference (if provided)
        if (model != null && !model.isEmpty() && !MODEL_REF_RE.matcher(model).matches()) {
            return PatchResult.error("INVALID_REQUEST", "Invalid model: " + model);
        }

        // Validate thinkingLevel
        if (thinkingLevel != null) {
            var valid = Set.of("off", "minimal", "low", "medium", "high", "xhigh");
            if (!valid.contains(thinkingLevel)) {
                return PatchResult.error("INVALID_REQUEST",
                        "Invalid thinkingLevel: " + thinkingLevel);
            }
        }

        // Validate sendPolicy
        if (sendPolicy != null) {
            var valid = Set.of("send-on-tool-result", "send-on-enter", "off");
            if (!valid.contains(sendPolicy)) {
                return PatchResult.error("INVALID_REQUEST",
                        "Invalid sendPolicy: " + sendPolicy);
            }
        }

        // Validate contextTokens
        if (contextTokens != null && contextTokens <= 0) {
            return PatchResult.error("INVALID_REQUEST",
                    "contextTokens must be positive");
        }

        // --- Build updated entry ---
        Map<String, Object> updated = new LinkedHashMap<>();
        if (existing != null) {
            // Carry over existing fields
            addIfNonNull(updated, "sessionId", existing.sessionId());
            addIfNonNull(updated, "displayName", existing.displayName());
            addIfNonNull(updated, "subject", existing.subject());
            addIfNonNull(updated, "label", existing.label());
            addIfNonNull(updated, "channel", existing.channel());
            addIfNonNull(updated, "chatType", existing.chatType());
            addIfNonNull(updated, "thinkingLevel", existing.thinkingLevel());
            addIfNonNull(updated, "sendPolicy", existing.sendPolicy());
            addIfNonNull(updated, "modelOverride", existing.modelOverride());
            addIfNonNull(updated, "providerOverride", existing.providerOverride());
            addIfNonNull(updated, "contextTokens", existing.contextTokens());
        }

        // Apply patch overrides
        if (displayName != null)
            updated.put("displayName", displayName);
        if (label != null)
            updated.put("label", label);
        if (subject != null)
            updated.put("subject", subject);
        if (model != null)
            updated.put("modelOverride", model);
        if (modelProvider != null)
            updated.put("providerOverride", modelProvider);
        if (thinkingLevel != null)
            updated.put("thinkingLevel", thinkingLevel);
        if (sendPolicy != null)
            updated.put("sendPolicy", sendPolicy);
        if (channel != null)
            updated.put("channel", channel);
        if (contextTokens != null)
            updated.put("contextTokens", contextTokens);

        updated.put("updatedAt", System.currentTimeMillis());

        return PatchResult.ok(updated);
    }

    private static String extractString(Map<String, Object> params, String key) {
        Object v = params.get(key);
        if (v instanceof String s && !s.isBlank()) {
            return s.trim();
        }
        return null;
    }

    private static Integer extractInt(Map<String, Object> params, String key) {
        Object v = params.get(key);
        if (v instanceof Number n) {
            return n.intValue();
        }
        return null;
    }

    private static void addIfNonNull(Map<String, Object> map, String key, Object value) {
        if (value != null) {
            map.put(key, value);
        }
    }

    // ─── Result types ───────────────────────────────────────────────────

    public sealed interface PatchResult permits PatchResult.Ok, PatchResult.Err {
        static PatchResult ok(Map<String, Object> entry) {
            return new Ok(entry);
        }

        static PatchResult error(String code, String message) {
            return new Err(code, message);
        }

        record Ok(Map<String, Object> entry) implements PatchResult {
        }

        record Err(String code, String message) implements PatchResult {
        }
    }
}

package com.openclaw.gateway.chat;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Immutable payload for an agent event emitted during a chat run.
 * <p>
 * Mirrors the shape of the TypeScript agent-event payload in server-chat.ts.
 */
public record AgentEventPayload(
        String runId,
        String stream,
        int seq,
        Map<String, Object> data,
        String sessionKey) {

    /**
     * Create a payload without a sessionKey.
     */
    public static AgentEventPayload of(String runId, String stream, int seq, Map<String, Object> data) {
        return new AgentEventPayload(runId, stream, seq, data, null);
    }

    /**
     * Return a copy with the given sessionKey set.
     */
    public AgentEventPayload withSessionKey(String sessionKey) {
        return new AgentEventPayload(runId, stream, seq, data, sessionKey);
    }

    /**
     * Convenience: get a string value from the data map.
     */
    public String dataString(String key) {
        if (data == null)
            return null;
        Object v = data.get(key);
        return v instanceof String s ? s : null;
    }

    /**
     * Convert to a flat map suitable for JSON serialization / broadcasting.
     */
    public Map<String, Object> toMap() {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("runId", runId);
        map.put("stream", stream);
        map.put("seq", seq);
        if (data != null) {
            map.put("data", data);
        }
        if (sessionKey != null) {
            map.put("sessionKey", sessionKey);
        }
        return map;
    }
}

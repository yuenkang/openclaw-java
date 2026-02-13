package com.openclaw.gateway.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.chat.GatewayConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Gateway WebSocket event broadcaster with scope guards and slow consumer
 * handling.
 * Corresponds to TypeScript's server-broadcast.ts.
 */
public class GatewayBroadcaster {

    private static final Logger log = LoggerFactory.getLogger(GatewayBroadcaster.class);

    // =========================================================================
    // Scope guards
    // =========================================================================

    private static final String ADMIN_SCOPE = "operator.admin";
    private static final String APPROVALS_SCOPE = "operator.approvals";
    private static final String PAIRING_SCOPE = "operator.pairing";

    private static final Map<String, List<String>> EVENT_SCOPE_GUARDS = Map.of(
            "exec.approval.requested", List.of(APPROVALS_SCOPE),
            "exec.approval.resolved", List.of(APPROVALS_SCOPE),
            "device.pair.requested", List.of(PAIRING_SCOPE),
            "device.pair.resolved", List.of(PAIRING_SCOPE),
            "node.pair.requested", List.of(PAIRING_SCOPE),
            "node.pair.resolved", List.of(PAIRING_SCOPE));

    // =========================================================================
    // Types
    // =========================================================================

    /**
     * Represents a connected WebSocket client with auth context.
     */
    public record ConnectedClient(
            String connId,
            WebSocketSession session,
            String role,
            List<String> scopes) {
    }

    public record BroadcastOptions(
            boolean dropIfSlow,
            Integer presenceVersion,
            Integer healthVersion) {
        public static final BroadcastOptions DEFAULT = new BroadcastOptions(false, null, null);
    }

    // =========================================================================
    // Instance state
    // =========================================================================

    private final Set<ConnectedClient> clients = ConcurrentHashMap.newKeySet();
    private final ObjectMapper mapper;
    private final AtomicLong seq = new AtomicLong(0);

    public GatewayBroadcaster(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    public void addClient(ConnectedClient client) {
        clients.add(client);
    }

    public void removeClient(ConnectedClient client) {
        clients.remove(client);
    }

    public Set<ConnectedClient> getClients() {
        return clients;
    }

    // =========================================================================
    // Broadcast
    // =========================================================================

    /**
     * Broadcast an event to all connected clients.
     */
    public void broadcast(String event, Object payload) {
        broadcast(event, payload, BroadcastOptions.DEFAULT);
    }

    public void broadcast(String event, Object payload, BroadcastOptions opts) {
        broadcastInternal(event, payload, opts, null);
    }

    /**
     * Broadcast an event to specific connection IDs.
     */
    public void broadcastToConnIds(String event, Object payload, Set<String> connIds) {
        broadcastToConnIds(event, payload, connIds, BroadcastOptions.DEFAULT);
    }

    public void broadcastToConnIds(String event, Object payload, Set<String> connIds, BroadcastOptions opts) {
        if (connIds == null || connIds.isEmpty())
            return;
        broadcastInternal(event, payload, opts, connIds);
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private void broadcastInternal(String event, Object payload, BroadcastOptions opts, Set<String> targetConnIds) {
        boolean isTargeted = targetConnIds != null;
        Long eventSeq = isTargeted ? null : seq.incrementAndGet();

        Map<String, Object> stateVersion = null;
        if (opts.presenceVersion() != null || opts.healthVersion() != null) {
            stateVersion = new java.util.LinkedHashMap<>();
            if (opts.presenceVersion() != null)
                stateVersion.put("presence", opts.presenceVersion());
            if (opts.healthVersion() != null)
                stateVersion.put("health", opts.healthVersion());
        }

        Map<String, Object> frame = new java.util.LinkedHashMap<>();
        frame.put("type", "event");
        frame.put("event", event);
        frame.put("payload", payload);
        if (eventSeq != null)
            frame.put("seq", eventSeq);
        if (stateVersion != null)
            frame.put("stateVersion", stateVersion);

        String frameJson;
        try {
            frameJson = mapper.writeValueAsString(frame);
        } catch (JsonProcessingException e) {
            log.warn("failed to serialize broadcast frame for event={}: {}", event, e.getMessage());
            return;
        }

        TextMessage message = new TextMessage(frameJson);

        for (ConnectedClient c : clients) {
            if (isTargeted && !targetConnIds.contains(c.connId()))
                continue;
            if (!hasEventScope(c, event))
                continue;

            boolean slow = c.session().isOpen()
                    && c.session().getBufferSize() > GatewayConstants.MAX_BUFFERED_BYTES;
            if (slow && opts.dropIfSlow())
                continue;
            if (slow) {
                try {
                    c.session().close();
                } catch (IOException ignored) {
                }
                continue;
            }

            try {
                if (c.session().isOpen()) {
                    c.session().sendMessage(message);
                }
            } catch (IOException ignored) {
            }
        }
    }

    static boolean hasEventScope(ConnectedClient client, String event) {
        List<String> required = EVENT_SCOPE_GUARDS.get(event);
        if (required == null)
            return true;

        String role = client.role() != null ? client.role() : "operator";
        if (!"operator".equals(role))
            return false;

        List<String> scopes = client.scopes() != null ? client.scopes() : List.of();
        if (scopes.contains(ADMIN_SCOPE))
            return true;
        return required.stream().anyMatch(scopes::contains);
    }
}

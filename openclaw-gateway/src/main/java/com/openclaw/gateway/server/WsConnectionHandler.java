package com.openclaw.gateway.server;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Gateway WebSocket connection lifecycle handler.
 * <p>
 * Mirrors {@code server/ws-connection.ts :: attachGatewayWsConnectionHandler}.
 * In TypeScript this directly operates on {@code ws.WebSocketServer};
 * in Java it provides the lifecycle hooks that the WebSocket framework
 * adapter should invoke.
 */
public class WsConnectionHandler {

    private final Set<GatewayWsClient> clients = ConcurrentHashMap.newKeySet();
    private final BroadcastFunction broadcast;

    /**
     * @param broadcast function to broadcast events to all connected clients
     */
    public WsConnectionHandler(BroadcastFunction broadcast) {
        this.broadcast = broadcast;
    }

    public Set<GatewayWsClient> getClients() {
        return clients;
    }

    /**
     * Register a newly connected client.
     */
    public void addClient(GatewayWsClient client) {
        clients.add(client);
    }

    /**
     * Remove a disconnected client and handle presence update.
     */
    public void removeClient(GatewayWsClient client) {
        clients.remove(client);
    }

    /**
     * Broadcast helper matching the TS signature:
     * {@code broadcast(event, payload, opts?)}.
     */
    @FunctionalInterface
    public interface BroadcastFunction {
        void broadcast(String event, Object payload, BroadcastOptions opts);
    }

    /**
     * Options for a broadcast call (mirrors the TS opts parameter).
     */
    public record BroadcastOptions(
            boolean dropIfSlow,
            Integer presenceVersion,
            Integer healthVersion) {

        public static final BroadcastOptions DROP_IF_SLOW = new BroadcastOptions(true, null, null);
    }
}

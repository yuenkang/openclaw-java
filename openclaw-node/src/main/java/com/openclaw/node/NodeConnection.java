package com.openclaw.node;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Abstraction over the underlying connection to a node.
 * <p>
 * Implemented by {@code GatewayConnection} in the gateway module.
 * This interface allows the node module to remain independent of
 * gateway/websocket internals.
 * </p>
 */
public interface NodeConnection {

    /** Unique connection identifier. */
    String getConnectionId();

    /** Client ID extracted from the connect handshake (may be null). */
    String getClientId();

    /** IP address of the connected node client. */
    String getClientIp();

    /**
     * Send a named event with a JSON payload to this connection.
     *
     * @param event   event name (e.g. "node.invoke.request")
     * @param payload event payload (will be serialised)
     * @param mapper  ObjectMapper for serialisation
     */
    void sendEvent(String event, Object payload, ObjectMapper mapper) throws Exception;
}

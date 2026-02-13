package com.openclaw.gateway.server;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import com.openclaw.gateway.protocol.ProtocolTypes;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * WebSocket client connection descriptor.
 * Mirrors {@code server/ws-types.ts :: GatewayWsClient}.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class GatewayWsClient {

    /** WebSocket session — opaque handle in Java. */
    private Object socket;

    /** The connect params received during handshake. */
    private ProtocolTypes.ConnectParams connect;

    /** Unique connection identifier (UUID). */
    private String connId;

    /** Presence key for this client (if registered). */
    private String presenceKey;

    /** Resolved client IP address (if remote). */
    private String clientIp;
}

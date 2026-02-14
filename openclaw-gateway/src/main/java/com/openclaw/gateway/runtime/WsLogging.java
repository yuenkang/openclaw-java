package com.openclaw.gateway.runtime;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.WebSocketSession;

import java.net.InetSocketAddress;

/**
 * WebSocket connection logging utilities.
 * Corresponds to TS {@code gateway/ws-logging.ts}.
 */
@Slf4j
public class WsLogging {

    /**
     * Log a new WebSocket connection.
     */
    public static void logConnect(String connId, WebSocketSession session, String authMethod) {
        String remoteAddr = resolveRemoteAddr(session);
        log.info("ws connect: conn={} remote={} auth={}",
                connId, remoteAddr, authMethod != null ? authMethod : "pending");
    }

    /**
     * Log a WebSocket disconnection.
     */
    public static void logDisconnect(String connId, WebSocketSession session,
            int closeCode, String closeReason) {
        String remoteAddr = resolveRemoteAddr(session);
        log.info("ws disconnect: conn={} remote={} code={} reason={}",
                connId, remoteAddr, closeCode,
                closeReason != null ? closeReason : "normal");
    }

    /**
     * Log a WebSocket error.
     */
    public static void logError(String connId, String error) {
        log.warn("ws error: conn={} error={}", connId, error);
    }

    /**
     * Log an inbound RPC method call.
     */
    public static void logMethodCall(String connId, String method, String requestId) {
        log.debug("ws rpc: conn={} method={} id={}", connId, method, requestId);
    }

    /**
     * Resolve remote address from the session.
     */
    private static String resolveRemoteAddr(WebSocketSession session) {
        InetSocketAddress remoteAddr = session.getRemoteAddress();
        if (remoteAddr != null) {
            return remoteAddr.getAddress().getHostAddress() + ":" + remoteAddr.getPort();
        }
        return "unknown";
    }
}

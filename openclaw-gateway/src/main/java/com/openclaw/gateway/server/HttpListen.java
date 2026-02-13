package com.openclaw.gateway.server;

import java.io.IOException;
import java.net.BindException;

/**
 * HTTP / WebSocket server binding utility.
 * <p>
 * Mirrors {@code server/http-listen.ts}.
 * In Java the actual server binding depends on the chosen framework
 * (Jetty, Undertow, Netty, etc.). This class provides the error-wrapping
 * logic that was in TypeScript.
 */
public final class HttpListen {

    private HttpListen() {
    }

    /**
     * Exception thrown when the gateway port is already in use.
     */
    public static class GatewayLockError extends RuntimeException {
        public GatewayLockError(String message) {
            super(message);
        }

        public GatewayLockError(String message, Throwable cause) {
            super(message, cause);
        }
    }

    /**
     * Wrap a server-start attempt and throw {@link GatewayLockError}
     * with a helpful message if the port is occupied.
     *
     * @param bindHost the bind host address
     * @param port     the port number
     * @param starter  callback that actually starts the server
     */
    public static void listenOrThrow(String bindHost, int port, ServerStarter starter) {
        try {
            starter.start();
        } catch (BindException e) {
            throw new GatewayLockError(
                    "another gateway instance is already listening on ws://" + bindHost + ":" + port,
                    e);
        } catch (IOException e) {
            throw new GatewayLockError(
                    "failed to bind gateway socket on ws://" + bindHost + ":" + port + ": " + e.getMessage(),
                    e);
        }
    }

    /**
     * Functional interface for starting the underlying server.
     */
    @FunctionalInterface
    public interface ServerStarter {
        void start() throws IOException;
    }
}

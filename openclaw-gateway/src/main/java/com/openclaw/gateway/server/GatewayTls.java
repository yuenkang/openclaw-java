package com.openclaw.gateway.server;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Gateway TLS configuration runtime.
 * <p>
 * Mirrors {@code server/tls.ts} and the underlying
 * {@code infra/tls/gateway.ts :: GatewayTlsRuntime}.
 * The actual TLS context creation depends on the Java HTTPS server used.
 */
public final class GatewayTls {

    private GatewayTls() {
    }

    /**
     * Resolved TLS configuration for the gateway server.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class TlsRuntime {
        private boolean enabled;
        private String certPath;
        private String keyPath;
    }
}

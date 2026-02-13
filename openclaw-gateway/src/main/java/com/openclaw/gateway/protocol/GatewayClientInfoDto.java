package com.openclaw.gateway.protocol;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Gateway client info DTO — the {@code client} field within ConnectParams.
 * Mirrors the {@code GatewayClientInfo} type from
 * {@code protocol/client-info.ts}.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class GatewayClientInfoDto {

    /** Known client identifier, e.g. "cli", "openclaw-ios". */
    private String id;

    /** Human-readable display name. */
    private String displayName;

    /** Client version string. */
    private String version;

    /** Platform string, e.g. "macos-arm64". */
    private String platform;

    /** Device family, e.g. "iPhone". */
    private String deviceFamily;

    /** Model identifier, e.g. "iPhone15,3". */
    private String modelIdentifier;

    /** Client mode: "cli", "ui", "webchat", "backend", "node", "probe", "test". */
    private String mode;

    /** Instance identifier for distinguishing multiple connections. */
    private String instanceId;
}

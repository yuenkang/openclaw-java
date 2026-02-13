package com.openclaw.gateway.server;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.gateway.protocol.ClientInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Gateway WebSocket message handler — processes incoming frames after
 * the connection is established.
 * <p>
 * Mirrors {@code server/ws-connection/message-handler.ts}.
 * The TypeScript version is a 1000+ line monolith handling:
 * <ol>
 * <li>Handshake (connect challenge / protocol negotiation)</li>
 * <li>Device authentication &amp; pairing</li>
 * <li>Request frame dispatch to server-methods</li>
 * </ol>
 * <p>
 * In Java the handshake + auth logic is factored into separate helpers;
 * this class coordinates them and delegates request dispatch to the
 * method handler registry.
 */
public class WsMessageHandler {

    private static final Logger log = LoggerFactory.getLogger(WsMessageHandler.class);

    /** Maximum signature clock skew for device auth (10 minutes). */
    public static final long DEVICE_SIGNATURE_SKEW_MS = 10 * 60 * 1000L;

    private final ObjectMapper objectMapper;

    public WsMessageHandler(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    // ── Auth-failure message formatting (from message-handler.ts) ──

    /**
     * Authentication mode indicator.
     */
    public enum AuthProvided {
        TOKEN, PASSWORD, NONE
    }

    /**
     * Format a human-readable auth failure message depending on
     * the auth mode and the client type.
     */
    public static String formatAuthFailureMessage(
            String authMode,
            AuthProvided authProvided,
            String reason,
            String clientId) {

        boolean isCli = ClientInfo.CLI.equals(clientId)
                || ClientInfo.GATEWAY_CLIENT.equals(clientId);
        boolean isControlUi = ClientInfo.CONTROL_UI.equals(clientId);
        boolean isWebchat = ClientInfo.WEBCHAT.equals(clientId)
                || ClientInfo.WEBCHAT_UI.equals(clientId);

        String uiHint = "open the dashboard URL and paste the token in Control UI settings";
        String tokenHint = isCli
                ? "set gateway.remote.token to match gateway.auth.token"
                : (isControlUi || isWebchat) ? uiHint : "provide gateway auth token";
        String passwordHint = isCli
                ? "set gateway.remote.password to match gateway.auth.password"
                : (isControlUi || isWebchat) ? "enter the password in Control UI settings"
                        : "provide gateway auth password";

        if (reason != null) {
            return switch (reason) {
                case "token_missing" -> "unauthorized: gateway token missing (" + tokenHint + ")";
                case "token_mismatch" -> "unauthorized: gateway token mismatch (" + tokenHint + ")";
                case "token_missing_config" ->
                    "unauthorized: gateway token not configured on gateway (set gateway.auth.token)";
                case "password_missing" ->
                    "unauthorized: gateway password missing (" + passwordHint + ")";
                case "password_mismatch" ->
                    "unauthorized: gateway password mismatch (" + passwordHint + ")";
                case "password_missing_config" ->
                    "unauthorized: gateway password not configured on gateway (set gateway.auth.password)";
                case "tailscale_user_missing" ->
                    "unauthorized: tailscale identity missing (use Tailscale Serve auth or gateway token/password)";
                case "tailscale_proxy_missing" ->
                    "unauthorized: tailscale proxy headers missing (use Tailscale Serve or gateway token/password)";
                case "tailscale_whois_failed" ->
                    "unauthorized: tailscale identity check failed (use Tailscale Serve auth or gateway token/password)";
                case "tailscale_user_mismatch" ->
                    "unauthorized: tailscale identity mismatch (use Tailscale Serve auth or gateway token/password)";
                default -> "unauthorized";
            };
        }

        if ("token".equals(authMode) && authProvided == AuthProvided.NONE) {
            return "unauthorized: gateway token missing (" + tokenHint + ")";
        }
        if ("password".equals(authMode) && authProvided == AuthProvided.NONE) {
            return "unauthorized: gateway password missing (" + passwordHint + ")";
        }
        return "unauthorized";
    }

    /**
     * Resolve the hostname from a Host header value,
     * stripping port and handling IPv6 brackets.
     */
    public static String resolveHostName(String hostHeader) {
        if (hostHeader == null || hostHeader.isBlank()) {
            return "";
        }
        String host = hostHeader.strip().toLowerCase();
        if (host.startsWith("[")) {
            int end = host.indexOf(']');
            if (end != -1) {
                return host.substring(1, end);
            }
        }
        int colonPos = host.indexOf(':');
        return colonPos >= 0 ? host.substring(0, colonPos) : host;
    }
}

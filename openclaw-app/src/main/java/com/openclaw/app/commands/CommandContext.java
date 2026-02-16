package com.openclaw.app.commands;

import com.openclaw.common.config.ConfigService;
import com.openclaw.common.config.OpenClawConfig;
import jakarta.annotation.Nullable;

/**
 * Context passed to every command handler.
 * Encapsulates session, sender, config, authorization state, and config
 * persistence.
 * <p>
 * Mirrors TypeScript's command handler params (senderId, isAuthorizedSender,
 * cfg, ctx).
 * </p>
 */
public record CommandContext(
                String sessionKey,
                String senderId,
                OpenClawConfig config,
                boolean isAuthorizedSender,
                @Nullable ConfigService configService) {
}

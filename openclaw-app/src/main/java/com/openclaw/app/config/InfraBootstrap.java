package com.openclaw.app.config;

import com.openclaw.common.config.ConfigPaths;
import com.openclaw.common.config.ConfigService;
import com.openclaw.common.infra.DotEnv;
import com.openclaw.common.infra.PathEnv;
import com.openclaw.common.infra.RestartSentinel;
import com.openclaw.common.infra.UnhandledExceptions;
import com.openclaw.common.infra.UpdateChannels;
import com.openclaw.common.infra.UpdateStartup;
import com.openclaw.common.infra.Warnings;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

/**
 * Infrastructure bootstrap — integrates infra utilities into the Spring
 * application lifecycle.
 * <p>
 * Mirrors the startup sequence from TypeScript's {@code index.ts}:
 * {@code loadDotEnv → ensureCliOnPath → installUnhandledRejectionHandler}
 * and {@code server-startup.ts}: sentinel wake, update check, warning flush.
 */
@Slf4j
@Component
@Order(0) // Run before PluginBootstrap
public class InfraBootstrap {

    private final ConfigService configService;

    public InfraBootstrap(ConfigService configService) {
        this.configService = configService;
    }

    /**
     * Early initialization — runs before any other @PostConstruct beans
     * (due to @Order(0)).
     * <p>
     * Corresponds to the top-level synchronous calls in TS index.ts:
     * 
     * <pre>
     *   loadDotEnv({ quiet: true });
     *   ensureOpenClawCliOnPath();
     *   installUnhandledRejectionHandler();
     * </pre>
     */
    @PostConstruct
    public void init() {
        Path stateDir = ConfigPaths.resolveStateDir();

        // 1. Load .env files (CWD + global fallback)
        try {
            Map<String, String> env = new HashMap<>(System.getenv());
            DotEnv.loadDotEnv(stateDir, env, /* quiet */ true);
            // Apply loaded variables to system properties so they're accessible
            // throughout the application
            for (Map.Entry<String, String> entry : env.entrySet()) {
                if (System.getenv(entry.getKey()) == null) {
                    System.setProperty(entry.getKey(), entry.getValue());
                }
            }
            log.debug("DotEnv loaded ({} env vars from .env files)", env.size() - System.getenv().size());
        } catch (Exception e) {
            log.warn("Failed to load .env: {}", e.getMessage());
        }

        // 2. Ensure CLI is on PATH
        try {
            Map<String, String> pathEnv = new HashMap<>(System.getenv());
            PathEnv.ensureCliOnPath(pathEnv);
            String newPath = pathEnv.get("PATH");
            if (newPath != null) {
                System.setProperty("PATH", newPath);
            }
            log.debug("PathEnv bootstrap complete");
        } catch (Exception e) {
            log.warn("Failed to bootstrap PATH: {}", e.getMessage());
        }

        // 3. Install global uncaught exception handler
        UnhandledExceptions.install();
        log.debug("Global exception handler installed");

        log.info("Infra bootstrap complete (stateDir={})", stateDir);
    }

    /**
     * Late initialization — runs after all beans are wired and the application
     * is fully ready.
     * <p>
     * Corresponds to TS server-startup.ts post-ready tasks:
     * restart sentinel consumption, update check, warning flush.
     */
    @EventListener(ApplicationReadyEvent.class)
    public void onApplicationReady() {
        Path stateDir = ConfigPaths.resolveStateDir();

        // 1. Consume restart sentinel (if one exists from a previous restart)
        try {
            RestartSentinel.Payload sentinel = RestartSentinel.consumeSentinel(stateDir);
            if (sentinel != null) {
                log.info("Restart sentinel consumed: kind={}, status={}, message={}",
                        sentinel.kind(), sentinel.status(), sentinel.message());
            }
        } catch (Exception e) {
            log.debug("No restart sentinel to consume: {}", e.getMessage());
        }

        // 2. Async update check (throttled to once per day)
        try {
            String currentVersion = resolveCurrentVersion();
            String projectRoot = System.getProperty("user.dir");
            UpdateChannels.Channel configChannel = resolveConfigChannel();

            UpdateStartup.scheduleGatewayUpdateCheck(stateDir, currentVersion, projectRoot,
                    /* checkOnStart */ true, configChannel);
            log.debug("Update check scheduled");
        } catch (Exception e) {
            log.debug("Update check skipped: {}", e.getMessage());
        }

        // 3. Flush accumulated startup warnings
        Warnings.flushWarnings();

        log.info("Infra post-ready tasks complete");
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private String resolveCurrentVersion() {
        // Try to read version from config or package info
        Package pkg = getClass().getPackage();
        if (pkg != null && pkg.getImplementationVersion() != null) {
            return pkg.getImplementationVersion();
        }
        return "0.0.0-dev";
    }

    private UpdateChannels.Channel resolveConfigChannel() {
        try {
            var config = configService.loadConfig();
            // Check if config has an update channel setting
            if (config.getGateway() != null) {
                // Default to STABLE
                return UpdateChannels.Channel.STABLE;
            }
        } catch (Exception e) {
            // ignore
        }
        return UpdateChannels.Channel.STABLE;
    }
}

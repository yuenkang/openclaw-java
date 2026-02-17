package com.openclaw.agent.hooks;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * Bundled hook handlers that ship with OpenClaw.
 * Registers built-in hooks: boot-md, command-logger, session-memory, etc.
 * Corresponds to TypeScript's hooks/bundled/*.
 */
@Slf4j
public class BundledHookHandlers {

    private BundledHookHandlers() {
    }

    /**
     * Register all bundled hook handlers with the given registry.
     */
    public static void registerAll(InternalHookRegistry registry) {
        registerBootMdHook(registry);
        registerSoulEvilHook(registry);
        registerCommandLoggerHook(registry);
        registerSessionMemoryHook(registry);
        log.info("Registered {} bundled hook handlers", 4);
    }

    // =========================================================================
    // boot-md: Prepend HOOK.md content to agent system prompt on bootstrap
    // =========================================================================

    private static void registerBootMdHook(InternalHookRegistry registry) {
        registry.register("agent:bootstrap", event -> {
            String workspaceDir = (String) event.getContext().get("workspaceDir");
            if (workspaceDir == null)
                return;

            // Boot-md hook: reads HOOK.md instructions and appends to agent context
            log.debug("[boot-md] Agent bootstrap in workspace: {}", workspaceDir);
            // Actual HOOK.md reading is done by the agent prompt builder

            // Validate hooks directory if configured
            String hooksDir = (String) event.getContext().get("hooksDir");
            if (hooksDir != null && !hooksDir.isBlank()) {
                List<String> errors = HookInstall.validateHookDir(hooksDir);
                if (!errors.isEmpty()) {
                    log.warn("[boot-md] Hook directory validation errors: {}", errors);
                } else {
                    log.debug("[boot-md] Hook directory validated: {}", hooksDir);
                }
            }
        });
    }

    // =========================================================================
    // soul-evil: Override SOUL.md with SOUL_EVIL.md based on probability/schedule
    // =========================================================================

    @SuppressWarnings("unchecked")
    private static void registerSoulEvilHook(InternalHookRegistry registry) {
        registry.register("agent:bootstrap", event -> {
            Map<String, Object> ctx = event.getContext();
            if (ctx == null)
                return;

            String workspaceDir = (String) ctx.get("workspaceDir");
            Object configObj = ctx.get("config");
            Object filesObj = ctx.get("bootstrapFiles");
            if (workspaceDir == null || filesObj == null)
                return;

            // Skip subagent sessions
            String sessionKey = event.getSessionKey();
            if (sessionKey != null && sessionKey.contains(":sub:"))
                return;

            // Resolve soul-evil config from OpenClawConfig
            Map<String, Object> hookConfig = null;
            if (configObj instanceof OpenClawConfig cfg) {
                hookConfig = resolveSoulEvilHookConfig(cfg);
            }
            if (hookConfig == null)
                return;

            Object enabledVal = hookConfig.get("enabled");
            if (enabledVal != null && Boolean.FALSE.equals(enabledVal))
                return;

            SoulEvil.SoulEvilConfig soulConfig = SoulEvil.resolveSoulEvilConfig(hookConfig);
            if (soulConfig == null)
                return;

            List<SoulEvil.BootstrapFile> files = (List<SoulEvil.BootstrapFile>) filesObj;

            // Resolve user timezone from config
            String userTimezone = null;
            if (configObj instanceof OpenClawConfig cfg
                    && cfg.getAgents() != null
                    && cfg.getAgents().getDefaults() != null) {
                userTimezone = cfg.getAgents().getDefaults().getUserTimezone();
            }

            List<SoulEvil.BootstrapFile> updated = SoulEvil.applySoulEvilOverride(
                    files, Path.of(workspaceDir), soulConfig, userTimezone,
                    Instant.now(), Math::random);

            // Replace bootstrapFiles in context with updated list
            ctx.put("bootstrapFiles", updated);
        });
    }

    /**
     * Extract hooks.soul-evil config from OpenClawConfig.
     */
    @SuppressWarnings("unchecked")
    private static Map<String, Object> resolveSoulEvilHookConfig(OpenClawConfig config) {
        if (config.getHooks() == null)
            return null;
        Map<String, Object> entries = config.getHooks().getHookEntries();
        if (entries == null)
            return null;
        Object soulEvil = entries.get("soul-evil");
        if (soulEvil instanceof Map<?, ?> m) {
            return (Map<String, Object>) m;
        }
        return null;
    }

    // =========================================================================
    // command-logger: Log all agent commands
    // =========================================================================

    private static void registerCommandLoggerHook(InternalHookRegistry registry) {
        registry.register("command", event -> {
            String action = event.getAction();
            Map<String, Object> context = event.getContext();
            String sessionKey = event.getSessionKey();

            log.debug("[command-logger] session={} action={} context={}",
                    sessionKey, action, context);
        });
    }

    // =========================================================================
    // session-memory: Persist key facts from session to memory store
    // =========================================================================

    private static void registerSessionMemoryHook(InternalHookRegistry registry) {
        registry.register("session:end", event -> {
            String sessionKey = event.getSessionKey();

            log.debug("[session-memory] Session ended: session={}", sessionKey);
            // Memory persistence is handled by the session store
        });
    }
}

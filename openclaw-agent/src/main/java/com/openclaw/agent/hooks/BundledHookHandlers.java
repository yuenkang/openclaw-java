package com.openclaw.agent.hooks;

import lombok.extern.slf4j.Slf4j;

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
        registerCommandLoggerHook(registry);
        registerSessionMemoryHook(registry);
        log.info("Registered {} bundled hook handlers", 3);
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
        });
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
            Map<String, Object> context = event.getContext();

            log.debug("[session-memory] Session ended: session={}", sessionKey);
            // Memory persistence is handled by the session store
        });
    }
}

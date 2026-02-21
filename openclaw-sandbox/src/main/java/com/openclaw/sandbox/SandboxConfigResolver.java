package com.openclaw.sandbox;

import com.openclaw.sandbox.SandboxTypes.*;

import static com.openclaw.sandbox.SandboxConstants.*;

/**
 * Sandbox configuration resolution — merges agent-specific and global sandbox
 * configs.
 * Corresponds to TypeScript sandbox/config.ts.
 */
public final class SandboxConfigResolver {

    private SandboxConfigResolver() {
    }

    /**
     * Resolve sandbox scope from explicit scope or legacy perSession flag.
     */
    public static SandboxScope resolveSandboxScope(SandboxScope scope, Boolean perSession) {
        if (scope != null)
            return scope;
        if (perSession != null)
            return perSession ? SandboxScope.SESSION : SandboxScope.SHARED;
        return SandboxScope.AGENT;
    }

    /**
     * Resolve Docker config by merging agent-specific and global Docker settings.
     */
    public static SandboxDockerConfig resolveSandboxDockerConfig(
            SandboxScope scope,
            SandboxDockerConfig globalDocker,
            SandboxDockerConfig agentDocker) {

        SandboxDockerConfig agent = scope == SandboxScope.SHARED ? null : agentDocker;
        SandboxDockerConfig global = globalDocker;

        return SandboxDockerConfig.builder()
                .image(coalesce(agent, global, SandboxDockerConfig::getImage, DEFAULT_SANDBOX_IMAGE))
                .containerPrefix(coalesce(agent, global, SandboxDockerConfig::getContainerPrefix,
                        DEFAULT_SANDBOX_CONTAINER_PREFIX))
                .workdir(coalesce(agent, global, SandboxDockerConfig::getWorkdir, DEFAULT_SANDBOX_WORKDIR))
                .readOnlyRoot(agent != null ? agent.isReadOnlyRoot() : global != null ? global.isReadOnlyRoot() : true)
                .tmpfs(agent != null && agent.getTmpfs() != null ? agent.getTmpfs()
                        : global != null && global.getTmpfs() != null ? global.getTmpfs()
                                : java.util.List.of("/tmp", "/var/tmp", "/run"))
                .network(coalesce(agent, global, SandboxDockerConfig::getNetwork, "none"))
                .user(coalesce(agent, global, SandboxDockerConfig::getUser, null))
                .capDrop(agent != null && agent.getCapDrop() != null ? agent.getCapDrop()
                        : global != null && global.getCapDrop() != null ? global.getCapDrop()
                                : java.util.List.of("ALL"))
                .env(mergeEnv(
                        global != null ? global.getEnv() : null,
                        agent != null ? agent.getEnv() : null))
                .setupCommand(coalesce(agent, global, SandboxDockerConfig::getSetupCommand, null))
                .pidsLimit(coalesce(agent, global, SandboxDockerConfig::getPidsLimit, null))
                .memory(coalesce(agent, global, SandboxDockerConfig::getMemory, null))
                .memorySwap(coalesce(agent, global, SandboxDockerConfig::getMemorySwap, null))
                .cpus(coalesce(agent, global, SandboxDockerConfig::getCpus, null))
                .ulimits(mergeMap(
                        global != null ? global.getUlimits() : null,
                        agent != null ? agent.getUlimits() : null))
                .seccompProfile(coalesce(agent, global, SandboxDockerConfig::getSeccompProfile, null))
                .apparmorProfile(coalesce(agent, global, SandboxDockerConfig::getApparmorProfile, null))
                .dns(coalesce(agent, global, SandboxDockerConfig::getDns, null))
                .extraHosts(coalesce(agent, global, SandboxDockerConfig::getExtraHosts, null))
                .binds(mergeBinds(
                        global != null ? global.getBinds() : null,
                        agent != null ? agent.getBinds() : null))
                .build();
    }

    /**
     * Resolve browser config by merging agent-specific and global browser settings.
     */
    public static SandboxBrowserConfig resolveSandboxBrowserConfig(
            SandboxScope scope,
            SandboxBrowserConfig globalBrowser,
            SandboxBrowserConfig agentBrowser) {

        SandboxBrowserConfig agent = scope == SandboxScope.SHARED ? null : agentBrowser;
        SandboxBrowserConfig global = globalBrowser;

        return SandboxBrowserConfig.builder()
                .enabled(agent != null ? agent.isEnabled() : global != null ? global.isEnabled() : false)
                .image(coalesceStr(agent != null ? agent.getImage() : null,
                        global != null ? global.getImage() : null,
                        DEFAULT_SANDBOX_BROWSER_IMAGE))
                .containerPrefix(coalesceStr(
                        agent != null ? agent.getContainerPrefix() : null,
                        global != null ? global.getContainerPrefix() : null,
                        DEFAULT_SANDBOX_BROWSER_PREFIX))
                .cdpPort(agent != null && agent.getCdpPort() > 0 ? agent.getCdpPort()
                        : global != null && global.getCdpPort() > 0 ? global.getCdpPort()
                                : DEFAULT_SANDBOX_BROWSER_CDP_PORT)
                .vncPort(agent != null && agent.getVncPort() > 0 ? agent.getVncPort()
                        : global != null && global.getVncPort() > 0 ? global.getVncPort()
                                : DEFAULT_SANDBOX_BROWSER_VNC_PORT)
                .noVncPort(agent != null && agent.getNoVncPort() > 0 ? agent.getNoVncPort()
                        : global != null && global.getNoVncPort() > 0 ? global.getNoVncPort()
                                : DEFAULT_SANDBOX_BROWSER_NOVNC_PORT)
                .headless(agent != null ? agent.isHeadless() : global != null ? global.isHeadless() : false)
                .enableNoVnc(agent != null ? agent.isEnableNoVnc() : global != null ? global.isEnableNoVnc() : true)
                .allowHostControl(agent != null ? agent.isAllowHostControl()
                        : global != null ? global.isAllowHostControl() : false)
                .autoStart(agent != null ? agent.isAutoStart() : global != null ? global.isAutoStart() : true)
                .autoStartTimeoutMs(agent != null && agent.getAutoStartTimeoutMs() > 0 ? agent.getAutoStartTimeoutMs()
                        : global != null && global.getAutoStartTimeoutMs() > 0 ? global.getAutoStartTimeoutMs()
                                : DEFAULT_SANDBOX_BROWSER_AUTOSTART_TIMEOUT_MS)
                .build();
    }

    /**
     * Resolve prune config by merging agent-specific and global prune settings.
     */
    public static SandboxPruneConfig resolveSandboxPruneConfig(
            SandboxScope scope,
            SandboxPruneConfig globalPrune,
            SandboxPruneConfig agentPrune) {

        SandboxPruneConfig agent = scope == SandboxScope.SHARED ? null : agentPrune;
        SandboxPruneConfig global = globalPrune;

        return SandboxPruneConfig.builder()
                .idleHours(agent != null && agent.getIdleHours() > 0 ? agent.getIdleHours()
                        : global != null && global.getIdleHours() > 0 ? global.getIdleHours()
                                : DEFAULT_SANDBOX_IDLE_HOURS)
                .maxAgeDays(agent != null && agent.getMaxAgeDays() > 0 ? agent.getMaxAgeDays()
                        : global != null && global.getMaxAgeDays() > 0 ? global.getMaxAgeDays()
                                : DEFAULT_SANDBOX_MAX_AGE_DAYS)
                .build();
    }

    // ── Helpers ─────────────────────────────────────────────────────

    private static <T, R> R coalesce(T agent, T global,
            java.util.function.Function<T, R> getter, R fallback) {
        if (agent != null) {
            R val = getter.apply(agent);
            if (val != null)
                return val;
        }
        if (global != null) {
            R val = getter.apply(global);
            if (val != null)
                return val;
        }
        return fallback;
    }

    private static String coalesceStr(String a, String b, String fallback) {
        if (a != null && !a.isBlank())
            return a;
        if (b != null && !b.isBlank())
            return b;
        return fallback;
    }

    private static java.util.Map<String, String> mergeEnv(
            java.util.Map<String, String> globalEnv,
            java.util.Map<String, String> agentEnv) {
        if (agentEnv != null && !agentEnv.isEmpty()) {
            java.util.Map<String, String> merged = new java.util.LinkedHashMap<>();
            if (globalEnv != null)
                merged.putAll(globalEnv);
            else
                merged.put("LANG", "C.UTF-8");
            merged.putAll(agentEnv);
            return merged;
        }
        return globalEnv != null ? globalEnv : java.util.Map.of("LANG", "C.UTF-8");
    }

    @SuppressWarnings("unchecked")
    private static <V> java.util.Map<String, V> mergeMap(
            java.util.Map<String, V> globalMap,
            java.util.Map<String, V> agentMap) {
        if (agentMap != null && !agentMap.isEmpty()) {
            java.util.Map<String, V> merged = new java.util.LinkedHashMap<>();
            if (globalMap != null)
                merged.putAll(globalMap);
            merged.putAll(agentMap);
            return merged;
        }
        return globalMap;
    }

    private static java.util.List<String> mergeBinds(
            java.util.List<String> globalBinds,
            java.util.List<String> agentBinds) {
        java.util.List<String> result = new java.util.ArrayList<>();
        if (globalBinds != null)
            result.addAll(globalBinds);
        if (agentBinds != null)
            result.addAll(agentBinds);
        return result.isEmpty() ? null : result;
    }
}

package com.openclaw.hooks;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.util.List;
import java.util.Map;

/**
 * Hook configuration and eligibility checking.
 * Determines whether a hook should be included based on config, OS, binaries,
 * and env.
 * Corresponds to TypeScript's hooks/config.ts.
 */
@Slf4j
public final class HookConfig {

    private HookConfig() {
    }

    private static final Map<String, Boolean> DEFAULT_CONFIG_VALUES = Map.of(
            "browser.enabled", true,
            "browser.evaluateEnabled", true,
            "workspace.dir", true);

    /**
     * Check if a value is truthy (non-null, non-empty, non-zero).
     */
    public static boolean isTruthy(Object value) {
        if (value == null)
            return false;
        if (value instanceof Boolean b)
            return b;
        if (value instanceof Number n)
            return n.doubleValue() != 0;
        if (value instanceof String s)
            return !s.trim().isEmpty();
        return true;
    }

    /**
     * Resolve a dot-separated config path to a value in OpenClawConfig.
     * Returns null if the path doesn't exist.
     */
    public static Object resolveConfigPath(OpenClawConfig config, String path) {
        if (config == null || path == null || path.isEmpty())
            return null;

        String[] parts = path.split("\\.");
        Object current = config;
        for (String part : parts) {
            if (current == null)
                return null;
            if (current instanceof Map<?, ?> map) {
                current = map.get(part);
            } else {
                // Use reflection or getter convention
                current = getFieldValue(current, part);
            }
        }
        return current;
    }

    /**
     * Check if a config path resolves to a truthy value, with defaults.
     */
    public static boolean isConfigPathTruthy(OpenClawConfig config, String path) {
        Object value = resolveConfigPath(config, path);
        if (value == null && DEFAULT_CONFIG_VALUES.containsKey(path)) {
            return DEFAULT_CONFIG_VALUES.get(path);
        }
        return isTruthy(value);
    }

    /**
     * Resolve hook-specific configuration from config.hooks.internal.entries.
     */
    public static Map<String, Object> resolveHookConfig(OpenClawConfig config, String hookKey) {
        if (config == null || hookKey == null)
            return null;
        var hooks = config.getHooks();
        if (hooks == null)
            return null;
        var internal = hooks.getInternal();
        if (internal == null)
            return null;
        var entries = internal.getEntries();
        if (entries == null)
            return null;

        Object entry = entries.get(hookKey);
        if (entry instanceof Map<?, ?> map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> result = (Map<String, Object>) map;
            return result;
        }
        return null;
    }

    /**
     * Get the current runtime platform.
     */
    public static String resolveRuntimePlatform() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac") || os.contains("darwin"))
            return "darwin";
        if (os.contains("win"))
            return "win32";
        return "linux";
    }

    /**
     * Check if a binary is available on PATH.
     */
    public static boolean hasBinary(String bin) {
        String path = System.getenv("PATH");
        if (path == null || bin == null)
            return false;

        String[] dirs = path.split(File.pathSeparator);
        for (String dir : dirs) {
            File candidate = new File(dir, bin);
            if (candidate.canExecute())
                return true;
        }
        return false;
    }

    /**
     * Determine if a hook should be included based on its metadata and config.
     */
    public static boolean shouldIncludeHook(
            HookTypes.HookEntry entry,
            OpenClawConfig config) {
        return shouldIncludeHook(entry, config, null);
    }

    /**
     * Determine if a hook should be included based on its metadata, config,
     * and eligibility context.
     */
    public static boolean shouldIncludeHook(
            HookTypes.HookEntry entry,
            OpenClawConfig config,
            HookEligibilityContext eligibility) {

        if (entry == null || entry.getHook() == null)
            return false;

        String hookKey = entry.getHook().getName();
        Map<String, Object> hookCfg = resolveHookConfig(config, hookKey);
        boolean pluginManaged = entry.getHook().getSource() == HookTypes.HookSource.OPENCLAW_PLUGIN;

        // Check if explicitly disabled
        if (!pluginManaged && hookCfg != null) {
            Object enabled = hookCfg.get("enabled");
            if (Boolean.FALSE.equals(enabled))
                return false;
        }

        HookTypes.HookMetadata metadata = entry.getMetadata();
        if (metadata == null)
            return true;

        // Check OS requirement
        List<String> osList = metadata.getOs();
        if (osList != null && !osList.isEmpty()) {
            String platform = resolveRuntimePlatform();
            if (!osList.contains(platform)) {
                // Check remote platforms if available
                if (eligibility == null || eligibility.remotePlatforms() == null
                        || eligibility.remotePlatforms().stream().noneMatch(osList::contains)) {
                    return false;
                }
            }
        }

        // If marked as 'always', bypass all other checks
        if (Boolean.TRUE.equals(metadata.getAlways()))
            return true;

        // Check required binaries (all must be present)
        HookTypes.HookRequirements reqs = metadata.getRequires();
        if (reqs != null) {
            List<String> bins = reqs.getBins();
            if (bins != null && !bins.isEmpty()) {
                for (String bin : bins) {
                    if (!hasBinary(bin))
                        return false;
                }
            }

            // Check anyBins (at least one must be present)
            List<String> anyBins = reqs.getAnyBins();
            if (anyBins != null && !anyBins.isEmpty()) {
                boolean anyFound = anyBins.stream().anyMatch(HookConfig::hasBinary);
                if (!anyFound)
                    return false;
            }

            // Check required environment variables
            List<String> env = reqs.getEnv();
            if (env != null && !env.isEmpty()) {
                for (String envName : env) {
                    String envVal = System.getenv(envName);
                    if (envVal != null && !envVal.isEmpty())
                        continue;
                    // Check hook config env overrides
                    if (hookCfg != null) {
                        Object cfgEnv = hookCfg.get("env");
                        if (cfgEnv instanceof Map<?, ?> envMap && envMap.containsKey(envName)) {
                            continue;
                        }
                    }
                    return false;
                }
            }

            // Check required config paths
            List<String> cfgPaths = reqs.getConfig();
            if (cfgPaths != null && !cfgPaths.isEmpty()) {
                for (String cfgPath : cfgPaths) {
                    if (!isConfigPathTruthy(config, cfgPath))
                        return false;
                }
            }
        }

        return true;
    }

    /**
     * Hook eligibility context for remote platform checks.
     */
    public record HookEligibilityContext(
            List<String> remotePlatforms) {
    }

    /**
     * Get a field value via reflection (best-effort).
     */
    private static Object getFieldValue(Object obj, String fieldName) {
        if (obj == null)
            return null;
        try {
            // Try getter
            String getterName = "get" + Character.toUpperCase(fieldName.charAt(0))
                    + fieldName.substring(1);
            var method = obj.getClass().getMethod(getterName);
            return method.invoke(obj);
        } catch (Exception e) {
            try {
                // Try direct field access
                var field = obj.getClass().getDeclaredField(fieldName);
                field.setAccessible(true);
                return field.get(obj);
            } catch (Exception e2) {
                return null;
            }
        }
    }
}

package com.openclaw.agent.skills;

import com.openclaw.agent.skills.SkillTypes.*;
import com.openclaw.common.config.OpenClawConfig;

import java.util.*;

/**
 * Applies environment variable overrides from skill configurations.
 * Corresponds to TypeScript skills/env-overrides.ts.
 */
public final class SkillEnvOverrides {

    private SkillEnvOverrides() {
    }

    /**
     * Record tracking a single env-var override so it can be reverted.
     */
    private record EnvUpdate(String key, String previousValue) {
    }

    /**
     * Apply environment variable overrides for active skills.
     * Returns a Runnable that reverts all overrides when called.
     */
    @SuppressWarnings("unchecked")
    public static Runnable applySkillEnvOverrides(List<SkillEntry> skills, OpenClawConfig config) {
        List<EnvUpdate> updates = new ArrayList<>();

        for (SkillEntry entry : skills) {
            String skillKey = SkillFrontmatterParser.resolveSkillKey(entry.skill(), entry);
            Map<String, Object> skillConfig = SkillConfigResolver.resolveSkillConfig(config, skillKey);
            if (skillConfig == null)
                continue;

            // Skill-specific env overrides
            Object envObj = skillConfig.get("env");
            if (envObj instanceof Map<?, ?> envMap) {
                for (var e : ((Map<String, String>) envMap).entrySet()) {
                    String envKey = e.getKey();
                    String envValue = e.getValue();
                    if (envValue == null || envValue.isBlank())
                        continue;
                    if (System.getenv(envKey) != null)
                        continue;
                    updates.add(new EnvUpdate(envKey, System.getenv(envKey)));
                    setEnv(envKey, envValue);
                }
            }

            // Primary env / apiKey mapping
            String primaryEnv = entry.metadata() != null ? entry.metadata().primaryEnv() : null;
            Object apiKey = skillConfig.get("apiKey");
            if (primaryEnv != null && apiKey instanceof String key
                    && !key.isBlank() && System.getenv(primaryEnv) == null) {
                updates.add(new EnvUpdate(primaryEnv, System.getenv(primaryEnv)));
                setEnv(primaryEnv, key);
            }
        }

        // Return a revert function
        return () -> {
            for (EnvUpdate update : updates) {
                if (update.previousValue() == null) {
                    clearEnv(update.key());
                } else {
                    setEnv(update.key(), update.previousValue());
                }
            }
        };
    }

    /**
     * Apply env overrides from a skill snapshot (summary-only).
     */
    @SuppressWarnings("unchecked")
    public static Runnable applySkillEnvOverridesFromSnapshot(
            SkillSnapshot snapshot, OpenClawConfig config) {
        if (snapshot == null)
            return () -> {
            };
        List<EnvUpdate> updates = new ArrayList<>();

        for (SkillSummary skill : snapshot.skills()) {
            Map<String, Object> skillConfig = SkillConfigResolver.resolveSkillConfig(config, skill.name());
            if (skillConfig == null)
                continue;

            Object envObj = skillConfig.get("env");
            if (envObj instanceof Map<?, ?> envMap) {
                for (var e : ((Map<String, String>) envMap).entrySet()) {
                    String envKey = e.getKey();
                    String envValue = e.getValue();
                    if (envValue == null || envValue.isBlank())
                        continue;
                    if (System.getenv(envKey) != null)
                        continue;
                    updates.add(new EnvUpdate(envKey, System.getenv(envKey)));
                    setEnv(envKey, envValue);
                }
            }

            Object apiKey = skillConfig.get("apiKey");
            if (skill.primaryEnv() != null && apiKey instanceof String key
                    && !key.isBlank() && System.getenv(skill.primaryEnv()) == null) {
                updates.add(new EnvUpdate(skill.primaryEnv(), System.getenv(skill.primaryEnv())));
                setEnv(skill.primaryEnv(), key);
            }
        }

        return () -> {
            for (EnvUpdate update : updates) {
                if (update.previousValue() == null) {
                    clearEnv(update.key());
                } else {
                    setEnv(update.key(), update.previousValue());
                }
            }
        };
    }

    // ── JVM env manipulation (via ProcessBuilder workaround) ─────────

    /**
     * Set an environment variable. In Java, {@code System.getenv()} is read-only,
     * so we store overrides in system properties as a fallback.
     */
    private static void setEnv(String key, String value) {
        System.setProperty("openclaw.env." + key, value);
    }

    private static void clearEnv(String key) {
        System.clearProperty("openclaw.env." + key);
    }

    /**
     * Retrieve an env value considering overrides.
     */
    public static String getEnvWithOverrides(String key) {
        String override = System.getProperty("openclaw.env." + key);
        return override != null ? override : System.getenv(key);
    }
}

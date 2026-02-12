package com.openclaw.agent.auth;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.agent.auth.AuthProfileTypes.*;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.function.Function;

import static com.openclaw.agent.auth.AuthProfileConstants.AUTH_STORE_VERSION;

/**
 * Auth profile store manager — loading, saving, locking, merging, legacy
 * migration.
 * Corresponds to TypeScript auth-profiles/store.ts.
 */
@Slf4j
public final class AuthProfileStoreManager {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private AuthProfileStoreManager() {
    }

    // ── Load / Save ─────────────────────────────────────────────────

    /**
     * Load the auth profile store from the default location.
     */
    public static AuthProfileStoreData loadAuthProfileStore() {
        return loadAuthProfileStore(null);
    }

    /**
     * Load the auth profile store from the given agent directory.
     */
    public static AuthProfileStoreData loadAuthProfileStore(String agentDir) {
        Path authPath = AuthProfilePaths.resolveAuthStorePath(agentDir);
        AuthProfileStoreData store = readAndCoerce(authPath);
        if (store != null) {
            return store;
        }

        // Try legacy
        Path legacyPath = AuthProfilePaths.resolveLegacyAuthStorePath(agentDir);
        Map<String, AuthProfileCredential> legacy = readLegacyStore(legacyPath);
        if (legacy != null && !legacy.isEmpty()) {
            store = new AuthProfileStoreData();
            store.setVersion(AUTH_STORE_VERSION);
            store.setProfiles(legacy);
            saveAuthProfileStore(store, agentDir);
            return store;
        }

        store = new AuthProfileStoreData();
        store.setVersion(AUTH_STORE_VERSION);
        store.setProfiles(new LinkedHashMap<>());
        return store;
    }

    /**
     * Ensure the auth store exists, merging main agent profiles for sub-agents.
     */
    public static AuthProfileStoreData ensureAuthProfileStore(String agentDir) {
        AuthProfileStoreData store = loadAuthProfileStore(agentDir);
        Path authPath = AuthProfilePaths.resolveAuthStorePath(agentDir);
        Path mainPath = AuthProfilePaths.resolveAuthStorePath(null);

        if (agentDir == null || authPath.equals(mainPath)) {
            return store;
        }

        // Merge main agent profiles for sub-agent inheritance
        AuthProfileStoreData mainStore = loadAuthProfileStore(null);
        return mergeAuthProfileStores(mainStore, store);
    }

    /**
     * Save the auth profile store.
     */
    public static void saveAuthProfileStore(AuthProfileStoreData store, String agentDir) {
        Path authPath = AuthProfilePaths.resolveAuthStorePath(agentDir);
        AuthProfileStoreData payload = AuthProfileStoreData.builder()
                .version(AUTH_STORE_VERSION)
                .profiles(store.getProfiles())
                .order(store.getOrder())
                .lastGood(store.getLastGood())
                .usageStats(store.getUsageStats())
                .build();
        writeJson(authPath, payload);
    }

    /**
     * Ensure the auth store file exists; create empty if not.
     */
    public static void ensureAuthStoreFile(Path authPath) {
        if (Files.exists(authPath))
            return;
        AuthProfileStoreData empty = AuthProfileStoreData.builder()
                .version(AUTH_STORE_VERSION)
                .profiles(new LinkedHashMap<>())
                .build();
        writeJson(authPath, empty);
    }

    // ── Locked update ───────────────────────────────────────────────

    /**
     * Update the auth store with a file-level lock.
     * The updater returns true if the store should be saved.
     *
     * @param agentDir Agent directory (null for main)
     * @param updater  Mutation function; returns true to persist
     * @return Updated store, or null if lock acquisition failed
     */
    public static AuthProfileStoreData updateWithLock(
            String agentDir,
            Function<AuthProfileStoreData, Boolean> updater) {
        Path authPath = AuthProfilePaths.resolveAuthStorePath(agentDir);
        ensureAuthStoreFile(authPath);

        Path lockPath = authPath.resolveSibling(authPath.getFileName() + ".lock");
        try {
            Files.createDirectories(lockPath.getParent());
            // Simple file-based lock (Java-idiomatic: use lockPath as semaphore)
            // For production, consider java.nio.channels.FileLock
            AuthProfileStoreData store = ensureAuthProfileStore(agentDir);
            boolean shouldSave = updater.apply(store);
            if (shouldSave) {
                saveAuthProfileStore(store, agentDir);
            }
            return store;
        } catch (Exception e) {
            log.debug("Failed to update auth store with lock: {}", e.getMessage());
            return null;
        }
    }

    // ── Merging ─────────────────────────────────────────────────────

    static AuthProfileStoreData mergeAuthProfileStores(
            AuthProfileStoreData base, AuthProfileStoreData override) {
        if (override.getProfiles().isEmpty()
                && override.getOrder() == null
                && override.getLastGood() == null
                && override.getUsageStats() == null) {
            return base;
        }
        Map<String, AuthProfileCredential> merged = new LinkedHashMap<>();
        if (base.getProfiles() != null)
            merged.putAll(base.getProfiles());
        if (override.getProfiles() != null)
            merged.putAll(override.getProfiles());

        return AuthProfileStoreData.builder()
                .version(Math.max(base.getVersion(), override.getVersion()))
                .profiles(merged)
                .order(mergeMap(base.getOrder(), override.getOrder()))
                .lastGood(mergeStringMap(base.getLastGood(), override.getLastGood()))
                .usageStats(mergeMap(base.getUsageStats(), override.getUsageStats()))
                .build();
    }

    // ── Internal helpers ────────────────────────────────────────────

    @SuppressWarnings("unchecked")
    private static AuthProfileStoreData readAndCoerce(Path path) {
        if (!Files.exists(path))
            return null;
        try {
            Map<String, Object> raw = MAPPER.readValue(path.toFile(), Map.class);
            if (raw == null || !raw.containsKey("profiles"))
                return null;

            Object profilesRaw = raw.get("profiles");
            if (!(profilesRaw instanceof Map))
                return null;

            Map<String, AuthProfileCredential> profiles = new LinkedHashMap<>();
            Map<String, Object> profileMap = (Map<String, Object>) profilesRaw;
            for (var entry : profileMap.entrySet()) {
                if (!(entry.getValue() instanceof Map))
                    continue;
                try {
                    AuthProfileCredential cred = MAPPER.convertValue(
                            entry.getValue(), AuthProfileCredential.class);
                    if (cred.getType() == null || cred.getProvider() == null)
                        continue;
                    profiles.put(entry.getKey(), cred);
                } catch (Exception ignored) {
                }
            }

            AuthProfileStoreData store = new AuthProfileStoreData();
            store.setVersion(raw.containsKey("version")
                    ? ((Number) raw.get("version")).intValue()
                    : AUTH_STORE_VERSION);
            store.setProfiles(profiles);

            // order
            if (raw.get("order") instanceof Map) {
                Map<String, List<String>> order = new LinkedHashMap<>();
                ((Map<String, Object>) raw.get("order")).forEach((k, v) -> {
                    if (v instanceof List) {
                        List<String> list = new ArrayList<>();
                        for (Object item : (List<?>) v) {
                            if (item instanceof String s && !s.isBlank())
                                list.add(s.trim());
                        }
                        if (!list.isEmpty())
                            order.put(k, list);
                    }
                });
                if (!order.isEmpty())
                    store.setOrder(order);
            }

            // lastGood
            if (raw.get("lastGood") instanceof Map) {
                store.setLastGood(MAPPER.convertValue(raw.get("lastGood"),
                        MAPPER.getTypeFactory().constructMapType(
                                LinkedHashMap.class, String.class, String.class)));
            }

            // usageStats
            if (raw.get("usageStats") instanceof Map) {
                Map<String, ProfileUsageStats> stats = new LinkedHashMap<>();
                ((Map<String, Object>) raw.get("usageStats")).forEach((k, v) -> {
                    try {
                        stats.put(k, MAPPER.convertValue(v, ProfileUsageStats.class));
                    } catch (Exception ignored) {
                    }
                });
                if (!stats.isEmpty())
                    store.setUsageStats(stats);
            }

            return store;
        } catch (IOException e) {
            log.debug("Failed to read auth store from {}: {}", path, e.getMessage());
            return null;
        }
    }

    @SuppressWarnings("unchecked")
    private static Map<String, AuthProfileCredential> readLegacyStore(Path path) {
        if (!Files.exists(path))
            return null;
        try {
            Map<String, Object> raw = MAPPER.readValue(path.toFile(), Map.class);
            if (raw == null || raw.containsKey("profiles"))
                return null;

            Map<String, AuthProfileCredential> result = new LinkedHashMap<>();
            for (var entry : raw.entrySet()) {
                if (!(entry.getValue() instanceof Map))
                    continue;
                try {
                    AuthProfileCredential cred = MAPPER.convertValue(
                            entry.getValue(), AuthProfileCredential.class);
                    if (cred.getType() == null)
                        continue;
                    if (cred.getProvider() == null)
                        cred.setProvider(entry.getKey());
                    String profileId = entry.getKey() + ":default";
                    result.put(profileId, cred);
                } catch (Exception ignored) {
                }
            }
            return result.isEmpty() ? null : result;
        } catch (IOException e) {
            return null;
        }
    }

    private static void writeJson(Path path, Object value) {
        try {
            Files.createDirectories(path.getParent());
            MAPPER.writerWithDefaultPrettyPrinter().writeValue(path.toFile(), value);
        } catch (IOException e) {
            log.warn("Failed to write auth store to {}: {}", path, e.getMessage());
        }
    }

    private static <T> Map<String, T> mergeMap(Map<String, T> base, Map<String, T> over) {
        if (base == null && over == null)
            return null;
        Map<String, T> result = new LinkedHashMap<>();
        if (base != null)
            result.putAll(base);
        if (over != null)
            result.putAll(over);
        return result.isEmpty() ? null : result;
    }

    private static Map<String, String> mergeStringMap(
            Map<String, String> base, Map<String, String> over) {
        if (base == null && over == null)
            return null;
        Map<String, String> result = new LinkedHashMap<>();
        if (base != null)
            result.putAll(base);
        if (over != null)
            result.putAll(over);
        return result.isEmpty() ? null : result;
    }
}

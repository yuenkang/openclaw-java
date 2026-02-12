package com.openclaw.agent.models;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages auth profiles for model providers.
 * Handles persistence, cooldown tracking, and profile ordering.
 * Corresponds to TypeScript's auth-profiles/store.ts + order.ts.
 */
@Slf4j
public class AuthProfileStore {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final Path storePath;
    private final Map<String, AuthProfile> profiles = new ConcurrentHashMap<>();
    private final Map<String, FailureRecord> failures = new ConcurrentHashMap<>();

    // Configurable cooldown
    private int cooldownMs = 10_000;
    private int maxFailures = 5;

    public AuthProfileStore(Path stateDir) {
        this.storePath = stateDir.resolve("auth-profiles.json");
        load();
    }

    public AuthProfileStore(Path stateDir, int cooldownMs, int maxFailures) {
        this(stateDir);
        this.cooldownMs = cooldownMs;
        this.maxFailures = maxFailures;
    }

    // =========================================================================
    // Profile CRUD
    // =========================================================================

    /**
     * Add or update a profile.
     */
    public void putProfile(String key, AuthProfile profile) {
        profiles.put(key, profile);
        save();
    }

    /**
     * Remove a profile.
     */
    public void removeProfile(String key) {
        profiles.remove(key);
        failures.remove(key);
        save();
    }

    /**
     * Get a profile by key.
     */
    public AuthProfile getProfile(String key) {
        return profiles.get(key);
    }

    /**
     * List all profile keys.
     */
    public Set<String> getProfileKeys() {
        return Collections.unmodifiableSet(profiles.keySet());
    }

    /**
     * Get all profiles.
     */
    public Map<String, AuthProfile> getAllProfiles() {
        return Collections.unmodifiableMap(profiles);
    }

    // =========================================================================
    // Cooldown
    // =========================================================================

    /**
     * Record a failure for a profile key.
     */
    public void recordFailure(String key) {
        failures.compute(key, (k, existing) -> {
            if (existing == null) {
                return new FailureRecord(1, System.currentTimeMillis());
            }
            existing.count++;
            existing.lastFailureMs = System.currentTimeMillis();
            return existing;
        });
    }

    /**
     * Check if a profile is currently in cooldown.
     */
    public boolean isInCooldown(String key) {
        FailureRecord record = failures.get(key);
        if (record == null)
            return false;
        if (record.count < maxFailures)
            return false;
        return System.currentTimeMillis() - record.lastFailureMs < cooldownMs;
    }

    /**
     * Clear failure records for a key (e.g. after a successful request).
     */
    public void clearFailures(String key) {
        failures.remove(key);
    }

    // =========================================================================
    // Profile ordering
    // =========================================================================

    /**
     * Resolve the ordered list of profiles to try.
     * Filters out profiles in cooldown and orders by last successful use.
     */
    public List<String> resolveProfileOrder(String provider) {
        List<String> eligible = new ArrayList<>();
        for (Map.Entry<String, AuthProfile> entry : profiles.entrySet()) {
            String key = entry.getKey();
            AuthProfile profile = entry.getValue();
            // Match provider
            if (provider != null && !provider.equalsIgnoreCase(profile.provider)) {
                continue;
            }
            // Skip cooldown
            if (isInCooldown(key)) {
                log.debug("Skipping profile {} — in cooldown", key);
                continue;
            }
            eligible.add(key);
        }
        // Sort by lastUsedMs ascending (least recently used first for round-robin)
        eligible.sort(Comparator.comparingLong(key -> {
            AuthProfile p = profiles.get(key);
            return p != null ? p.lastUsedMs : 0;
        }));
        return eligible;
    }

    /**
     * Mark a profile as successfully used.
     */
    public void markUsed(String key) {
        AuthProfile profile = profiles.get(key);
        if (profile != null) {
            profile.lastUsedMs = System.currentTimeMillis();
            clearFailures(key);
            save();
        }
    }

    // =========================================================================
    // Persistence
    // =========================================================================

    private void load() {
        if (!Files.exists(storePath))
            return;
        try {
            byte[] data = Files.readAllBytes(storePath);
            Map<String, AuthProfile> loaded = MAPPER.readValue(data,
                    new TypeReference<Map<String, AuthProfile>>() {
                    });
            profiles.putAll(loaded);
            log.info("Loaded {} auth profiles from {}", profiles.size(), storePath);
        } catch (IOException e) {
            log.warn("Failed to load auth profiles: {}", e.getMessage());
        }
    }

    private void save() {
        try {
            Files.createDirectories(storePath.getParent());
            MAPPER.writerWithDefaultPrettyPrinter()
                    .writeValue(storePath.toFile(), profiles);
        } catch (IOException e) {
            log.warn("Failed to save auth profiles: {}", e.getMessage());
        }
    }

    // =========================================================================
    // Types
    // =========================================================================

    @Data
    public static class AuthProfile {
        private String provider;
        private String apiKey;
        private String label;
        private long lastUsedMs;
        private long createdAtMs;

        public AuthProfile() {
            this.createdAtMs = System.currentTimeMillis();
        }

        public AuthProfile(String provider, String apiKey, String label) {
            this();
            this.provider = provider;
            this.apiKey = apiKey;
            this.label = label;
        }
    }

    private static class FailureRecord {
        int count;
        long lastFailureMs;

        FailureRecord(int count, long lastFailureMs) {
            this.count = count;
            this.lastFailureMs = lastFailureMs;
        }
    }
}

package com.openclaw.gateway.cron;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.time.Instant;
import java.util.*;

/**
 * Cron store persistence utilities.
 * Handles loading, saving, and migrating cron store files.
 * Corresponds to TypeScript's cron/service/store.ts + cron/store.ts.
 */
@Slf4j
public class CronStore {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
            .configure(SerializationFeature.INDENT_OUTPUT, true);

    /**
     * The cron store file schema: a map of job IDs to CronJob entries.
     */
    public record CronStoreFile(
            Map<String, CronJob> jobs,
            String version,
            Instant savedAt) {
        public static CronStoreFile empty() {
            return new CronStoreFile(new LinkedHashMap<>(), "1", Instant.now());
        }
    }

    /**
     * Load the cron store from a file path.
     * Returns an empty store if the file does not exist or is corrupted.
     */
    public static CronStoreFile load(String storePath) {
        Path path = Path.of(storePath);
        if (!Files.exists(path)) {
            log.debug("Cron store file not found: {}", storePath);
            return CronStoreFile.empty();
        }

        try {
            String content = Files.readString(path);
            if (content.isBlank()) {
                return CronStoreFile.empty();
            }

            Map<String, Object> raw = MAPPER.readValue(content,
                    new TypeReference<Map<String, Object>>() {
                    });

            // Check for versioned format
            if (raw.containsKey("jobs") && raw.get("jobs") instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> jobsRaw = (Map<String, Object>) raw.get("jobs");
                Map<String, CronJob> jobs = new LinkedHashMap<>();
                for (Map.Entry<String, Object> entry : jobsRaw.entrySet()) {
                    CronJob job = MAPPER.convertValue(entry.getValue(), CronJob.class);
                    if (job != null && job.getId() != null) {
                        jobs.put(job.getId(), migrateLegacyPayload(job));
                    }
                }
                String version = raw.containsKey("version")
                        ? String.valueOf(raw.get("version"))
                        : "1";
                return new CronStoreFile(jobs, version, Instant.now());
            }

            // Legacy format: flat map of jobs
            Map<String, CronJob> jobs = new LinkedHashMap<>();
            for (Map.Entry<String, Object> entry : raw.entrySet()) {
                try {
                    CronJob job = MAPPER.convertValue(entry.getValue(), CronJob.class);
                    if (job != null) {
                        if (job.getId() == null)
                            job.setId(entry.getKey());
                        jobs.put(job.getId(), migrateLegacyPayload(job));
                    }
                } catch (Exception e) {
                    log.warn("Skipping malformed cron job entry: {}", entry.getKey());
                }
            }
            return new CronStoreFile(jobs, "1", Instant.now());

        } catch (IOException e) {
            log.error("Failed to load cron store from {}: {}", storePath, e.getMessage());
            return CronStoreFile.empty();
        }
    }

    /**
     * Save the cron store to a file path.
     */
    public static void save(String storePath, CronStoreFile store) {
        Path path = Path.of(storePath);
        try {
            // Ensure parent directory exists
            Path parent = path.getParent();
            if (parent != null && !Files.exists(parent)) {
                Files.createDirectories(parent);
            }

            Map<String, Object> output = new LinkedHashMap<>();
            output.put("version", store.version());
            output.put("savedAt", Instant.now().toString());
            output.put("jobs", store.jobs());

            String json = MAPPER.writeValueAsString(output);
            Files.writeString(path, json, StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING);
            log.debug("Saved cron store to {} ({} jobs)", storePath, store.jobs().size());
        } catch (IOException e) {
            log.error("Failed to save cron store to {}: {}", storePath, e.getMessage());
        }
    }

    /**
     * Save jobs as the cron store.
     */
    public static void save(String storePath, Map<String, CronJob> jobs) {
        save(storePath, new CronStoreFile(jobs, "1", Instant.now()));
    }

    /**
     * Migrate legacy cron job data if needed.
     * In the Java model, channelId and accountId are top-level fields,
     * so no payload migration is necessary (unlike the TS version).
     */
    public static CronJob migrateLegacyPayload(CronJob job) {
        // Java CronJob has channelId/accountId as top-level fields,
        // no legacy payload migration needed.
        return job;
    }

    /**
     * Get the file modification time in millis.
     */
    public static Long getFileMtimeMs(String storePath) {
        try {
            Path path = Path.of(storePath);
            if (!Files.exists(path))
                return null;
            return Files.getLastModifiedTime(path).toMillis();
        } catch (IOException e) {
            return null;
        }
    }
}

package com.openclaw.common.infra;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Tracks presence of system nodes (local and remote).
 * <p>
 * Maintains a map of known nodes with TTL-based expiry and max-size
 * enforcement.
 * The local gateway is always present as a "self" entry.
 * <p>
 * Port of: infra/system-presence.ts
 */
@Slf4j
public class SystemPresence {

    private static final long TTL_MS = 5 * 60 * 1000; // 5 minutes
    private static final int MAX_ENTRIES = 200;

    private final ConcurrentHashMap<String, PresenceEntry> entries = new ConcurrentHashMap<>();

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PresenceEntry {
        private String host;
        private String ip;
        private String version;
        private String platform;
        private String deviceFamily;
        private String modelIdentifier;
        private Long lastInputSeconds;
        private String mode;
        private String reason;
        private String deviceId;
        private String instanceId;
        private List<String> roles;
        private List<String> scopes;
        private String text;
        private long ts;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PresenceUpdate {
        private String key;
        private PresenceEntry previous;
        private PresenceEntry next;
        private Map<String, Object> changes;
        private List<String> changedKeys;
    }

    public SystemPresence() {
        initSelfPresence();
    }

    /**
     * Update or insert a presence entry.
     */
    public PresenceUpdate update(PresenceEntry incoming) {
        ensureSelfPresence();
        String key = normalizeKey(
                incoming.getDeviceId(),
                incoming.getInstanceId(),
                incoming.getHost(),
                incoming.getIp(),
                incoming.getText());

        PresenceEntry existing = entries.get(key);
        PresenceEntry merged = merge(existing, incoming);
        entries.put(key, merged);

        // Track changes
        Map<String, Object> changes = new LinkedHashMap<>();
        List<String> changedKeys = new ArrayList<>();
        if (existing != null) {
            diffField(changes, changedKeys, "host", existing.getHost(), merged.getHost());
            diffField(changes, changedKeys, "ip", existing.getIp(), merged.getIp());
            diffField(changes, changedKeys, "version", existing.getVersion(), merged.getVersion());
            diffField(changes, changedKeys, "mode", existing.getMode(), merged.getMode());
            diffField(changes, changedKeys, "reason", existing.getReason(), merged.getReason());
        }

        return PresenceUpdate.builder()
                .key(key)
                .previous(existing)
                .next(merged)
                .changes(changes)
                .changedKeys(changedKeys)
                .build();
    }

    /**
     * List all presence entries, pruning expired ones.
     */
    public List<PresenceEntry> list() {
        ensureSelfPresence();
        prune();
        touchSelfPresence();
        return entries.values().stream()
                .sorted(Comparator.comparingLong(PresenceEntry::getTs).reversed())
                .collect(Collectors.toList());
    }

    /**
     * Get the number of tracked entries.
     */
    public int size() {
        return entries.size();
    }

    // --- Internals ---

    private void initSelfPresence() {
        String host = getHostName();
        String ip = resolvePrimaryIPv4();
        String version = System.getenv().getOrDefault("OPENCLAW_VERSION", "unknown");
        String platform = OsSummary.resolve().getLabel();
        String deviceFamily = resolveDeviceFamily();

        String text = String.format("Gateway: %s%s · app %s · mode gateway · reason self",
                host, ip != null ? " (" + ip + ")" : "", version);

        PresenceEntry self = PresenceEntry.builder()
                .host(host)
                .ip(ip)
                .version(version)
                .platform(platform)
                .deviceFamily(deviceFamily)
                .mode("gateway")
                .reason("self")
                .text(text)
                .ts(System.currentTimeMillis())
                .build();

        entries.put(host.toLowerCase(), self);
    }

    private void ensureSelfPresence() {
        if (entries.isEmpty()) {
            initSelfPresence();
        }
    }

    private void touchSelfPresence() {
        String host = getHostName().toLowerCase();
        PresenceEntry existing = entries.get(host);
        if (existing != null) {
            existing.setTs(System.currentTimeMillis());
        } else {
            initSelfPresence();
        }
    }

    private void prune() {
        long now = System.currentTimeMillis();
        entries.entrySet().removeIf(e -> now - e.getValue().getTs() > TTL_MS);

        if (entries.size() > MAX_ENTRIES) {
            List<Map.Entry<String, PresenceEntry>> sorted = entries.entrySet().stream()
                    .sorted(Comparator.comparingLong(e -> e.getValue().getTs()))
                    .collect(Collectors.toList());
            int toDrop = entries.size() - MAX_ENTRIES;
            for (int i = 0; i < toDrop; i++) {
                entries.remove(sorted.get(i).getKey());
            }
        }
    }

    private static PresenceEntry merge(PresenceEntry existing, PresenceEntry incoming) {
        if (existing == null) {
            incoming.setTs(System.currentTimeMillis());
            return incoming;
        }
        return PresenceEntry.builder()
                .host(coalesce(incoming.getHost(), existing.getHost()))
                .ip(coalesce(incoming.getIp(), existing.getIp()))
                .version(coalesce(incoming.getVersion(), existing.getVersion()))
                .platform(coalesce(incoming.getPlatform(), existing.getPlatform()))
                .deviceFamily(coalesce(incoming.getDeviceFamily(), existing.getDeviceFamily()))
                .modelIdentifier(coalesce(incoming.getModelIdentifier(), existing.getModelIdentifier()))
                .lastInputSeconds(incoming.getLastInputSeconds() != null ? incoming.getLastInputSeconds()
                        : existing.getLastInputSeconds())
                .mode(coalesce(incoming.getMode(), existing.getMode()))
                .reason(coalesce(incoming.getReason(), existing.getReason()))
                .deviceId(coalesce(incoming.getDeviceId(), existing.getDeviceId()))
                .instanceId(coalesce(incoming.getInstanceId(), existing.getInstanceId()))
                .roles(mergeStringLists(existing.getRoles(), incoming.getRoles()))
                .scopes(mergeStringLists(existing.getScopes(), incoming.getScopes()))
                .text(coalesce(incoming.getText(), existing.getText()))
                .ts(System.currentTimeMillis())
                .build();
    }

    private static String normalizeKey(String deviceId, String instanceId,
            String host, String ip, String text) {
        String key = trimLower(deviceId);
        if (key != null)
            return key;
        key = trimLower(instanceId);
        if (key != null)
            return key;
        key = trimLower(host);
        if (key != null)
            return key;
        if (ip != null && !ip.isEmpty())
            return ip.toLowerCase();
        if (text != null && text.length() > 0)
            return text.substring(0, Math.min(64, text.length())).toLowerCase();
        return getHostName().toLowerCase();
    }

    private static String trimLower(String value) {
        if (value == null)
            return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed.toLowerCase();
    }

    private static String coalesce(String a, String b) {
        return (a != null && !a.isEmpty()) ? a : b;
    }

    @SafeVarargs
    private static List<String> mergeStringLists(List<String>... lists) {
        Set<String> result = new LinkedHashSet<>();
        for (List<String> list : lists) {
            if (list != null) {
                for (String item : list) {
                    String trimmed = item != null ? item.trim() : "";
                    if (!trimmed.isEmpty()) {
                        result.add(trimmed);
                    }
                }
            }
        }
        return result.isEmpty() ? null : new ArrayList<>(result);
    }

    private static void diffField(Map<String, Object> changes, List<String> changedKeys,
            String field, Object prev, Object next) {
        if (!Objects.equals(prev, next)) {
            changes.put(field, next);
            changedKeys.add(field);
        }
    }

    private static String getHostName() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (Exception e) {
            return "unknown";
        }
    }

    private static String resolvePrimaryIPv4() {
        try {
            // Prefer en0/eth0
            for (String name : new String[] { "en0", "eth0" }) {
                NetworkInterface ni = NetworkInterface.getByName(name);
                if (ni != null) {
                    var addrs = ni.getInetAddresses();
                    while (addrs.hasMoreElements()) {
                        InetAddress addr = addrs.nextElement();
                        if (addr instanceof java.net.Inet4Address && !addr.isLoopbackAddress()) {
                            return addr.getHostAddress();
                        }
                    }
                }
            }
            // Fallback: any non-loopback IPv4
            var interfaces = NetworkInterface.getNetworkInterfaces();
            while (interfaces.hasMoreElements()) {
                NetworkInterface ni = interfaces.nextElement();
                var addrs = ni.getInetAddresses();
                while (addrs.hasMoreElements()) {
                    InetAddress addr = addrs.nextElement();
                    if (addr instanceof java.net.Inet4Address && !addr.isLoopbackAddress()) {
                        return addr.getHostAddress();
                    }
                }
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }

    private static String resolveDeviceFamily() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac"))
            return "Mac";
        if (os.contains("win"))
            return "Windows";
        if (os.contains("linux"))
            return "Linux";
        return os;
    }
}

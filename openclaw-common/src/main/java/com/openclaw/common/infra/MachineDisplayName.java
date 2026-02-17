package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.util.concurrent.TimeUnit;

/**
 * Resolves a human-readable display name for this machine.
 * <p>
 * On macOS, uses {@code scutil --get ComputerName}, falling back to hostname.
 * <p>
 * Port of: infra/machine-name.ts
 */
@Slf4j
public class MachineDisplayName {

    private static volatile String cached;

    /**
     * Get the machine's display name, cached after first call.
     */
    public static String get() {
        if (cached != null) {
            return cached;
        }
        synchronized (MachineDisplayName.class) {
            if (cached != null) {
                return cached;
            }
            cached = resolve();
            return cached;
        }
    }

    private static String resolve() {
        String osName = System.getProperty("os.name", "").toLowerCase();
        if (osName.contains("mac")) {
            // Try scutil --get ComputerName
            String name = tryScutil("ComputerName");
            if (name != null) {
                return name;
            }
            name = tryScutil("LocalHostName");
            if (name != null) {
                return name;
            }
        }
        return fallbackHostName();
    }

    private static String tryScutil(String key) {
        try {
            ProcessBuilder pb = new ProcessBuilder("/usr/sbin/scutil", "--get", key);
            pb.redirectErrorStream(true);
            Process p = pb.start();
            boolean finished = p.waitFor(1, TimeUnit.SECONDS);
            if (!finished) {
                p.destroyForcibly();
                return null;
            }
            if (p.exitValue() != 0) {
                return null;
            }
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
                String line = reader.readLine();
                if (line != null) {
                    String trimmed = line.trim();
                    return trimmed.isEmpty() ? null : trimmed;
                }
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }

    private static String fallbackHostName() {
        try {
            String hostname = InetAddress.getLocalHost().getHostName();
            if (hostname != null) {
                String cleaned = hostname.replaceAll("(?i)\\.local$", "").trim();
                return cleaned.isEmpty() ? "openclaw" : cleaned;
            }
        } catch (Exception e) {
            // ignore
        }
        return "openclaw";
    }
}

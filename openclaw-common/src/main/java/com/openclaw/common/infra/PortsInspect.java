package com.openclaw.common.infra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * Port usage inspection — lsof/ss/netstat integration and port availability
 * checks.
 * <p>
 * Corresponds to TypeScript's infra/ports-inspect.ts + ports.ts +
 * ports-lsof.ts.
 */
public final class PortsInspect {

    private PortsInspect() {
    }

    private static final Logger log = LoggerFactory.getLogger(PortsInspect.class);
    private static final long CMD_TIMEOUT_MS = 5_000;

    // =========================================================================
    // Data model
    // =========================================================================

    public enum PortStatus {
        FREE, BUSY, UNKNOWN
    }

    public record PortListener(
            Integer pid,
            String command,
            String commandLine,
            String address,
            String user) {
    }

    public record PortUsage(
            int port,
            PortStatus status,
            List<PortListener> listeners,
            List<String> hints,
            List<String> errors) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Inspect whether a port is in use and who is using it.
     */
    public static PortUsage inspectPort(int port) {
        List<String> errors = new ArrayList<>();
        boolean isWindows = System.getProperty("os.name", "").toLowerCase().contains("win");

        LsofResult result;
        if (isWindows) {
            result = readWindowsListeners(port);
        } else {
            result = readUnixListeners(port);
        }
        errors.addAll(result.errors);

        PortStatus status;
        List<PortListener> listeners = result.listeners;

        if (!listeners.isEmpty()) {
            status = PortStatus.BUSY;
        } else {
            // Fallback: try to bind
            status = checkPortFree(port);
        }

        if (status != PortStatus.BUSY) {
            listeners = List.of();
        }

        List<String> hints = buildHints(listeners, port);
        if (status == PortStatus.BUSY && listeners.isEmpty()) {
            hints.add("Port is in use but process details are unavailable " +
                    "(install lsof or run as an admin user).");
        }

        return new PortUsage(port, status, listeners, hints, errors);
    }

    /**
     * Check if a port is available for binding.
     */
    public static boolean isPortFree(int port) {
        return checkPortFree(port) == PortStatus.FREE;
    }

    // =========================================================================
    // Unix (lsof) + macOS/Linux
    // =========================================================================

    private record LsofResult(List<PortListener> listeners, List<String> errors) {
    }

    private static LsofResult readUnixListeners(int port) {
        List<String> errors = new ArrayList<>();
        String lsof = resolveLsofCommand();
        CmdResult res = runCmd(lsof, "-nP", "-iTCP:" + port, "-sTCP:LISTEN", "-FpcFn");

        if (res.exitCode == 0) {
            List<PortListener> listeners = parseLsofFieldOutput(res.stdout);
            // Enrich with ps info
            for (int i = 0; i < listeners.size(); i++) {
                PortListener l = listeners.get(i);
                if (l.pid() != null) {
                    String cmdLine = resolveUnixCommandLine(l.pid());
                    String user = resolveUnixUser(l.pid());
                    listeners.set(i, new PortListener(l.pid(), l.command(),
                            cmdLine != null ? cmdLine : l.commandLine(),
                            l.address(),
                            user != null ? user : l.user()));
                }
            }
            return new LsofResult(listeners, errors);
        }

        // lsof returns 1 with no output when port not found
        if (res.exitCode == 1 && (res.stderr == null || res.stderr.isBlank()) && res.error == null) {
            return new LsofResult(List.of(), errors);
        }

        if (res.error != null) {
            errors.add(res.error);
        }
        return new LsofResult(List.of(), errors);
    }

    static List<PortListener> parseLsofFieldOutput(String output) {
        List<PortListener> listeners = new ArrayList<>();
        if (output == null || output.isBlank()) {
            return listeners;
        }

        Integer currentPid = null;
        String currentCmd = null;
        String currentAddr = null;

        for (String line : output.split("\\r?\\n")) {
            if (line.isEmpty()) {
                continue;
            }
            char prefix = line.charAt(0);
            String value = line.substring(1);

            switch (prefix) {
                case 'p' -> {
                    // Flush previous
                    if (currentPid != null || currentCmd != null) {
                        listeners.add(new PortListener(currentPid, currentCmd, null,
                                currentAddr, null));
                    }
                    try {
                        currentPid = Integer.parseInt(value);
                    } catch (NumberFormatException e) {
                        currentPid = null;
                    }
                    currentCmd = null;
                    currentAddr = null;
                }
                case 'c' -> currentCmd = value;
                case 'n' -> {
                    if (currentAddr == null) {
                        currentAddr = value;
                    }
                }
            }
        }
        // Flush last
        if (currentPid != null || currentCmd != null) {
            listeners.add(new PortListener(currentPid, currentCmd, null, currentAddr, null));
        }

        return listeners;
    }

    private static String resolveUnixCommandLine(int pid) {
        CmdResult res = runCmd("ps", "-p", String.valueOf(pid), "-o", "command=");
        if (res.exitCode == 0 && res.stdout != null) {
            String trimmed = res.stdout.trim();
            return trimmed.isEmpty() ? null : trimmed;
        }
        return null;
    }

    private static String resolveUnixUser(int pid) {
        CmdResult res = runCmd("ps", "-p", String.valueOf(pid), "-o", "user=");
        if (res.exitCode == 0 && res.stdout != null) {
            String trimmed = res.stdout.trim();
            return trimmed.isEmpty() ? null : trimmed;
        }
        return null;
    }

    // =========================================================================
    // Windows (netstat)
    // =========================================================================

    private static LsofResult readWindowsListeners(int port) {
        List<String> errors = new ArrayList<>();
        CmdResult res = runCmd("netstat", "-ano", "-p", "tcp");
        if (res.exitCode != 0) {
            if (res.error != null)
                errors.add(res.error);
            return new LsofResult(List.of(), errors);
        }

        List<PortListener> listeners = new ArrayList<>();
        String portToken = ":" + port;
        for (String rawLine : res.stdout.split("\\r?\\n")) {
            String line = rawLine.trim();
            if (line.isEmpty() || !line.toLowerCase().contains("listen") || !line.contains(portToken)) {
                continue;
            }
            String[] parts = line.split("\\s+");
            if (parts.length < 4)
                continue;
            Integer pid = null;
            try {
                pid = Integer.parseInt(parts[parts.length - 1]);
            } catch (NumberFormatException ignored) {
            }
            String address = parts[1].contains(portToken) ? parts[1] : null;
            listeners.add(new PortListener(pid, null, null, address, null));
        }
        return new LsofResult(listeners, errors);
    }

    // =========================================================================
    // Port binding check
    // =========================================================================

    static PortStatus checkPortFree(int port) {
        String[] hosts = { "127.0.0.1", "0.0.0.0" };
        for (String host : hosts) {
            try (ServerSocket socket = new ServerSocket()) {
                socket.setReuseAddress(false);
                socket.bind(new InetSocketAddress(host, port));
            } catch (IOException e) {
                return PortStatus.BUSY;
            }
        }
        return PortStatus.FREE;
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static String resolveLsofCommand() {
        // Prefer system lsof; some macOS versions don't have it on PATH
        if (Binaries.hasBinary("lsof")) {
            return "lsof";
        }
        // Try common fallback
        if (new java.io.File("/usr/sbin/lsof").canExecute()) {
            return "/usr/sbin/lsof";
        }
        return "lsof";
    }

    private static List<String> buildHints(List<PortListener> listeners, int port) {
        List<String> hints = new ArrayList<>();
        for (PortListener l : listeners) {
            StringBuilder sb = new StringBuilder();
            if (l.command() != null) {
                sb.append(l.command());
            }
            if (l.pid() != null) {
                sb.append(" (PID ").append(l.pid()).append(')');
            }
            if (l.user() != null) {
                sb.append(" [").append(l.user()).append(']');
            }
            if (!sb.isEmpty()) {
                hints.add("Port " + port + " is used by: " + sb.toString().trim());
            }
        }
        return hints;
    }

    private record CmdResult(String stdout, String stderr, int exitCode, String error) {
    }

    private static CmdResult runCmd(String... argv) {
        try {
            ProcessBuilder pb = new ProcessBuilder(argv).redirectErrorStream(false);
            Process process = pb.start();
            String stdout;
            String stderr;
            try (BufferedReader r1 = new BufferedReader(new InputStreamReader(process.getInputStream()));
                    BufferedReader r2 = new BufferedReader(new InputStreamReader(process.getErrorStream()))) {
                stdout = readAll(r1);
                stderr = readAll(r2);
            }
            boolean finished = process.waitFor(CMD_TIMEOUT_MS, TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                return new CmdResult("", "", 1, "timeout");
            }
            return new CmdResult(stdout, stderr, process.exitValue(), null);
        } catch (Exception e) {
            return new CmdResult("", "", 1, e.getMessage());
        }
    }

    private static String readAll(BufferedReader reader) throws IOException {
        StringBuilder sb = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            if (!sb.isEmpty())
                sb.append('\n');
            sb.append(line);
        }
        return sb.toString();
    }
}

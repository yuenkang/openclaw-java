package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

/**
 * Shell configuration and binary-output sanitisation.
 * Mirrors {@code agents/shell-utils.ts}.
 */
@Slf4j
public final class ShellUtils {

    private ShellUtils() {
    }

    public record ShellConfig(String shell, String[] args) {
    }

    /**
     * Resolve the user's shell configuration.
     * On macOS/Linux uses {@code $SHELL} (falls back to {@code sh}),
     * skips fish in favour of bash.
     * On Windows uses PowerShell.
     */
    public static ShellConfig getShellConfig() {
        String os = System.getProperty("os.name", "").toLowerCase();

        if (os.contains("win")) {
            String pwsh = resolvePowerShellPath();
            return new ShellConfig(pwsh, new String[] { "-NoProfile", "-NonInteractive", "-Command" });
        }

        String envShell = System.getenv("SHELL");
        if (envShell != null)
            envShell = envShell.trim();
        String shellName = envShell != null ? Path.of(envShell).getFileName().toString() : "";

        // Fish rejects common bashisms used by tools
        if ("fish".equals(shellName)) {
            String bash = resolveShellFromPath("bash");
            if (bash != null)
                return new ShellConfig(bash, new String[] { "-c" });
            String sh = resolveShellFromPath("sh");
            if (sh != null)
                return new ShellConfig(sh, new String[] { "-c" });
        }

        String shell = (envShell != null && !envShell.isEmpty()) ? envShell : "sh";
        return new ShellConfig(shell, new String[] { "-c" });
    }

    /**
     * Strip non-printable binary control characters from command output,
     * preserving tabs, newlines, and carriage returns.
     */
    public static String sanitizeBinaryOutput(String text) {
        if (text == null || text.isEmpty())
            return "";

        // Remove Unicode format and surrogate characters
        String scrubbed = text.replaceAll("[\\p{Cf}\\p{Cs}]", "");
        if (scrubbed.isEmpty())
            return scrubbed;

        StringBuilder sb = new StringBuilder(scrubbed.length());
        for (int i = 0; i < scrubbed.length();) {
            int cp = scrubbed.codePointAt(i);
            // Allow tab, newline, carriage return
            if (cp == 0x09 || cp == 0x0A || cp == 0x0D) {
                sb.appendCodePoint(cp);
            } else if (cp >= 0x20) {
                // Allow printable characters
                sb.appendCodePoint(cp);
            }
            // Skip control chars < 0x20 (except tab/nl/cr)
            i += Character.charCount(cp);
        }
        return sb.toString();
    }

    /**
     * Kill a process and its child tree.
     * Uses {@code kill -9 -pid} (process group) on Unix,
     * {@code taskkill /F /T /PID} on Windows.
     */
    public static void killProcessTree(long pid) {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("win")) {
            try {
                new ProcessBuilder("taskkill", "/F", "/T", "/PID", String.valueOf(pid))
                        .redirectErrorStream(true)
                        .start();
            } catch (Exception ignored) {
            }
            return;
        }

        // Try killing process group first, then individual PID
        try {
            // SIGKILL to process group (negative PID)
            new ProcessBuilder("kill", "-9", "-" + pid)
                    .redirectErrorStream(true)
                    .start()
                    .waitFor(2, TimeUnit.SECONDS);
        } catch (Exception e) {
            try {
                new ProcessBuilder("kill", "-9", String.valueOf(pid))
                        .redirectErrorStream(true)
                        .start()
                        .waitFor(2, TimeUnit.SECONDS);
            } catch (Exception ignored) {
            }
        }
    }

    // --- Internal helpers ---

    private static String resolvePowerShellPath() {
        String systemRoot = System.getenv("SystemRoot");
        if (systemRoot == null)
            systemRoot = System.getenv("WINDIR");
        if (systemRoot != null) {
            Path candidate = Path.of(systemRoot, "System32", "WindowsPowerShell",
                    "v1.0", "powershell.exe");
            if (Files.isRegularFile(candidate)) {
                return candidate.toString();
            }
        }
        return "powershell.exe";
    }

    private static String resolveShellFromPath(String name) {
        String envPath = System.getenv("PATH");
        if (envPath == null || envPath.isEmpty())
            return null;

        for (String dir : envPath.split(File.pathSeparator)) {
            if (dir.isEmpty())
                continue;
            Path candidate = Path.of(dir, name);
            if (Files.isRegularFile(candidate) && Files.isExecutable(candidate)) {
                return candidate.toString();
            }
        }
        return null;
    }
}

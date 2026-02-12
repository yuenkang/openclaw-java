package com.openclaw.agent.runtime;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;

/**
 * Sandbox path resolution and validation utilities.
 * Mirrors {@code agents/sandbox-paths.ts}.
 */
public final class SandboxPaths {

    private SandboxPaths() {
    }

    private static final Pattern UNICODE_SPACES = Pattern.compile("[\\u00A0\\u2000-\\u200A\\u202F\\u205F\\u3000]");
    private static final Pattern HTTP_URL = Pattern.compile("^https?://", Pattern.CASE_INSENSITIVE);
    private static final Pattern DATA_URL = Pattern.compile("^data:", Pattern.CASE_INSENSITIVE);
    private static final Pattern FILE_URL = Pattern.compile("^file://", Pattern.CASE_INSENSITIVE);

    // --- Public types ---

    /** Result of resolving a sandbox path. */
    public record SandboxPathResult(String resolved, String relative) {
    }

    // --- Public API ---

    /**
     * Resolve a file path within a sandbox root, ensuring it doesn't escape.
     *
     * @throws IllegalArgumentException if path escapes the sandbox root
     */
    public static SandboxPathResult resolveSandboxPath(String filePath, String cwd, String root) {
        String resolved = resolveToCwd(filePath, cwd);
        String rootResolved = Path.of(root).toAbsolutePath().normalize().toString();
        Path rootPath = Path.of(rootResolved);
        Path resolvedPath = Path.of(resolved);
        Path relative;
        try {
            relative = rootPath.relativize(resolvedPath);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(
                    "Path escapes sandbox root (" + shortPath(rootResolved) + "): " + filePath);
        }
        String relStr = relative.toString();
        if (relStr.isEmpty()) {
            return new SandboxPathResult(resolved, "");
        }
        if (relStr.startsWith("..") || Path.of(relStr).isAbsolute()) {
            throw new IllegalArgumentException(
                    "Path escapes sandbox root (" + shortPath(rootResolved) + "): " + filePath);
        }
        return new SandboxPathResult(resolved, relStr);
    }

    /**
     * Assert a sandbox path is safe (no symlinks in the chain).
     *
     * @throws IllegalArgumentException if path escapes root
     * @throws IOException              if a symlink is found or I/O fails
     */
    public static SandboxPathResult assertSandboxPath(String filePath, String cwd, String root)
            throws IOException {
        SandboxPathResult result = resolveSandboxPath(filePath, cwd, root);
        assertNoSymlink(result.relative(), Path.of(root).toAbsolutePath().normalize());
        return result;
    }

    /**
     * Throw if the media string is a data: URL.
     */
    public static void assertMediaNotDataUrl(String media) {
        if (media != null && DATA_URL.matcher(media.trim()).find()) {
            throw new IllegalArgumentException(
                    "data: URLs are not supported for media. Use buffer instead.");
        }
    }

    /**
     * Resolve a media source within a sandbox (HTTP URLs pass through).
     */
    public static String resolveSandboxedMediaSource(String media, String sandboxRoot)
            throws IOException {
        String raw = media == null ? "" : media.trim();
        if (raw.isEmpty()) {
            return raw;
        }
        if (HTTP_URL.matcher(raw).find()) {
            return raw;
        }
        String candidate = raw;
        if (FILE_URL.matcher(candidate).find()) {
            try {
                candidate = Paths.get(URI.create(candidate)).toString();
            } catch (Exception e) {
                throw new IllegalArgumentException(
                        "Invalid file:// URL for sandboxed media: " + raw, e);
            }
        }
        SandboxPathResult result = assertSandboxPath(candidate, sandboxRoot, sandboxRoot);
        return result.resolved();
    }

    // --- Internal helpers ---

    static String normalizeUnicodeSpaces(String str) {
        return UNICODE_SPACES.matcher(str).replaceAll(" ");
    }

    static String expandPath(String filePath) {
        String normalized = normalizeUnicodeSpaces(filePath);
        if ("~".equals(normalized)) {
            return System.getProperty("user.home");
        }
        if (normalized.startsWith("~/")) {
            return System.getProperty("user.home") + normalized.substring(1);
        }
        return normalized;
    }

    static String resolveToCwd(String filePath, String cwd) {
        String expanded = expandPath(filePath);
        Path p = Path.of(expanded);
        if (p.isAbsolute()) {
            return p.normalize().toString();
        }
        return Path.of(cwd).resolve(expanded).normalize().toString();
    }

    private static void assertNoSymlink(String relative, Path root) throws IOException {
        if (relative == null || relative.isEmpty()) {
            return;
        }
        Path current = root;
        for (Path part : Path.of(relative)) {
            current = current.resolve(part);
            if (Files.exists(current, LinkOption.NOFOLLOW_LINKS)) {
                if (Files.isSymbolicLink(current)) {
                    throw new IOException("Symlink not allowed in sandbox path: " + current);
                }
            } else {
                return; // doesn't exist yet, stop checking
            }
        }
    }

    private static String shortPath(String value) {
        String home = System.getProperty("user.home");
        if (value.startsWith(home)) {
            return "~" + value.substring(home.length());
        }
        return value;
    }
}

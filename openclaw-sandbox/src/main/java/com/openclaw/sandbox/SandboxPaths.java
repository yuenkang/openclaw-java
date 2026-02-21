package com.openclaw.sandbox;

import java.io.IOException;
import java.nio.file.*;
import java.util.regex.Pattern;

/**
 * Sandbox path resolution and security — ensures paths don't escape the sandbox
 * root
 * and blocks symlinks.
 * Corresponds to TypeScript sandbox-paths.ts.
 */
public final class SandboxPaths {

    private SandboxPaths() {
    }

    private static final Pattern UNICODE_SPACES = Pattern.compile("[\\u00A0\\u2000-\\u200A\\u202F\\u205F\\u3000]");
    private static final Pattern HTTP_URL_RE = Pattern.compile("^https?://", Pattern.CASE_INSENSITIVE);
    private static final Pattern DATA_URL_RE = Pattern.compile("^data:", Pattern.CASE_INSENSITIVE);

    /**
     * Result of resolving a sandbox path.
     */
    public record ResolvedPath(String resolved, String relative) {
    }

    /**
     * Resolve a file path within the sandbox root, validating it doesn't escape.
     */
    public static ResolvedPath resolveSandboxPath(String filePath, String cwd, String root)
            throws SandboxPathEscapeException {
        String resolved = resolveToCwd(filePath, cwd);
        String rootResolved = Path.of(root).toAbsolutePath().normalize().toString();
        Path resolvedPath = Path.of(resolved).toAbsolutePath().normalize();
        Path rootPath = Path.of(rootResolved);

        try {
            Path relative = rootPath.relativize(resolvedPath);
            String relStr = relative.toString();

            if (relStr.isEmpty()) {
                return new ResolvedPath(resolved, "");
            }
            if (relStr.startsWith("..") || Path.of(relStr).isAbsolute()) {
                throw new SandboxPathEscapeException(
                        String.format("Path escapes sandbox root (%s): %s", shortPath(rootResolved), filePath));
            }
            return new ResolvedPath(resolvedPath.toString(), relStr);
        } catch (IllegalArgumentException e) {
            throw new SandboxPathEscapeException(
                    String.format("Path escapes sandbox root (%s): %s", shortPath(rootResolved), filePath));
        }
    }

    /**
     * Assert a path is within sandbox root and not a symlink.
     */
    public static ResolvedPath assertSandboxPath(String filePath, String cwd, String root)
            throws SandboxPathEscapeException, IOException {
        ResolvedPath resolved = resolveSandboxPath(filePath, cwd, root);
        assertNoSymlink(resolved.relative(), Path.of(root).toAbsolutePath().normalize().toString());
        return resolved;
    }

    /**
     * Assert media is not a data: URL.
     */
    public static void assertMediaNotDataUrl(String media) {
        if (media != null && DATA_URL_RE.matcher(media.trim()).find()) {
            throw new IllegalArgumentException("data: URLs are not supported for media. Use buffer instead.");
        }
    }

    /**
     * Resolve a sandboxed media source — HTTP URLs pass through, file paths are
     * validated.
     */
    public static String resolveSandboxedMediaSource(String media, String sandboxRoot)
            throws SandboxPathEscapeException, IOException {
        String raw = media == null ? "" : media.trim();
        if (raw.isEmpty())
            return raw;
        if (HTTP_URL_RE.matcher(raw).find())
            return raw;

        String candidate = raw;
        if (candidate.toLowerCase().startsWith("file://")) {
            // Convert file URL to path
            try {
                candidate = java.net.URI.create(candidate).getPath();
            } catch (Exception e) {
                throw new IllegalArgumentException("Invalid file:// URL for sandboxed media: " + raw);
            }
        }

        ResolvedPath resolved = assertSandboxPath(candidate, sandboxRoot, sandboxRoot);
        return resolved.resolved();
    }

    // ── Helpers ─────────────────────────────────────────────────────

    private static String normalizeUnicodeSpaces(String str) {
        return UNICODE_SPACES.matcher(str).replaceAll(" ");
    }

    private static String expandPath(String filePath) {
        String normalized = normalizeUnicodeSpaces(filePath);
        if ("~".equals(normalized))
            return System.getProperty("user.home");
        if (normalized.startsWith("~/")) {
            return System.getProperty("user.home") + normalized.substring(1);
        }
        return normalized;
    }

    private static String resolveToCwd(String filePath, String cwd) {
        String expanded = expandPath(filePath);
        Path p = Path.of(expanded);
        if (p.isAbsolute())
            return p.normalize().toString();
        return Path.of(cwd).resolve(expanded).normalize().toString();
    }

    private static void assertNoSymlink(String relative, String root) throws IOException {
        if (relative == null || relative.isEmpty())
            return;
        String[] parts = relative.split(java.util.regex.Pattern.quote(java.io.File.separator));
        Path current = Path.of(root);
        for (String part : parts) {
            if (part.isEmpty())
                continue;
            current = current.resolve(part);
            if (Files.exists(current, LinkOption.NOFOLLOW_LINKS)) {
                if (Files.isSymbolicLink(current)) {
                    throw new IOException("Symlink not allowed in sandbox path: " + current);
                }
            } else {
                return; // Path doesn't exist yet, nothing else to check
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

    /**
     * Exception thrown when a path escapes the sandbox root.
     */
    public static class SandboxPathEscapeException extends RuntimeException {
        public SandboxPathEscapeException(String message) {
            super(message);
        }
    }
}

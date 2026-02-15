package com.openclaw.common.infra;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;

/**
 * Secure file open — opens a file only if it's within a given root directory,
 * blocking symlink escape and path traversal.
 * Corresponds to TypeScript's infra/fs-safe.ts.
 */
public final class FsSafe {

    private FsSafe() {
    }

    // ── Error ───────────────────────────────────────────────────────────

    public enum ErrorCode {
        INVALID_PATH, NOT_FOUND
    }

    public static class SafeOpenError extends RuntimeException {
        private final ErrorCode code;

        public SafeOpenError(ErrorCode code, String message) {
            super(message);
            this.code = code;
        }

        public ErrorCode getCode() {
            return code;
        }
    }

    // ── Result ──────────────────────────────────────────────────────────

    public record SafeOpenResult(Path realPath, long size) {
    }

    // ── API ─────────────────────────────────────────────────────────────

    /**
     * Open a file within a root directory, blocking symlinks and path traversal.
     *
     * @param rootDir      the trusted root directory
     * @param relativePath the relative path within root
     * @return the real path and file size
     * @throws SafeOpenError if the path is invalid or the file is not found
     */
    public static SafeOpenResult openFileWithinRoot(Path rootDir, String relativePath) {
        Path rootReal;
        try {
            rootReal = rootDir.toRealPath();
        } catch (IOException e) {
            throw new SafeOpenError(ErrorCode.NOT_FOUND, "root dir not found");
        }

        Path resolved = rootReal.resolve(relativePath).normalize();
        if (!resolved.startsWith(rootReal)) {
            throw new SafeOpenError(ErrorCode.INVALID_PATH, "path escapes root");
        }

        // Check if file exists (without following symlinks first)
        if (!Files.exists(resolved, LinkOption.NOFOLLOW_LINKS)) {
            throw new SafeOpenError(ErrorCode.NOT_FOUND, "file not found");
        }

        // Block symlinks
        if (Files.isSymbolicLink(resolved)) {
            throw new SafeOpenError(ErrorCode.INVALID_PATH, "symlink not allowed");
        }

        // Verify real path stays within root
        Path realPath;
        try {
            realPath = resolved.toRealPath();
        } catch (IOException e) {
            throw new SafeOpenError(ErrorCode.NOT_FOUND, "file not found");
        }

        if (!realPath.startsWith(rootReal)) {
            throw new SafeOpenError(ErrorCode.INVALID_PATH, "path escapes root");
        }

        if (!Files.isRegularFile(realPath)) {
            throw new SafeOpenError(ErrorCode.INVALID_PATH, "not a file");
        }

        try {
            long size = Files.size(realPath);
            return new SafeOpenResult(realPath, size);
        } catch (IOException e) {
            throw new SafeOpenError(ErrorCode.NOT_FOUND, "cannot stat file");
        }
    }

    /**
     * Read a file's contents securely within a root directory.
     */
    public static byte[] readFileWithinRoot(Path rootDir, String relativePath) {
        SafeOpenResult result = openFileWithinRoot(rootDir, relativePath);
        try {
            return Files.readAllBytes(result.realPath());
        } catch (IOException e) {
            throw new SafeOpenError(ErrorCode.NOT_FOUND, "cannot read file");
        }
    }

    /**
     * Open an input stream for a file securely within a root directory.
     */
    public static InputStream openStreamWithinRoot(Path rootDir, String relativePath) throws IOException {
        SafeOpenResult result = openFileWithinRoot(rootDir, relativePath);
        return Files.newInputStream(result.realPath());
    }
}

package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.file.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Archive extraction utilities for zip and tar archives.
 * <p>
 * Port of: infra/archive.ts
 */
@Slf4j
public class Archive {

    public enum ArchiveKind {
        TAR, ZIP
    }

    private static final String[] TAR_SUFFIXES = { ".tgz", ".tar.gz", ".tar" };

    /**
     * Determine the kind of archive from the file extension.
     */
    public static ArchiveKind resolveArchiveKind(String filePath) {
        String lower = filePath.toLowerCase();
        if (lower.endsWith(".zip")) {
            return ArchiveKind.ZIP;
        }
        for (String suffix : TAR_SUFFIXES) {
            if (lower.endsWith(suffix)) {
                return ArchiveKind.TAR;
            }
        }
        return null;
    }

    /**
     * Find the root directory inside an extracted archive.
     * Handles npm-style "package/" layout or single-dir layouts.
     */
    public static Path resolvePackedRootDir(Path extractDir) throws IOException {
        Path packageDir = extractDir.resolve("package");
        if (Files.isDirectory(packageDir)) {
            return packageDir;
        }
        try (var stream = Files.list(extractDir)) {
            var dirs = stream.filter(Files::isDirectory).toList();
            if (dirs.size() != 1) {
                throw new IOException("unexpected archive layout (dirs: " +
                        dirs.stream().map(d -> d.getFileName().toString())
                                .reduce((a, b) -> a + ", " + b).orElse("")
                        + ")");
            }
            return dirs.get(0);
        }
    }

    /**
     * Extract a zip file to the destination directory.
     */
    public static void extractZip(Path archivePath, Path destDir) throws IOException {
        Files.createDirectories(destDir);
        try (ZipInputStream zis = new ZipInputStream(
                new BufferedInputStream(Files.newInputStream(archivePath)))) {
            ZipEntry entry;
            while ((entry = zis.getNextEntry()) != null) {
                String name = entry.getName().replace('\\', '/');
                Path outPath = destDir.resolve(name).normalize();

                // Security: prevent zip-slip
                if (!outPath.startsWith(destDir)) {
                    throw new IOException("zip entry escapes destination: " + entry.getName());
                }

                if (entry.isDirectory()) {
                    Files.createDirectories(outPath);
                } else {
                    Files.createDirectories(outPath.getParent());
                    try (OutputStream out = Files.newOutputStream(outPath)) {
                        zis.transferTo(out);
                    }
                }
                zis.closeEntry();
            }
        }
    }

    /**
     * Extract a tar (.tar, .tar.gz, .tgz) file using the system's tar command.
     * Falls back to ProcessBuilder since Java stdlib doesn't include tar support.
     */
    public static void extractTar(Path archivePath, Path destDir) throws IOException {
        Files.createDirectories(destDir);
        String fileName = archivePath.getFileName().toString().toLowerCase();
        boolean gzipped = fileName.endsWith(".gz") || fileName.endsWith(".tgz");

        try {
            ProcessBuilder pb;
            if (gzipped) {
                pb = new ProcessBuilder("tar", "xzf",
                        archivePath.toAbsolutePath().toString(),
                        "-C", destDir.toAbsolutePath().toString());
            } else {
                pb = new ProcessBuilder("tar", "xf",
                        archivePath.toAbsolutePath().toString(),
                        "-C", destDir.toAbsolutePath().toString());
            }
            pb.redirectErrorStream(true);
            Process process = pb.start();
            int exitCode = process.waitFor();
            if (exitCode != 0) {
                try (BufferedReader reader = new BufferedReader(
                        new InputStreamReader(process.getInputStream()))) {
                    String error = reader.lines().reduce("", (a, b) -> a + "\n" + b);
                    throw new IOException("tar extraction failed (exit " + exitCode + "): " + error);
                }
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("tar extraction interrupted", e);
        }
    }

    /**
     * Extract an archive (auto-detect format) to the destination directory.
     */
    public static void extractArchive(Path archivePath, Path destDir) throws IOException {
        ArchiveKind kind = resolveArchiveKind(archivePath.toString());
        if (kind == null) {
            throw new IOException("unsupported archive: " + archivePath);
        }
        if (kind == ArchiveKind.TAR) {
            extractTar(archivePath, destDir);
        } else {
            extractZip(archivePath, destDir);
        }
    }
}

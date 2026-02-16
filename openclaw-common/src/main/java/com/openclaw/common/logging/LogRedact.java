package com.openclaw.common.logging;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Sensitive information redaction for log output and tool details.
 * Translates TS logging/redact.ts — pattern-based token masking.
 *
 */
public final class LogRedact {

    private LogRedact() {
    }

    // -----------------------------------------------------------------------
    // Modes
    // -----------------------------------------------------------------------

    public enum RedactMode {
        OFF,
        TOOLS;

        public static RedactMode normalize(String value) {
            if ("off".equalsIgnoreCase(value)) {
                return OFF;
            }
            return TOOLS;
        }
    }

    // -----------------------------------------------------------------------
    // Constants
    // -----------------------------------------------------------------------

    private static final int MIN_TOKEN_LENGTH = 18;
    private static final int KEEP_START = 6;
    private static final int KEEP_END = 4;

    // -----------------------------------------------------------------------
    // Default patterns — matching TS DEFAULT_REDACT_PATTERNS
    // -----------------------------------------------------------------------

    private static final List<String> DEFAULT_PATTERN_SOURCES = List.of(
            // ENV-style assignments
            "\\b[A-Z0-9_]*(?:KEY|TOKEN|SECRET|PASSWORD|PASSWD)\\b\\s*[=:]\\s*([\"']?)([^\\s\"'\\\\]+)\\1",
            // JSON fields
            "\"(?:apiKey|token|secret|password|passwd|accessToken|refreshToken)\"\\s*:\\s*\"([^\"]+)\"",
            // CLI flags
            "--(?:api[-_]?key|token|secret|password|passwd)\\s+([\"']?)([^\\s\"']+)\\1",
            // Authorization headers
            "Authorization\\s*[:=]\\s*Bearer\\s+([A-Za-z0-9._\\-+=]+)",
            "\\bBearer\\s+([A-Za-z0-9._\\-+=]{18,})\\b",
            // PEM blocks
            "-----BEGIN [A-Z ]*PRIVATE KEY-----[\\s\\S]+?-----END [A-Z ]*PRIVATE KEY-----",
            // Common token prefixes
            "\\b(sk-[A-Za-z0-9_-]{8,})\\b",
            "\\b(ghp_[A-Za-z0-9]{20,})\\b",
            "\\b(github_pat_[A-Za-z0-9_]{20,})\\b",
            "\\b(xox[baprs]-[A-Za-z0-9-]{10,})\\b",
            "\\b(xapp-[A-Za-z0-9-]{10,})\\b",
            "\\b(gsk_[A-Za-z0-9_-]{10,})\\b",
            "\\b(AIza[0-9A-Za-z\\-_]{20,})\\b",
            "\\b(pplx-[A-Za-z0-9_-]{10,})\\b",
            "\\b(npm_[A-Za-z0-9]{10,})\\b",
            "\\b(\\d{6,}:[A-Za-z0-9_-]{20,})\\b");

    private static final List<Pattern> DEFAULT_PATTERNS;
    static {
        List<Pattern> compiled = new ArrayList<>();
        for (String src : DEFAULT_PATTERN_SOURCES) {
            try {
                compiled.add(Pattern.compile(src, Pattern.CASE_INSENSITIVE));
            } catch (Exception ignored) {
                // skip invalid patterns
            }
        }
        DEFAULT_PATTERNS = Collections.unmodifiableList(compiled);
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Redact sensitive tokens in text using default patterns.
     */
    public static String redactSensitiveText(String text) {
        return redactSensitiveText(text, RedactMode.TOOLS, null);
    }

    /**
     * Redact sensitive tokens with configurable mode and custom patterns.
     *
     * @param text     the input text
     * @param mode     redaction mode (OFF disables redaction)
     * @param patterns custom patterns (null = use defaults)
     */
    public static String redactSensitiveText(String text, RedactMode mode, List<Pattern> patterns) {
        if (text == null || text.isEmpty()) {
            return text;
        }
        if (mode == RedactMode.OFF) {
            return text;
        }
        List<Pattern> effectivePatterns = (patterns != null && !patterns.isEmpty())
                ? patterns
                : DEFAULT_PATTERNS;
        if (effectivePatterns.isEmpty()) {
            return text;
        }
        return redactText(text, effectivePatterns);
    }

    /**
     * Redact tool output details (only applies when mode is TOOLS).
     */
    public static String redactToolDetail(String detail, RedactMode mode) {
        if (mode != RedactMode.TOOLS) {
            return detail;
        }
        return redactSensitiveText(detail, mode, null);
    }

    /**
     * Mask a single token, preserving start/end characters.
     */
    public static String maskToken(String token) {
        if (token.length() < MIN_TOKEN_LENGTH) {
            return "***";
        }
        String start = token.substring(0, KEEP_START);
        String end = token.substring(token.length() - KEEP_END);
        return start + "…" + end;
    }

    /**
     * Returns a copy of the default pattern source strings.
     */
    public static List<String> getDefaultRedactPatterns() {
        return new ArrayList<>(DEFAULT_PATTERN_SOURCES);
    }

    /**
     * Parse a pattern string into a compiled regex.
     * Supports /pattern/flags syntax and plain regex strings.
     */
    public static Pattern parsePattern(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        Matcher slashForm = Pattern.compile("^/(.+)/([gimsuy]*)$").matcher(raw);
        try {
            if (slashForm.matches()) {
                String body = slashForm.group(1);
                String flags = slashForm.group(2);
                int javaFlags = flags.contains("i") ? Pattern.CASE_INSENSITIVE : 0;
                if (flags.contains("s"))
                    javaFlags |= Pattern.DOTALL;
                if (flags.contains("m"))
                    javaFlags |= Pattern.MULTILINE;
                return Pattern.compile(body, javaFlags);
            }
            return Pattern.compile(raw, Pattern.CASE_INSENSITIVE);
        } catch (Exception e) {
            return null;
        }
    }

    // -----------------------------------------------------------------------
    // Internals
    // -----------------------------------------------------------------------

    private static String redactText(String text, List<Pattern> patterns) {
        String result = text;
        for (Pattern pattern : patterns) {
            result = redactWithPattern(result, pattern);
        }
        return result;
    }

    private static String redactWithPattern(String text, Pattern pattern) {
        Matcher matcher = pattern.matcher(text);
        StringBuilder sb = new StringBuilder();
        while (matcher.find()) {
            String fullMatch = matcher.group(0);
            String replacement = redactMatch(fullMatch, matcher);
            matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement));
        }
        matcher.appendTail(sb);
        return sb.toString();
    }

    private static String redactMatch(String fullMatch, Matcher matcher) {
        if (fullMatch.contains("PRIVATE KEY-----")) {
            return redactPemBlock(fullMatch);
        }
        // Find the last non-null capture group
        String token = fullMatch;
        for (int i = matcher.groupCount(); i >= 1; i--) {
            String group = matcher.group(i);
            if (group != null && !group.isEmpty()) {
                token = group;
                break;
            }
        }
        String masked = maskToken(token);
        if (token.equals(fullMatch)) {
            return masked;
        }
        return fullMatch.replace(token, masked);
    }

    private static String redactPemBlock(String block) {
        String[] lines = block.split("\\r?\\n");
        if (lines.length < 2) {
            return "***";
        }
        return lines[0] + "\n…redacted…\n" + lines[lines.length - 1];
    }
}

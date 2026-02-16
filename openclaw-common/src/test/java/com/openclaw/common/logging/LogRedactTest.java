package com.openclaw.common.logging;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link LogRedact} — sensitive information redaction.
 */
class LogRedactTest {

    // -----------------------------------------------------------------------
    // maskToken
    // -----------------------------------------------------------------------

    @Test
    void maskToken_shortToken_returnsStars() {
        assertEquals("***", LogRedact.maskToken("short"));
        assertEquals("***", LogRedact.maskToken("12345678901234567")); // 17 chars
    }

    @Test
    void maskToken_longToken_preservesStartAndEnd() {
        String token = "sk-abcdefghijklmnopqrstuvwxyz";
        String masked = LogRedact.maskToken(token);
        assertTrue(masked.startsWith("sk-abc"));
        assertTrue(masked.endsWith("wxyz"));
        assertTrue(masked.contains("…"));
    }

    // -----------------------------------------------------------------------
    // API key patterns
    // -----------------------------------------------------------------------

    @Test
    void redact_skPrefix() {
        String text = "Using key sk-1234567890abcdefghijklmn for API call";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("sk-1234567890abcdefghijklmn"));
        assertTrue(result.contains("sk-123"));
    }

    @Test
    void redact_ghpToken() {
        String text = "Token ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZabcd";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZabcd"));
        assertTrue(result.contains("ghp_AB"));
    }

    @Test
    void redact_githubPat() {
        String text = "github_pat_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefgh";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("github_pat_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefgh"));
        assertTrue(result.contains("github"));
    }

    @Test
    void redact_slackToken() {
        String text = "xoxb-fake-000000-testtoken000";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("xoxb-fake-000000-testtoken000"));
    }

    @Test
    void redact_googleApiKey() {
        String text = "AIzaSyABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("AIzaSyABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        assertTrue(result.contains("AIzaSy"));
    }

    // -----------------------------------------------------------------------
    // Environment variable patterns
    // -----------------------------------------------------------------------

    @Test
    void redact_envAssignment() {
        String text = "OPENAI_API_KEY=sk-abc123def456ghi789jkl012mno";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("sk-abc123def456ghi789jkl012mno"));
        assertTrue(result.contains("OPENAI_API_KEY="));
    }

    // -----------------------------------------------------------------------
    // Bearer token
    // -----------------------------------------------------------------------

    @Test
    void redact_bearerToken() {
        String text = "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.payload.sig";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.payload.sig"));
    }

    // -----------------------------------------------------------------------
    // JSON field
    // -----------------------------------------------------------------------

    @Test
    void redact_jsonApiKey() {
        String text = "{\"apiKey\": \"my-super-secret-api-key-12345\"}";
        String result = LogRedact.redactSensitiveText(text);
        assertFalse(result.contains("my-super-secret-api-key-12345"));
    }

    // -----------------------------------------------------------------------
    // PEM block
    // -----------------------------------------------------------------------

    @Test
    void redact_pemBlock() {
        String text = "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBg...\nline2\n-----END PRIVATE KEY-----";
        String result = LogRedact.redactSensitiveText(text);
        assertTrue(result.contains("-----BEGIN PRIVATE KEY-----"));
        assertTrue(result.contains("-----END PRIVATE KEY-----"));
        assertTrue(result.contains("…redacted…"));
        assertFalse(result.contains("MIIEvQIBADANBg"));
    }

    // -----------------------------------------------------------------------
    // Mode: OFF
    // -----------------------------------------------------------------------

    @Test
    void redact_offMode_returnsOriginal() {
        String text = "sk-1234567890abcdefghijklmn";
        String result = LogRedact.redactSensitiveText(text, LogRedact.RedactMode.OFF, null);
        assertEquals(text, result);
    }

    // -----------------------------------------------------------------------
    // Null / empty
    // -----------------------------------------------------------------------

    @Test
    void redact_nullOrEmpty() {
        assertNull(LogRedact.redactSensitiveText(null));
        assertEquals("", LogRedact.redactSensitiveText(""));
    }

    @Test
    void redact_innocuousText_unchanged() {
        String text = "Hello, this is a normal log message with no secrets";
        assertEquals(text, LogRedact.redactSensitiveText(text));
    }

    // -----------------------------------------------------------------------
    // parsePattern
    // -----------------------------------------------------------------------

    @Test
    void parsePattern_slashForm() {
        var pattern = LogRedact.parsePattern("/foo(bar)/i");
        assertNotNull(pattern);
        assertTrue(pattern.matcher("FOOBAR").find());
    }

    @Test
    void parsePattern_plainRegex() {
        var pattern = LogRedact.parsePattern("secret");
        assertNotNull(pattern);
        assertTrue(pattern.matcher("my SECRET value").find());
    }

    @Test
    void parsePattern_invalidReturnsNull() {
        assertNull(LogRedact.parsePattern(""));
        assertNull(LogRedact.parsePattern(null));
        assertNull(LogRedact.parsePattern("/[invalid/"));
    }

    // -----------------------------------------------------------------------
    // LogLevel
    // -----------------------------------------------------------------------

    @Test
    void logLevel_normalize() {
        assertEquals(LogLevel.INFO, LogLevel.normalize(null));
        assertEquals(LogLevel.INFO, LogLevel.normalize(""));
        assertEquals(LogLevel.DEBUG, LogLevel.normalize("debug"));
        assertEquals(LogLevel.WARN, LogLevel.normalize("WARNING"));
        assertEquals(LogLevel.ERROR, LogLevel.normalize("error"));
        assertEquals(LogLevel.INFO, LogLevel.normalize("invalid"));
        assertEquals(LogLevel.DEBUG, LogLevel.normalize("invalid", LogLevel.DEBUG));
    }

    @Test
    void logLevel_priority() {
        assertTrue(LogLevel.FATAL.priority() < LogLevel.ERROR.priority());
        assertTrue(LogLevel.ERROR.priority() < LogLevel.WARN.priority());
        assertTrue(LogLevel.WARN.priority() < LogLevel.INFO.priority());
        assertTrue(LogLevel.INFO.priority() < LogLevel.DEBUG.priority());
    }

    @Test
    void logLevel_isEnabledFor() {
        assertTrue(LogLevel.ERROR.isEnabledFor(LogLevel.INFO));
        assertTrue(LogLevel.INFO.isEnabledFor(LogLevel.INFO));
        assertFalse(LogLevel.DEBUG.isEnabledFor(LogLevel.INFO));
        assertFalse(LogLevel.TRACE.isEnabledFor(LogLevel.SILENT));
    }
}

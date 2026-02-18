package com.openclaw.gateway.net;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link OriginChecker}.
 */
class OriginCheckerTest {

    // =========================================================================
    // Loopback host checks
    // =========================================================================

    @ParameterizedTest
    @ValueSource(strings = { "localhost", "127.0.0.1", "127.0.0.2", "::1" })
    void isLoopbackHost_recognizesLoopback(String host) {
        assertTrue(OriginChecker.isLoopbackHost(host));
    }

    @ParameterizedTest
    @ValueSource(strings = { "example.com", "192.168.1.1", "10.0.0.1", "" })
    void isLoopbackHost_rejectsNonLoopback(String host) {
        assertFalse(OriginChecker.isLoopbackHost(host));
    }

    @Test
    void isLoopbackHost_nullReturnsFlase() {
        assertFalse(OriginChecker.isLoopbackHost(null));
    }

    // =========================================================================
    // Origin parsing
    // =========================================================================

    @Test
    void parseOrigin_validHttps() {
        var parsed = OriginChecker.parseOrigin("https://example.com");
        assertNotNull(parsed);
        assertEquals("https://example.com", parsed.origin());
        assertEquals("example.com", parsed.host());
        assertEquals("example.com", parsed.hostname());
    }

    @Test
    void parseOrigin_withPort() {
        var parsed = OriginChecker.parseOrigin("http://localhost:3000");
        assertNotNull(parsed);
        assertEquals("http://localhost:3000", parsed.origin());
        assertEquals("localhost:3000", parsed.host());
        assertEquals("localhost", parsed.hostname());
    }

    @Test
    void parseOrigin_nullOrEmpty() {
        assertNull(OriginChecker.parseOrigin(null));
        assertNull(OriginChecker.parseOrigin(""));
        assertNull(OriginChecker.parseOrigin("  "));
        assertNull(OriginChecker.parseOrigin("null"));
    }

    @Test
    void parseOrigin_invalid() {
        assertNull(OriginChecker.parseOrigin("not-a-url"));
    }

    // =========================================================================
    // Host header normalization
    // =========================================================================

    @Test
    void normalizeHostHeader_trimsAndLowercases() {
        assertEquals("example.com", OriginChecker.normalizeHostHeader("  Example.COM  "));
    }

    @Test
    void normalizeHostHeader_null() {
        assertEquals("", OriginChecker.normalizeHostHeader(null));
    }

    @Test
    void resolveHostName_stripsPort() {
        assertEquals("example.com", OriginChecker.resolveHostName("example.com:8080"));
    }

    @Test
    void resolveHostName_ipv6() {
        assertEquals("::1", OriginChecker.resolveHostName("[::1]:8080"));
    }

    // =========================================================================
    // checkBrowserOrigin integration
    // =========================================================================

    @Test
    void checkBrowserOrigin_sameHost_allowed() {
        var result = OriginChecker.checkBrowserOrigin(
                "example.com", "http://example.com", null);
        assertInstanceOf(OriginChecker.OriginOk.class, result);
    }

    @Test
    void checkBrowserOrigin_differentHost_rejected() {
        var result = OriginChecker.checkBrowserOrigin(
                "example.com", "http://evil.com", null);
        assertInstanceOf(OriginChecker.OriginRejected.class, result);
    }

    @Test
    void checkBrowserOrigin_loopbackPair_allowed() {
        var result = OriginChecker.checkBrowserOrigin(
                "localhost:3000", "http://127.0.0.1:5173", null);
        assertInstanceOf(OriginChecker.OriginOk.class, result);
    }

    @Test
    void checkBrowserOrigin_allowList_matches() {
        var result = OriginChecker.checkBrowserOrigin(
                "api.example.com", "https://app.example.com",
                List.of("https://app.example.com"));
        assertInstanceOf(OriginChecker.OriginOk.class, result);
    }

    @Test
    void checkBrowserOrigin_allowList_noMatch() {
        var result = OriginChecker.checkBrowserOrigin(
                "api.example.com", "https://evil.com",
                List.of("https://app.example.com"));
        assertInstanceOf(OriginChecker.OriginRejected.class, result);
    }

    @Test
    void checkBrowserOrigin_missingOrigin_rejected() {
        var result = OriginChecker.checkBrowserOrigin(
                "example.com", null, null);
        assertInstanceOf(OriginChecker.OriginRejected.class, result);
    }
}

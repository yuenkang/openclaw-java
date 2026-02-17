package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;

import static org.junit.jupiter.api.Assertions.*;

class SsrfGuardTest {

    @Nested
    class PrivateIpDetection {
        @Test
        void loopbackIsPrivate() {
            assertTrue(SsrfGuard.isPrivateIpAddress("127.0.0.1"));
            assertTrue(SsrfGuard.isPrivateIpAddress("::1"));
        }

        @Test
        void rfc1918IsPrivate() {
            assertTrue(SsrfGuard.isPrivateIpAddress("10.0.0.1"));
            assertTrue(SsrfGuard.isPrivateIpAddress("172.16.0.1"));
            assertTrue(SsrfGuard.isPrivateIpAddress("192.168.1.1"));
        }

        @Test
        void publicIsNotPrivate() {
            assertFalse(SsrfGuard.isPrivateIpAddress("8.8.8.8"));
            assertFalse(SsrfGuard.isPrivateIpAddress("1.1.1.1"));
        }
    }

    @Nested
    class HostnameBlocking {
        @Test
        void localhostIsBlocked() {
            assertTrue(SsrfGuard.isBlockedHostname("localhost"));
            assertTrue(SsrfGuard.isBlockedHostname("LOCALHOST"));
        }

        @Test
        void metadataGoogleIsBlocked() {
            assertTrue(SsrfGuard.isBlockedHostname("metadata.google.internal"));
        }

        @Test
        void normalHostnameNotBlocked() {
            assertFalse(SsrfGuard.isBlockedHostname("example.com"));
        }
    }

    @Nested
    class HostnameNormalization {
        @Test
        void stripsTrailingDot() {
            assertEquals("example.com", SsrfGuard.normalizeHostname("example.com."));
        }

        @Test
        void stripsIpv6Brackets() {
            assertEquals("::1", SsrfGuard.normalizeHostname("[::1]"));
        }

        @Test
        void lowercases() {
            assertEquals("example.com", SsrfGuard.normalizeHostname("Example.COM"));
        }
    }

    @Nested
    class PolicyChecks {
        @Test
        void publicHostnameAllowed() {
            assertDoesNotThrow(() -> SsrfGuard.assertPublicHostname("google.com"));
        }

        @Test
        void localhostBlockedByDefault() {
            assertThrows(SsrfGuard.SsrfBlockedError.class,
                    () -> SsrfGuard.assertPublicHostname("localhost"));
        }

        @Test
        void localhostAllowedWithPolicy() {
            var policy = new SsrfGuard.SsrfPolicy(true, java.util.List.of());
            assertDoesNotThrow(() -> SsrfGuard.assertPublicHostname("localhost", policy));
        }

        @Test
        void allowedHostnamesBypassCheck() {
            var policy = new SsrfGuard.SsrfPolicy(false, java.util.List.of("localhost"));
            assertDoesNotThrow(() -> SsrfGuard.assertPublicHostname("localhost", policy));
        }
    }
}

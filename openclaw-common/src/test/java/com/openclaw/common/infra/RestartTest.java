package com.openclaw.common.infra;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RestartTest {

    @AfterEach
    void tearDown() {
        Restart.resetForTest();
    }

    @Test
    void authorizeAndConsume() {
        assertFalse(Restart.isAuthorized());
        assertFalse(Restart.consumeAuthorization());

        Restart.authorizeRestart();

        assertTrue(Restart.isAuthorized());
        assertTrue(Restart.consumeAuthorization());
        // Second consume should fail (already consumed)
        assertFalse(Restart.consumeAuthorization());
    }

    @Test
    void isAuthorized_doesNotConsume() {
        Restart.authorizeRestart();

        assertTrue(Restart.isAuthorized());
        assertTrue(Restart.isAuthorized()); // can check multiple times
        assertTrue(Restart.consumeAuthorization()); // still available for consume
    }

    @Test
    void resolveRestartMethod_returnsPlatformMethod() {
        Restart.RestartMethod method = Restart.resolveRestartMethod();
        assertNotNull(method);
        // On macOS this returns LAUNCHCTL, on Linux SYSTEMD
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac")) {
            assertEquals(Restart.RestartMethod.LAUNCHCTL, method);
        } else if (os.contains("linux")) {
            assertEquals(Restart.RestartMethod.SYSTEMD, method);
        }
    }
}

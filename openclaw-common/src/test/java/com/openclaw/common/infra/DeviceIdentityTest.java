package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;

import static org.junit.jupiter.api.Assertions.*;

class DeviceIdentityTest {

    @Nested
    class KeyGeneration {
        @Test
        void generateIdentity_createsValidIdentity() {
            var identity = DeviceIdentity.generateIdentity();
            assertNotNull(identity);
            assertNotNull(identity.getDeviceId());
            assertNotNull(identity.getPublicKeyBase64());
            assertNotNull(identity.getPrivateKeyBase64());
            assertEquals(64, identity.getDeviceId().length()); // SHA-256 hex = 64 chars
        }

        @Test
        void fingerprintIsConsistent() {
            var identity = DeviceIdentity.generateIdentity();
            assertNotNull(identity);
            String fp1 = DeviceIdentity.fingerprintPublicKey(identity.getPublicKeyBase64());
            String fp2 = DeviceIdentity.fingerprintPublicKey(identity.getPublicKeyBase64());
            assertEquals(fp1, fp2);
            assertEquals(identity.getDeviceId(), fp1);
        }
    }

    @Nested
    class SignAndVerify {
        @Test
        void signAndVerify_roundTrip() {
            var identity = DeviceIdentity.generateIdentity();
            assertNotNull(identity);
            String payload = "hello world";
            String signature = DeviceIdentity.signPayload(identity.getPrivateKeyBase64(), payload);
            assertNotNull(signature);
            assertTrue(DeviceIdentity.verifySignature(
                    identity.getPublicKeyBase64(), payload, signature));
        }

        @Test
        void verifyFails_withWrongPayload() {
            var identity = DeviceIdentity.generateIdentity();
            assertNotNull(identity);
            String signature = DeviceIdentity.signPayload(identity.getPrivateKeyBase64(), "original");
            assertNotNull(signature);
            assertFalse(DeviceIdentity.verifySignature(
                    identity.getPublicKeyBase64(), "tampered", signature));
        }

        @Test
        void verifyFails_withWrongKey() {
            var id1 = DeviceIdentity.generateIdentity();
            var id2 = DeviceIdentity.generateIdentity();
            assertNotNull(id1);
            assertNotNull(id2);
            String signature = DeviceIdentity.signPayload(id1.getPrivateKeyBase64(), "test");
            assertFalse(DeviceIdentity.verifySignature(
                    id2.getPublicKeyBase64(), "test", signature));
        }
    }
}

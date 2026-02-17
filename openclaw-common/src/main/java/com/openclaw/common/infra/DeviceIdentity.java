package com.openclaw.common.infra;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.security.*;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;
import java.util.Set;

/**
 * Manages a persistent device identity (Ed25519 keypair) stored on disk.
 * <p>
 * Identity is stored as a JSON file in ~/.openclaw/identity/device.json.
 * The deviceId is the SHA-256 hex fingerprint of the raw public key bytes.
 * <p>
 * Port of: infra/device-identity.ts
 */
@Slf4j
public class DeviceIdentity {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Path DEFAULT_DIR = Paths.get(System.getProperty("user.home"), ".openclaw", "identity");
    private static final Path DEFAULT_FILE = DEFAULT_DIR.resolve("device.json");

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Identity {
        private String deviceId;
        private String publicKeyBase64;
        private String privateKeyBase64;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    static class StoredIdentity {
        private int version;
        private String deviceId;
        private String publicKeyBase64;
        private String privateKeyBase64;
        private long createdAtMs;
    }

    /**
     * Load existing identity from disk or create a new one.
     */
    public static Identity loadOrCreate() {
        return loadOrCreate(DEFAULT_FILE);
    }

    public static Identity loadOrCreate(Path filePath) {
        // Try loading existing
        try {
            if (Files.exists(filePath)) {
                String raw = Files.readString(filePath, StandardCharsets.UTF_8);
                StoredIdentity stored = MAPPER.readValue(raw, StoredIdentity.class);
                if (stored.getVersion() == 1
                        && stored.getDeviceId() != null
                        && stored.getPublicKeyBase64() != null
                        && stored.getPrivateKeyBase64() != null) {

                    // Verify fingerprint
                    String derivedId = fingerprintPublicKey(stored.getPublicKeyBase64());
                    if (derivedId != null && !derivedId.equals(stored.getDeviceId())) {
                        stored.setDeviceId(derivedId);
                        writeIdentity(filePath, stored);
                    }

                    return Identity.builder()
                            .deviceId(stored.getDeviceId())
                            .publicKeyBase64(stored.getPublicKeyBase64())
                            .privateKeyBase64(stored.getPrivateKeyBase64())
                            .build();
                }
            }
        } catch (Exception e) {
            log.debug("Failed to load device identity, regenerating: {}", e.getMessage());
        }

        // Generate new identity
        Identity identity = generateIdentity();
        if (identity != null) {
            StoredIdentity stored = StoredIdentity.builder()
                    .version(1)
                    .deviceId(identity.getDeviceId())
                    .publicKeyBase64(identity.getPublicKeyBase64())
                    .privateKeyBase64(identity.getPrivateKeyBase64())
                    .createdAtMs(System.currentTimeMillis())
                    .build();
            writeIdentity(filePath, stored);
        }
        return identity;
    }

    /**
     * Generate a new Ed25519 identity with SHA-256 fingerprint.
     * Falls back to RSA-2048 on JDKs without EdDSA support.
     */
    public static Identity generateIdentity() {
        try {
            KeyPairGenerator gen;
            try {
                gen = KeyPairGenerator.getInstance("Ed25519");
            } catch (NoSuchAlgorithmException e) {
                // Fallback for older JDKs
                gen = KeyPairGenerator.getInstance("RSA");
                gen.initialize(2048);
            }
            KeyPair kp = gen.generateKeyPair();
            String pubBase64 = Base64.getEncoder().encodeToString(kp.getPublic().getEncoded());
            String privBase64 = Base64.getEncoder().encodeToString(kp.getPrivate().getEncoded());
            String deviceId = fingerprintPublicKey(pubBase64);
            return Identity.builder()
                    .deviceId(deviceId)
                    .publicKeyBase64(pubBase64)
                    .privateKeyBase64(privBase64)
                    .build();
        } catch (Exception e) {
            log.error("Failed to generate device identity: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Compute SHA-256 hex fingerprint of a public key.
     */
    public static String fingerprintPublicKey(String publicKeyBase64) {
        try {
            byte[] raw = Base64.getDecoder().decode(publicKeyBase64);
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(raw);
            StringBuilder hex = new StringBuilder();
            for (byte b : hash) {
                hex.append(String.format("%02x", b));
            }
            return hex.toString();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Sign a payload using the private key (Ed25519 or RSA).
     */
    public static String signPayload(String privateKeyBase64, String payload) {
        try {
            byte[] keyBytes = Base64.getDecoder().decode(privateKeyBase64);
            PKCS8EncodedKeySpec spec = new PKCS8EncodedKeySpec(keyBytes);

            String algorithm;
            PrivateKey privateKey;
            Signature sig;
            try {
                KeyFactory kf = KeyFactory.getInstance("Ed25519");
                privateKey = kf.generatePrivate(spec);
                algorithm = "Ed25519";
            } catch (Exception e) {
                KeyFactory kf = KeyFactory.getInstance("RSA");
                privateKey = kf.generatePrivate(spec);
                algorithm = "SHA256withRSA";
            }
            sig = Signature.getInstance(algorithm);
            sig.initSign(privateKey);
            sig.update(payload.getBytes(StandardCharsets.UTF_8));
            return base64UrlEncode(sig.sign());
        } catch (Exception e) {
            log.error("Failed to sign payload: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Verify a signature using the public key.
     */
    public static boolean verifySignature(String publicKeyBase64, String payload, String signatureBase64Url) {
        try {
            byte[] keyBytes = Base64.getDecoder().decode(publicKeyBase64);
            X509EncodedKeySpec spec = new X509EncodedKeySpec(keyBytes);

            String algorithm;
            PublicKey publicKey;
            try {
                KeyFactory kf = KeyFactory.getInstance("Ed25519");
                publicKey = kf.generatePublic(spec);
                algorithm = "Ed25519";
            } catch (Exception e) {
                KeyFactory kf = KeyFactory.getInstance("RSA");
                publicKey = kf.generatePublic(spec);
                algorithm = "SHA256withRSA";
            }
            Signature sig = Signature.getInstance(algorithm);
            sig.initVerify(publicKey);
            sig.update(payload.getBytes(StandardCharsets.UTF_8));
            return sig.verify(base64UrlDecode(signatureBase64Url));
        } catch (Exception e) {
            return false;
        }
    }

    // --- Helpers ---

    private static String base64UrlEncode(byte[] data) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(data);
    }

    private static byte[] base64UrlDecode(String input) {
        return Base64.getUrlDecoder().decode(input);
    }

    private static void writeIdentity(Path filePath, StoredIdentity stored) {
        try {
            Files.createDirectories(filePath.getParent());
            String json = MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(stored) + "\n";
            Files.writeString(filePath, json, StandardCharsets.UTF_8);
            try {
                Files.setPosixFilePermissions(filePath, Set.of(
                        PosixFilePermission.OWNER_READ,
                        PosixFilePermission.OWNER_WRITE));
            } catch (Exception e) {
                // best-effort (may not be POSIX)
            }
        } catch (IOException e) {
            log.warn("Failed to write device identity: {}", e.getMessage());
        }
    }
}

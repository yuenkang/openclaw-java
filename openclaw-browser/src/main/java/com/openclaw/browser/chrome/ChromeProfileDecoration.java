package com.openclaw.browser.chrome;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Chrome profile decoration — inject OpenClaw branding and extension config.
 * Corresponds to TypeScript's chrome.profile-decoration.ts.
 */
@Slf4j
public final class ChromeProfileDecoration {

    private static final ObjectMapper mapper = new ObjectMapper();
    private static final String OPENCLAW_PROFILE_KEY = "openclaw";

    private ChromeProfileDecoration() {
    }

    /**
     * Decorate a Chrome profile with OpenClaw configuration.
     * Sets the profile name and color in Chrome's Preferences file.
     */
    public static void decorateProfile(String userDataDir, String profileName, String color) {
        Path prefsPath = Path.of(userDataDir, "Default", "Preferences");
        if (!Files.exists(prefsPath)) {
            log.debug("Preferences file not found, skipping decoration: {}", prefsPath);
            return;
        }

        try {
            String content = Files.readString(prefsPath);
            ObjectNode prefs = content.isBlank()
                    ? mapper.createObjectNode()
                    : (ObjectNode) mapper.readTree(content);

            // Set profile name
            ObjectNode profileNode = prefs.has("profile")
                    ? (ObjectNode) prefs.get("profile")
                    : mapper.createObjectNode();
            profileNode.put("name", "OpenClaw — " + profileName);
            prefs.set("profile", profileNode);

            // Mark as OpenClaw-managed
            ObjectNode openclawNode = mapper.createObjectNode();
            openclawNode.put("decorated", true);
            openclawNode.put("profileName", profileName);
            openclawNode.put("color", color != null ? color.toUpperCase() : "#FF6B35");
            prefs.set(OPENCLAW_PROFILE_KEY, openclawNode);

            // Disable first-run prompts
            prefs.put("browser.has_seen_welcome_page", true);
            if (!prefs.has("browser")) {
                prefs.set("browser", mapper.createObjectNode());
            }

            Files.writeString(prefsPath, mapper.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(prefs));
            log.debug("Decorated profile: {}", prefsPath);
        } catch (IOException e) {
            log.warn("Failed to decorate profile: {}", e.getMessage());
        }
    }

    /**
     * Ensure Chrome doesn't show crash-recovery bubble by clearing exit state.
     */
    public static void ensureCleanExit(String userDataDir) {
        Path prefsPath = Path.of(userDataDir, "Default", "Preferences");
        if (!Files.exists(prefsPath)) return;

        try {
            String content = Files.readString(prefsPath);
            ObjectNode prefs = (ObjectNode) mapper.readTree(content);

            // Remove crash state
            if (prefs.has("profile")) {
                ObjectNode profile = (ObjectNode) prefs.get("profile");
                profile.put("exit_type", "Normal");
                profile.put("exited_cleanly", true);
            }

            // Remove session restore state
            if (prefs.has("session")) {
                ObjectNode session = (ObjectNode) prefs.get("session");
                session.put("restore_on_startup", 5); // 5 = open new tab page
            }

            Files.writeString(prefsPath, mapper.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(prefs));
        } catch (IOException e) {
            log.warn("Failed to ensure clean exit: {}", e.getMessage());
        }
    }

    /**
     * Check if a Chrome profile has been decorated by OpenClaw.
     */
    public static boolean isProfileDecorated(String userDataDir, String profileName,
                                              String color) {
        Path prefsPath = Path.of(userDataDir, "Default", "Preferences");
        if (!Files.exists(prefsPath)) return false;

        try {
            String content = Files.readString(prefsPath);
            JsonNode prefs = mapper.readTree(content);
            JsonNode ocNode = prefs.get(OPENCLAW_PROFILE_KEY);
            if (ocNode == null || !ocNode.has("decorated")) return false;
            if (!ocNode.get("decorated").asBoolean()) return false;

            // Check if profile name and color match
            String storedName = ocNode.has("profileName") ? ocNode.get("profileName").asText() : "";
            String storedColor = ocNode.has("color") ? ocNode.get("color").asText() : "";

            return profileName.equals(storedName)
                    && (color == null || color.equalsIgnoreCase(storedColor));
        } catch (IOException e) {
            return false;
        }
    }
}

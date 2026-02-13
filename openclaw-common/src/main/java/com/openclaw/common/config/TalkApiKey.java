package com.openclaw.common.config;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * ElevenLabs (Talk) API key resolution.
 * Corresponds to TypeScript's talk.ts.
 */
public final class TalkApiKey {

    private TalkApiKey() {
    }

    private static final Pattern API_KEY_PATTERN = Pattern.compile(
            "(?:^|\\n)\\s*(?:export\\s+)?ELEVENLABS_API_KEY\\s*=\\s*[\"']?([^\\n\"']+)[\"']?");

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve the ElevenLabs API key from environment or profile files.
     */
    public static String resolveTalkApiKey() {
        String envValue = System.getenv("ELEVENLABS_API_KEY");
        if (envValue != null && !envValue.trim().isEmpty()) {
            return envValue.trim();
        }
        return readTalkApiKeyFromProfile();
    }

    /**
     * Read the API key from shell profile files (~/.profile, ~/.zprofile, ~/.zshrc,
     * ~/.bashrc).
     */
    public static String readTalkApiKeyFromProfile() {
        String home = System.getProperty("user.home");
        List<String> candidates = List.of(".profile", ".zprofile", ".zshrc", ".bashrc");
        for (String name : candidates) {
            Path candidate = Path.of(home, name);
            if (!Files.exists(candidate)) {
                continue;
            }
            try {
                String text = Files.readString(candidate);
                Matcher matcher = API_KEY_PATTERN.matcher(text);
                if (matcher.find()) {
                    String value = matcher.group(1);
                    if (value != null && !value.trim().isEmpty()) {
                        return value.trim();
                    }
                }
            } catch (IOException e) {
                // Ignore profile read errors.
            }
        }
        return null;
    }
}

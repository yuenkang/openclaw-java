package com.openclaw.agent.embedded;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.*;
import java.util.function.Consumer;
import java.util.regex.Pattern;

/**
 * Bootstrap content processing: session header creation, context file trimming,
 * thought-signature stripping, and Google turn ordering.
 * Mirrors {@code agents/pi-embedded-helpers/bootstrap.ts}.
 */
public final class BootstrapHelpers {

    private BootstrapHelpers() {
    }

    private static final ObjectMapper MAPPER = new ObjectMapper();
    public static final int DEFAULT_BOOTSTRAP_MAX_CHARS = 20_000;
    private static final double BOOTSTRAP_HEAD_RATIO = 0.7;
    private static final double BOOTSTRAP_TAIL_RATIO = 0.2;
    private static final Pattern BASE64_CHARS = Pattern.compile("^[A-Za-z0-9+/=_-]+$");

    // --- Thought signature stripping ---

    /**
     * Check if a string is a valid base64 signature.
     */
    public static boolean isBase64Signature(String value) {
        if (value == null)
            return false;
        String trimmed = value.trim();
        if (trimmed.isEmpty())
            return false;
        String compact = trimmed.replaceAll("\\s+", "");
        if (!BASE64_CHARS.matcher(compact).matches())
            return false;
        try {
            Base64.Decoder decoder = compact.contains("-") || compact.contains("_")
                    ? Base64.getUrlDecoder()
                    : Base64.getDecoder();
            byte[] buf = decoder.decode(compact);
            if (buf.length == 0)
                return false;
            Base64.Encoder encoder = compact.contains("-") || compact.contains("_")
                    ? Base64.getUrlEncoder().withoutPadding()
                    : Base64.getEncoder();
            String encoded = encoder.encodeToString(buf);
            String normA = encoded.replaceAll("=+$", "");
            String normB = compact.replaceAll("=+$", "");
            return normA.equals(normB);
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Strip Claude-style thought_signature fields from content blocks.
     * 
     * @param content          list of content block maps
     * @param allowBase64Only  if true, strip non-base64 signatures; if false, strip
     *                         msg_* only
     * @param includeCamelCase if true, also strip thoughtSignature (camelCase)
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> stripThoughtSignatures(
            List<Map<String, Object>> content, boolean allowBase64Only, boolean includeCamelCase) {
        if (content == null)
            return content;
        List<Map<String, Object>> result = new ArrayList<>(content.size());
        for (Map<String, Object> block : content) {
            if (block == null) {
                result.add(null);
                continue;
            }
            boolean stripSnake = shouldStripSignature(block.get("thought_signature"), allowBase64Only);
            boolean stripCamel = includeCamelCase
                    && shouldStripSignature(block.get("thoughtSignature"), allowBase64Only);
            if (!stripSnake && !stripCamel) {
                result.add(block);
                continue;
            }
            Map<String, Object> next = new LinkedHashMap<>(block);
            if (stripSnake)
                next.remove("thought_signature");
            if (stripCamel)
                next.remove("thoughtSignature");
            result.add(next);
        }
        return result;
    }

    private static boolean shouldStripSignature(Object value, boolean allowBase64Only) {
        if (!allowBase64Only) {
            return value instanceof String s && s.startsWith("msg_");
        }
        return !(value instanceof String s) || !isBase64Signature(s);
    }

    // --- Bootstrap content trimming ---

    public record TrimResult(String content, boolean truncated, int maxChars, int originalLength) {
    }

    /**
     * Resolve the bootstrap max chars from config, with fallback.
     */
    public static int resolveBootstrapMaxChars(Integer configured) {
        if (configured != null && configured > 0)
            return configured;
        return DEFAULT_BOOTSTRAP_MAX_CHARS;
    }

    /**
     * Trim bootstrap content to fit within maxChars, preserving head and tail.
     */
    public static TrimResult trimBootstrapContent(String content, String fileName, int maxChars) {
        String trimmed = content == null ? "" : content.stripTrailing();
        if (trimmed.length() <= maxChars) {
            return new TrimResult(trimmed, false, maxChars, trimmed.length());
        }
        int headChars = (int) Math.floor(maxChars * BOOTSTRAP_HEAD_RATIO);
        int tailChars = (int) Math.floor(maxChars * BOOTSTRAP_TAIL_RATIO);
        String head = trimmed.substring(0, headChars);
        String tail = trimmed.substring(trimmed.length() - tailChars);
        String marker = "\n[...truncated, read " + fileName + " for full content...]\n"
                + "…(truncated " + fileName + ": kept " + headChars + "+" + tailChars
                + " chars of " + trimmed.length() + ")…\n";
        return new TrimResult(head + marker + tail, true, maxChars, trimmed.length());
    }

    // --- Session header ---

    /**
     * Ensure the session file has a header entry (create if missing).
     */
    public static void ensureSessionHeader(String sessionFile, String sessionId, String cwd)
            throws IOException {
        Path path = Path.of(sessionFile);
        if (Files.exists(path))
            return;
        Path parent = path.getParent();
        if (parent != null)
            Files.createDirectories(parent);
        Map<String, Object> entry = new LinkedHashMap<>();
        entry.put("type", "session");
        entry.put("version", 2);
        entry.put("id", sessionId);
        entry.put("timestamp", Instant.now().toString());
        entry.put("cwd", cwd);
        Files.writeString(path, MAPPER.writeValueAsString(entry) + "\n");
    }

    // --- Bootstrap context file building ---

    /** Input for building bootstrap context files. */
    public record BootstrapFile(String name, String path, String content, boolean missing) {
    }

    /**
     * Build embedded context files from workspace bootstrap files.
     */
    public static List<EmbeddedTypes.EmbeddedContextFile> buildBootstrapContextFiles(
            List<BootstrapFile> files, Consumer<String> warn, int maxChars) {
        List<EmbeddedTypes.EmbeddedContextFile> result = new ArrayList<>();
        for (BootstrapFile file : files) {
            if (file.missing()) {
                result.add(new EmbeddedTypes.EmbeddedContextFile(
                        file.name(), "[MISSING] Expected at: " + file.path()));
                continue;
            }
            TrimResult trimmed = trimBootstrapContent(
                    file.content() != null ? file.content() : "", file.name(), maxChars);
            if (trimmed.content().isEmpty())
                continue;
            if (trimmed.truncated() && warn != null) {
                warn.accept("workspace bootstrap file " + file.name() + " is "
                        + trimmed.originalLength() + " chars (limit " + trimmed.maxChars()
                        + "); truncating in injected context");
            }
            result.add(new EmbeddedTypes.EmbeddedContextFile(file.name(), trimmed.content()));
        }
        return result;
    }

    // --- Google turn ordering ---

    private static final String GOOGLE_TURN_ORDER_BOOTSTRAP_TEXT = "(session bootstrap)";

    /**
     * Ensure the message list doesn't start with an assistant turn
     * (required by Cloud Code Assist / Gemini).
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> sanitizeGoogleTurnOrdering(
            List<Map<String, Object>> messages) {
        if (messages == null || messages.isEmpty())
            return messages;
        Map<String, Object> first = messages.get(0);
        if (first == null)
            return messages;
        String role = (String) first.get("role");
        Object content = first.get("content");
        if ("user".equals(role) && content instanceof String s
                && GOOGLE_TURN_ORDER_BOOTSTRAP_TEXT.equals(s.trim())) {
            return messages;
        }
        if (!"assistant".equals(role))
            return messages;

        Map<String, Object> bootstrap = new LinkedHashMap<>();
        bootstrap.put("role", "user");
        bootstrap.put("content", GOOGLE_TURN_ORDER_BOOTSTRAP_TEXT);
        bootstrap.put("timestamp", System.currentTimeMillis());
        List<Map<String, Object>> result = new ArrayList<>(messages.size() + 1);
        result.add(bootstrap);
        result.addAll(messages);
        return result;
    }
}

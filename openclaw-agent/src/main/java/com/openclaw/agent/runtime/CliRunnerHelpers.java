package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * CLI-runner helpers for managing external CLI agent backends.
 * Covers process cleanup, CLI-output parsing (JSON/JSONL), session ID
 * resolution,
 * prompt input routing, and args building.
 * Mirrors agents/cli-runner/helpers.ts.
 */
@Slf4j
public final class CliRunnerHelpers {

    private CliRunnerHelpers() {
    }

    // --- Run queue serializer ---

    private static final Map<String, CompletableFuture<?>> CLI_RUN_QUEUE = new ConcurrentHashMap<>();

    @SuppressWarnings("unchecked")
    public static <T> CompletableFuture<T> enqueueCliRun(String key, Callable<T> task) {
        CompletableFuture<?> prior = CLI_RUN_QUEUE.getOrDefault(key, CompletableFuture.completedFuture(null));
        CompletableFuture<T> chained = prior
                .exceptionally(ex -> null)
                .thenCompose(ignored -> {
                    try {
                        return CompletableFuture.completedFuture(task.call());
                    } catch (Exception e) {
                        return CompletableFuture.failedFuture(e);
                    }
                });
        CLI_RUN_QUEUE.put(key, chained);
        chained.whenComplete((r, ex) -> CLI_RUN_QUEUE.remove(key, chained));
        return chained;
    }

    // --- CLI output types ---

    public record CliUsage(
            Integer input,
            Integer output,
            Integer cacheRead,
            Integer cacheWrite,
            Integer total) {
        public boolean isEmpty() {
            return input == null && output == null && cacheRead == null
                    && cacheWrite == null && total == null;
        }
    }

    public record CliOutput(String text, String sessionId, CliUsage usage) {
    }

    // --- Process cleanup ---

    private static String escapeRegex(String value) {
        return Pattern.quote(value);
    }

    /**
     * Kill processes matching the backend's resume args pattern for a given
     * session.
     * No-op on Windows.
     */
    public static void cleanupResumeProcesses(Map<String, Object> backend, String sessionId) {
        if (System.getProperty("os.name", "").toLowerCase().contains("win"))
            return;

        @SuppressWarnings("unchecked")
        List<String> resumeArgs = backend.get("resumeArgs") instanceof List<?> l
                ? l.stream().map(Object::toString).toList()
                : List.of();
        if (resumeArgs.isEmpty())
            return;
        if (resumeArgs.stream().noneMatch(a -> a.contains("{sessionId}")))
            return;

        String command = backend.get("command") instanceof String s ? s : "";
        String commandToken = Path.of(command).getFileName().toString().strip();
        if (commandToken.isEmpty())
            return;

        List<String> tokens = resumeArgs.stream()
                .map(a -> a.replace("{sessionId}", sessionId))
                .toList();
        String pattern = commandToken + ".*" + tokens.stream()
                .filter(s -> !s.isEmpty())
                .map(CliRunnerHelpers::escapeRegex)
                .collect(Collectors.joining(".*"));
        if (pattern.isEmpty())
            return;

        try {
            new ProcessBuilder("pkill", "-f", pattern)
                    .redirectErrorStream(true)
                    .start()
                    .waitFor(5, TimeUnit.SECONDS);
        } catch (Exception ignored) {
            // best effort
        }
    }

    // --- JSON/JSONL parsing ---

    @SuppressWarnings("unchecked")
    private static Integer pickInt(Map<String, Object> map, String... keys) {
        for (String key : keys) {
            Object v = map.get(key);
            if (v instanceof Number n && n.intValue() > 0)
                return n.intValue();
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private static CliUsage toUsage(Map<String, Object> raw) {
        Integer input = pickInt(raw, "input_tokens", "inputTokens");
        Integer output = pickInt(raw, "output_tokens", "outputTokens");
        Integer cacheRead = pickInt(raw, "cache_read_input_tokens", "cached_input_tokens", "cacheRead");
        Integer cacheWrite = pickInt(raw, "cache_write_input_tokens", "cacheWrite");
        Integer total = pickInt(raw, "total_tokens", "total");
        CliUsage usage = new CliUsage(input, output, cacheRead, cacheWrite, total);
        return usage.isEmpty() ? null : usage;
    }

    private static String collectText(Object value) {
        if (value == null)
            return "";
        if (value instanceof String s)
            return s;
        if (value instanceof List<?> list) {
            return list.stream().map(CliRunnerHelpers::collectText).collect(Collectors.joining());
        }
        if (value instanceof Map<?, ?> map) {
            if (map.get("text") instanceof String t)
                return t;
            if (map.get("content") instanceof String c)
                return c;
            if (map.get("content") instanceof List<?> cl)
                return collectText(cl);
            if (map.get("message") instanceof Map<?, ?> msg)
                return collectText(msg);
        }
        return "";
    }

    @SuppressWarnings("unchecked")
    private static String pickSessionId(Map<String, Object> parsed, Map<String, Object> backend) {
        List<String> fields = backend.get("sessionIdFields") instanceof List<?> l
                ? l.stream().map(Object::toString).toList()
                : List.of("session_id", "sessionId", "conversation_id", "conversationId");
        for (String field : fields) {
            if (parsed.get(field) instanceof String s && !s.isBlank())
                return s.strip();
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public static CliOutput parseCliJson(String raw, Map<String, Object> backend) {
        String trimmed = raw.strip();
        if (trimmed.isEmpty())
            return null;
        try {
            Object parsed = new com.fasterxml.jackson.databind.ObjectMapper()
                    .readValue(trimmed, Map.class);
            if (!(parsed instanceof Map<?, ?> map))
                return null;
            Map<String, Object> m = (Map<String, Object>) map;
            String sessionId = pickSessionId(m, backend);
            CliUsage usage = m.get("usage") instanceof Map<?, ?> u
                    ? toUsage((Map<String, Object>) u)
                    : null;
            String text = collectText(m.get("message"));
            if (text.isEmpty())
                text = collectText(m.get("content"));
            if (text.isEmpty())
                text = collectText(m.get("result"));
            if (text.isEmpty())
                text = collectText(m);
            return new CliOutput(text.strip(), sessionId, usage);
        } catch (Exception e) {
            return null;
        }
    }

    @SuppressWarnings("unchecked")
    public static CliOutput parseCliJsonl(String raw, Map<String, Object> backend) {
        String[] lines = raw.split("\\r?\\n");
        String sessionId = null;
        CliUsage usage = null;
        List<String> texts = new ArrayList<>();
        var mapper = new com.fasterxml.jackson.databind.ObjectMapper();

        for (String line : lines) {
            String trimmed = line.strip();
            if (trimmed.isEmpty())
                continue;
            try {
                Object parsed = mapper.readValue(trimmed, Map.class);
                if (!(parsed instanceof Map<?, ?> map))
                    continue;
                Map<String, Object> m = (Map<String, Object>) map;
                if (sessionId == null)
                    sessionId = pickSessionId(m, backend);
                if (sessionId == null && m.get("thread_id") instanceof String tid) {
                    sessionId = tid.strip();
                }
                if (m.get("usage") instanceof Map<?, ?> u) {
                    CliUsage parsed2 = toUsage((Map<String, Object>) u);
                    if (parsed2 != null)
                        usage = parsed2;
                }
                if (m.get("item") instanceof Map<?, ?> item) {
                    if (item.get("text") instanceof String t) {
                        String type = item.get("type") instanceof String s ? s.toLowerCase() : "";
                        if (type.isEmpty() || type.contains("message"))
                            texts.add(t);
                    }
                }
            } catch (Exception ignored) {
            }
        }

        String text = String.join("\n", texts).strip();
        return text.isEmpty() ? null : new CliOutput(text, sessionId, usage);
    }

    // --- Session & prompt ---

    public record SessionIdResult(String sessionId, boolean isNew) {
    }

    public static SessionIdResult resolveSessionIdToSend(Map<String, Object> backend, String cliSessionId) {
        String mode = backend.get("sessionMode") instanceof String s ? s : "always";
        String existing = cliSessionId != null ? cliSessionId.strip() : null;
        if (existing != null && existing.isEmpty())
            existing = null;

        return switch (mode) {
            case "none" -> new SessionIdResult(null, existing == null);
            case "existing" -> new SessionIdResult(existing, existing == null);
            default -> {
                if (existing != null)
                    yield new SessionIdResult(existing, false);
                yield new SessionIdResult(UUID.randomUUID().toString(), true);
            }
        };
    }

    public record PromptInput(String argsPrompt, String stdin) {
    }

    public static PromptInput resolvePromptInput(Map<String, Object> backend, String prompt) {
        String inputMode = backend.get("input") instanceof String s ? s : "arg";
        if ("stdin".equals(inputMode))
            return new PromptInput(null, prompt);
        Object maxObj = backend.get("maxPromptArgChars");
        if (maxObj instanceof Number n && prompt.length() > n.intValue()) {
            return new PromptInput(null, prompt);
        }
        return new PromptInput(prompt, null);
    }

    public static String normalizeCliModel(String modelId, Map<String, Object> backend) {
        String trimmed = modelId.strip();
        if (trimmed.isEmpty())
            return trimmed;
        @SuppressWarnings("unchecked")
        Map<String, String> aliases = backend.get("modelAliases") instanceof Map<?, ?> m
                ? (Map<String, String>) m
                : Map.of();
        String direct = aliases.get(trimmed);
        if (direct != null)
            return direct;
        String lower = trimmed.toLowerCase();
        String mapped = aliases.get(lower);
        return mapped != null ? mapped : trimmed;
    }

    // --- Image helpers ---

    public static String resolveImageExtension(String mimeType) {
        String norm = mimeType.toLowerCase();
        if (norm.contains("png"))
            return "png";
        if (norm.contains("jpeg") || norm.contains("jpg"))
            return "jpg";
        if (norm.contains("gif"))
            return "gif";
        if (norm.contains("webp"))
            return "webp";
        return "bin";
    }

    public static String appendImagePathsToPrompt(String prompt, List<String> paths) {
        if (paths == null || paths.isEmpty())
            return prompt;
        String trimmed = prompt.stripTrailing();
        String separator = trimmed.isEmpty() ? "" : "\n\n";
        return trimmed + separator + String.join("\n", paths);
    }

    /**
     * Write image data to temp files and return paths.
     * Returns a record with paths and a cleanup Runnable.
     */
    public record WriteImagesResult(List<String> paths, Runnable cleanup) {
    }

    public static WriteImagesResult writeCliImages(List<Map<String, Object>> images) throws IOException {
        Path tempDir = Files.createTempDirectory("openclaw-cli-images-");
        List<String> paths = new ArrayList<>();
        for (int i = 0; i < images.size(); i++) {
            Map<String, Object> image = images.get(i);
            String mimeType = (String) image.getOrDefault("mimeType", "image/png");
            String ext = resolveImageExtension(mimeType);
            Path filePath = tempDir.resolve("image-" + (i + 1) + "." + ext);
            byte[] data = Base64.getDecoder().decode((String) image.get("data"));
            Files.write(filePath, data);
            paths.add(filePath.toString());
        }
        Runnable cleanup = () -> {
            try {
                Files.walk(tempDir)
                        .sorted(Comparator.reverseOrder())
                        .forEach(p -> {
                            try {
                                Files.deleteIfExists(p);
                            } catch (IOException ignored) {
                            }
                        });
            } catch (IOException ignored) {
            }
        };
        return new WriteImagesResult(paths, cleanup);
    }

    // --- CLI args builder ---

    @SuppressWarnings("unchecked")
    public static List<String> buildCliArgs(
            Map<String, Object> backend,
            List<String> baseArgs,
            String modelId,
            String sessionId,
            String systemPrompt,
            List<String> imagePaths,
            String promptArg,
            boolean useResume) {

        List<String> args = new ArrayList<>(baseArgs);

        if (!useResume && backend.get("modelArg") instanceof String ma && !modelId.isEmpty()) {
            args.add(ma);
            args.add(modelId);
        }
        if (!useResume && systemPrompt != null && backend.get("systemPromptArg") instanceof String spa) {
            args.add(spa);
            args.add(systemPrompt);
        }
        if (!useResume && sessionId != null) {
            if (backend.get("sessionArgs") instanceof List<?> sa && !sa.isEmpty()) {
                for (Object entry : sa) {
                    args.add(entry.toString().replace("{sessionId}", sessionId));
                }
            } else if (backend.get("sessionArg") instanceof String sarg) {
                args.add(sarg);
                args.add(sessionId);
            }
        }
        if (imagePaths != null && !imagePaths.isEmpty()) {
            String imageMode = backend.get("imageMode") instanceof String s ? s : "repeat";
            String imageArg = backend.get("imageArg") instanceof String s ? s : null;
            if (imageArg != null) {
                if ("list".equals(imageMode)) {
                    args.add(imageArg);
                    args.add(String.join(",", imagePaths));
                } else {
                    for (String path : imagePaths) {
                        args.add(imageArg);
                        args.add(path);
                    }
                }
            }
        }
        if (promptArg != null)
            args.add(promptArg);
        return args;
    }

    // --- System prompt resolution ---

    public static String resolveSystemPromptUsage(Map<String, Object> backend,
            boolean isNewSession, String systemPrompt) {
        if (systemPrompt == null || systemPrompt.isBlank())
            return null;
        String when = backend.get("systemPromptWhen") instanceof String s ? s : "first";
        if ("never".equals(when))
            return null;
        if ("first".equals(when) && !isNewSession)
            return null;
        if (!(backend.get("systemPromptArg") instanceof String spa) || spa.isBlank())
            return null;
        return systemPrompt.strip();
    }
}

package com.openclaw.agent.autoreply.reply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Stage inbound media into the sandbox workspace — copy files from the
 * media directory into the session sandbox, rename on collision, and
 * rewrite context paths.
 * Mirrors {@code auto-reply/reply/stage-sandbox-media.ts}.
 */
public final class StageSandboxMedia {

    private static final Logger log = LoggerFactory.getLogger(StageSandboxMedia.class);

    private StageSandboxMedia() {
    }

    /**
     * Stage inbound media files into the sandbox workspace.
     *
     * @param ctx          mutable context map
     * @param sessionCtx   mutable session context map
     * @param cfg          agent config
     * @param sessionKey   current session key
     * @param workspaceDir workspace root directory
     */
    @SuppressWarnings("unchecked")
    public static CompletableFuture<Void> stageSandboxMedia(
            Map<String, Object> ctx,
            Map<String, Object> sessionCtx,
            Map<String, Object> cfg,
            String sessionKey,
            String workspaceDir) {

        if (sessionKey == null || sessionKey.isEmpty()) {
            return CompletableFuture.completedFuture(null);
        }

        // Resolve raw paths
        List<String> rawPaths = new ArrayList<>();
        Object mediaPaths = ctx.get("MediaPaths");
        if (mediaPaths instanceof List<?> list && !list.isEmpty()) {
            for (Object item : list) {
                if (item instanceof String s)
                    rawPaths.add(s);
            }
        } else {
            Object mediaPath = ctx.get("MediaPath");
            if (mediaPath instanceof String s && !s.trim().isEmpty()) {
                rawPaths.add(s.trim());
            }
        }
        if (rawPaths.isEmpty()) {
            return CompletableFuture.completedFuture(null);
        }

        return CompletableFuture.runAsync(() -> {
            try {
                // Destination: <workspace>/media/inbound
                Path destDir = Path.of(workspaceDir, "media", "inbound");
                Files.createDirectories(destDir);

                Set<String> usedNames = new HashSet<>();
                Map<String, String> staged = new LinkedHashMap<>();

                for (String raw : rawPaths) {
                    String source = resolveAbsolutePath(raw);
                    if (source == null)
                        continue;
                    if (staged.containsKey(source))
                        continue;

                    Path srcPath = Path.of(source);
                    if (!Files.exists(srcPath))
                        continue;

                    String baseName = srcPath.getFileName().toString();
                    if (baseName.isEmpty())
                        continue;

                    String fileName = baseName;
                    int dotIdx = baseName.lastIndexOf('.');
                    String name = dotIdx > 0 ? baseName.substring(0, dotIdx) : baseName;
                    String ext = dotIdx > 0 ? baseName.substring(dotIdx) : "";
                    int suffix = 1;
                    while (usedNames.contains(fileName)) {
                        fileName = name + "-" + suffix + ext;
                        suffix++;
                    }
                    usedNames.add(fileName);

                    Path dest = destDir.resolve(fileName);
                    Files.copy(srcPath, dest, StandardCopyOption.REPLACE_EXISTING);

                    String stagedPath = "media/inbound/" + fileName;
                    staged.put(source, stagedPath);
                }

                // Rewrite context paths
                if (!staged.isEmpty()) {
                    List<String> nextPaths = rawPaths.stream()
                            .map(p -> {
                                String abs = resolveAbsolutePath(p);
                                return abs != null && staged.containsKey(abs)
                                        ? staged.get(abs)
                                        : p;
                            })
                            .toList();
                    ctx.put("MediaPaths", nextPaths);
                    sessionCtx.put("MediaPaths", nextPaths);
                    if (!nextPaths.isEmpty()) {
                        ctx.put("MediaPath", nextPaths.get(0));
                        sessionCtx.put("MediaPath", nextPaths.get(0));
                    }
                }
            } catch (IOException e) {
                log.warn("Failed to stage inbound media for sandbox: {}", e.getMessage());
            }
        });
    }

    private static String resolveAbsolutePath(String value) {
        if (value == null)
            return null;
        String resolved = value.trim();
        if (resolved.isEmpty())
            return null;
        if (resolved.startsWith("file://")) {
            resolved = resolved.substring(7);
        }
        if (!resolved.startsWith("/"))
            return null;
        return resolved;
    }
}

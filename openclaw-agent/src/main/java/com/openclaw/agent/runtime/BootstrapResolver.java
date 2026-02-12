package com.openclaw.agent.runtime;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Resolves bootstrap/context files from the workspace for agent initialization.
 * Reads .openclaw/ bootstrap files and builds embedded context for the system
 * prompt.
 * Corresponds to TypeScript's bootstrap-files.ts + bootstrap-hooks.ts.
 */
@Slf4j
public class BootstrapResolver {

    private static final String BOOTSTRAP_DIR = ".openclaw";
    private static final int DEFAULT_MAX_CHARS = 50_000;

    /**
     * A bootstrap file loaded from the workspace.
     */
    public record BootstrapFile(
            String relativePath,
            String content,
            boolean isDefault) {
    }

    /**
     * Embedded context file for injection into the system prompt.
     */
    public record EmbeddedContextFile(
            String path,
            String content) {
    }

    /**
     * Resolved bootstrap result.
     */
    public record BootstrapResult(
            List<BootstrapFile> bootstrapFiles,
            List<EmbeddedContextFile> contextFiles) {
    }

    /**
     * Load all bootstrap files from the workspace's .openclaw/ directory.
     * Reads .md and .txt files up to maxDepth 2.
     */
    public static List<BootstrapFile> loadWorkspaceBootstrapFiles(String workspaceDir) {
        Path bootstrapPath = Path.of(workspaceDir, BOOTSTRAP_DIR);
        if (!Files.isDirectory(bootstrapPath)) {
            return List.of();
        }

        List<BootstrapFile> files = new ArrayList<>();
        try (Stream<Path> walk = Files.walk(bootstrapPath, 2)) {
            walk.filter(Files::isRegularFile)
                    .filter(p -> {
                        String name = p.getFileName().toString().toLowerCase();
                        return name.endsWith(".md") || name.endsWith(".txt")
                                || name.endsWith(".yaml") || name.endsWith(".yml");
                    })
                    .forEach(p -> {
                        try {
                            String content = Files.readString(p, StandardCharsets.UTF_8);
                            String relative = bootstrapPath.relativize(p).toString();
                            files.add(new BootstrapFile(relative, content, false));
                        } catch (IOException e) {
                            log.warn("Failed to read bootstrap file {}: {}", p, e.getMessage());
                        }
                    });
        } catch (IOException e) {
            log.warn("Failed to scan bootstrap directory {}: {}", bootstrapPath, e.getMessage());
        }

        return files;
    }

    /**
     * Build embedded context files from bootstrap files, respecting maxChars limit.
     */
    public static List<EmbeddedContextFile> buildBootstrapContextFiles(
            List<BootstrapFile> bootstrapFiles, int maxChars) {
        if (bootstrapFiles == null || bootstrapFiles.isEmpty()) {
            return List.of();
        }

        int effectiveMax = maxChars > 0 ? maxChars : DEFAULT_MAX_CHARS;
        List<EmbeddedContextFile> result = new ArrayList<>();
        int totalChars = 0;

        for (BootstrapFile bf : bootstrapFiles) {
            String content = bf.content();
            if (content == null || content.isBlank())
                continue;

            if (totalChars + content.length() > effectiveMax) {
                // Truncate this file to fit
                int remaining = effectiveMax - totalChars;
                if (remaining > 100) {
                    content = content.substring(0, remaining) + "\n... (truncated)";
                    result.add(new EmbeddedContextFile(bf.relativePath(), content));
                }
                log.debug("Bootstrap context truncated at {} chars (max {})", totalChars, effectiveMax);
                break;
            }

            result.add(new EmbeddedContextFile(bf.relativePath(), content));
            totalChars += content.length();
        }

        return result;
    }

    /**
     * Full pipeline: load + build context.
     */
    public static BootstrapResult resolveBootstrapContext(String workspaceDir) {
        return resolveBootstrapContext(workspaceDir, DEFAULT_MAX_CHARS);
    }

    /**
     * Full pipeline with custom maxChars.
     */
    public static BootstrapResult resolveBootstrapContext(String workspaceDir, int maxChars) {
        List<BootstrapFile> bootstrapFiles = loadWorkspaceBootstrapFiles(workspaceDir);
        List<EmbeddedContextFile> contextFiles = buildBootstrapContextFiles(bootstrapFiles, maxChars);
        return new BootstrapResult(bootstrapFiles, contextFiles);
    }
}

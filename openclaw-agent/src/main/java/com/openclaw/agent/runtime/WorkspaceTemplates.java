package com.openclaw.agent.runtime;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Resolve workspace template directory for scaffold files.
 * Mirrors {@code agents/workspace-templates.ts}.
 */
public final class WorkspaceTemplates {

    private WorkspaceTemplates() {
    }

    private static volatile String cachedTemplateDir;

    /**
     * Resolve the workspace template directory by scanning known locations.
     *
     * @param cwd  current working directory (fallback search root)
     * @param root optional package/project root
     * @return path to the templates directory
     */
    public static String resolveWorkspaceTemplateDir(String cwd, String root) {
        String cached = cachedTemplateDir;
        if (cached != null) {
            return cached;
        }

        List<String> candidates = new ArrayList<>();
        if (root != null) {
            candidates.add(Path.of(root, "docs", "reference", "templates")
                    .normalize().toString());
        }
        if (cwd != null) {
            candidates.add(Path.of(cwd, "docs", "reference", "templates")
                    .normalize().toString());
        }
        // Fallback: relative to classloader
        candidates.add(Path.of(System.getProperty("user.dir"),
                "docs", "reference", "templates").normalize().toString());

        for (String candidate : candidates) {
            if (pathExists(candidate)) {
                cachedTemplateDir = candidate;
                return candidate;
            }
        }

        String fallback = candidates.isEmpty()
                ? Path.of(System.getProperty("user.dir"), "docs", "reference", "templates")
                        .normalize().toString()
                : candidates.get(0);
        cachedTemplateDir = fallback;
        return fallback;
    }

    /** Reset the cached template directory (useful for testing). */
    public static void resetCache() {
        cachedTemplateDir = null;
    }

    private static boolean pathExists(String path) {
        try {
            return Files.exists(Path.of(path));
        } catch (Exception e) {
            return false;
        }
    }
}

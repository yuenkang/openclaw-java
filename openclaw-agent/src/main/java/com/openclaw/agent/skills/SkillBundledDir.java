package com.openclaw.agent.skills;

import com.openclaw.agent.skills.SkillTypes.*;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;

/**
 * Bundled skills directory resolution and context loading.
 * Corresponds to TypeScript skills/bundled-dir.ts + bundled-context.ts.
 */
@Slf4j
public final class SkillBundledDir {

    private SkillBundledDir() {
    }

    private static boolean hasWarnedMissing = false;

    // ── Directory resolution ────────────────────────────────────────

    /**
     * Resolve the bundled skills directory.
     * Priority: env override → sibling of executable → module-relative search.
     */
    public static String resolveBundledSkillsDir() {
        // 1. Env override
        String override = System.getenv("OPENCLAW_BUNDLED_SKILLS_DIR");
        if (override != null && !override.isBlank()) {
            return override.trim();
        }

        // 2. JAR/executable sibling
        try {
            Path jarDir = resolveJarDir();
            if (jarDir != null) {
                Path sibling = jarDir.resolve("skills");
                if (Files.isDirectory(sibling)) {
                    return sibling.toString();
                }
            }
        } catch (Exception e) {
            // ignore
        }

        // 3. Classpath resource (for development)
        try {
            var resource = SkillBundledDir.class.getClassLoader().getResource("skills");
            if (resource != null) {
                Path resourcePath = Path.of(resource.toURI());
                if (Files.isDirectory(resourcePath) && looksLikeSkillsDir(resourcePath)) {
                    return resourcePath.toString();
                }
            }
        } catch (Exception e) {
            // ignore
        }

        // 4. Walk up from working directory
        try {
            Path current = Path.of(System.getProperty("user.dir", ".")).toAbsolutePath();
            for (int depth = 0; depth < 6; depth++) {
                Path candidate = current.resolve("skills");
                if (looksLikeSkillsDir(candidate)) {
                    return candidate.toString();
                }
                Path parent = current.getParent();
                if (parent == null || parent.equals(current))
                    break;
                current = parent;
            }
        } catch (Exception e) {
            // ignore
        }

        return null;
    }

    // ── Context ─────────────────────────────────────────────────────

    /**
     * Bundled skills context: directory path + set of skill names.
     */
    public record BundledSkillsContext(String dir, Set<String> names) {
    }

    /**
     * Resolve bundled skills context: load all bundled skills and collect names.
     */
    public static BundledSkillsContext resolveBundledSkillsContext() {
        String dir = resolveBundledSkillsDir();
        Set<String> names = new LinkedHashSet<>();

        if (dir == null) {
            if (!hasWarnedMissing) {
                hasWarnedMissing = true;
                log.warn("Bundled skills directory could not be resolved; built-in skills may be missing.");
            }
            return new BundledSkillsContext(null, names);
        }

        List<Skill> skills = SkillLoader.loadSkillsFromDir(Path.of(dir), SkillSource.BUNDLED);
        for (Skill skill : skills) {
            if (skill.name() != null && !skill.name().isBlank()) {
                names.add(skill.name());
            }
        }

        return new BundledSkillsContext(dir, names);
    }

    // ── Helpers ─────────────────────────────────────────────────────

    private static boolean looksLikeSkillsDir(Path dir) {
        if (!Files.isDirectory(dir))
            return false;
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
            for (Path entry : stream) {
                String name = entry.getFileName().toString();
                if (name.startsWith("."))
                    continue;
                if (Files.isRegularFile(entry) && name.endsWith(".md"))
                    return true;
                if (Files.isDirectory(entry)) {
                    if (Files.isRegularFile(entry.resolve("SKILL.md")))
                        return true;
                }
            }
        } catch (IOException e) {
            return false;
        }
        return false;
    }

    private static Path resolveJarDir() {
        try {
            var location = SkillBundledDir.class.getProtectionDomain()
                    .getCodeSource().getLocation();
            if (location != null) {
                return Path.of(location.toURI()).getParent();
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }
}

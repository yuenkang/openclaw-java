package com.openclaw.agent.skills;

import java.util.List;
import java.util.Map;

/**
 * Skill-related type definitions for the agent skill system.
 * Corresponds to TypeScript's skills/types.ts.
 */
public final class SkillTypes {

    private SkillTypes() {
    }

    // =========================================================================
    // Skill source
    // =========================================================================

    public enum SkillSource {
        BUNDLED("openclaw-bundled"),
        MANAGED("openclaw-managed"),
        WORKSPACE("openclaw-workspace"),
        PLUGIN("openclaw-plugin");

        private final String label;

        SkillSource(String label) {
            this.label = label;
        }

        public String label() {
            return label;
        }
    }

    // =========================================================================
    // Core Skill record
    // =========================================================================

    /**
     * A loaded skill definition.
     *
     * @param name        skill name (directory name)
     * @param description short description (from frontmatter)
     * @param source      where the skill was loaded from
     * @param filePath    absolute path to the SKILL.md file
     * @param baseDir     directory containing the skill
     * @param content     full content of the SKILL.md (body after frontmatter)
     */
    public record Skill(
            String name,
            String description,
            SkillSource source,
            String filePath,
            String baseDir,
            String content) {
    }

    // =========================================================================
    // Metadata (from frontmatter JSON block)
    // =========================================================================

    /**
     * Parsed OpenClaw-specific skill metadata.
     */
    public record SkillMetadata(
            Boolean always,
            String skillKey,
            String primaryEnv,
            String emoji,
            String homepage,
            List<String> os,
            SkillRequires requires) {
    }

    /**
     * Dependency requirements for a skill.
     */
    public record SkillRequires(
            List<String> bins,
            List<String> anyBins,
            List<String> env,
            List<String> config) {
    }

    // =========================================================================
    // Invocation policy
    // =========================================================================

    /**
     * Controls how/whether a skill can be invoked.
     *
     * @param userInvocable          whether users can invoke this skill directly
     * @param disableModelInvocation whether the model should NOT auto-invoke this
     *                               skill
     */
    public record SkillInvocationPolicy(
            boolean userInvocable,
            boolean disableModelInvocation) {
        public static final SkillInvocationPolicy DEFAULT = new SkillInvocationPolicy(true, false);
    }

    // =========================================================================
    // Skill entry (skill + parsed metadata)
    // =========================================================================

    /**
     * A fully resolved skill entry with frontmatter, metadata, and invocation
     * policy.
     */
    public record SkillEntry(
            Skill skill,
            Map<String, String> frontmatter,
            SkillMetadata metadata,
            SkillInvocationPolicy invocation) {
    }

    // =========================================================================
    // Skill snapshot (for prompt embedding)
    // =========================================================================

    /**
     * Snapshot of resolved skills for embedding in system prompt.
     */
    public record SkillSnapshot(
            String prompt,
            List<SkillSummary> skills,
            int version) {
    }

    /**
     * Minimal skill summary for snapshot.
     */
    public record SkillSummary(
            String name,
            String primaryEnv) {
    }
}

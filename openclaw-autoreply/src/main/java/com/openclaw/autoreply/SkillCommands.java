package com.openclaw.autoreply;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.*;

/**
 * Skill command resolution — list workspace/agent skill commands,
 * normalize skill command lookups, and resolve skill command invocations.
 * Mirrors {@code auto-reply/skill-commands.ts}.
 */
public final class SkillCommands {

    private static final Logger log = LoggerFactory.getLogger(SkillCommands.class);
    private static final Pattern SKILL_INVOCATION = Pattern.compile("^/([^\\s]+)(?:\\s+([\\s\\S]+))?$");

    private SkillCommands() {
    }

    /** A skill command specification. */
    public record SkillCommandSpec(
            String name,
            String skillName,
            String description,
            String toolDispatch,
            String skillDir) {
    }

    /** A resolved skill command invocation. */
    public record SkillCommandInvocation(
            SkillCommandSpec command,
            String args) {
    }

    /**
     * List skill commands for a workspace directory.
     */
    public static List<SkillCommandSpec> listSkillCommandsForWorkspace(
            String workspaceDir, Map<String, Object> cfg, List<String> skillFilter) {
        // Full buildWorkspaceSkillCommandSpecs integration deferred
        log.debug("Listing skill commands for workspace: {}", workspaceDir);
        return List.of();
    }

    /**
     * List skill commands for all agents.
     */
    public static List<SkillCommandSpec> listSkillCommandsForAgents(
            Map<String, Object> cfg, List<String> agentIds) {
        log.debug("Listing skill commands for agents");
        return List.of();
    }

    /**
     * Resolve a skill command invocation from the normalized command body.
     */
    public static SkillCommandInvocation resolveSkillCommandInvocation(
            String commandBodyNormalized, List<SkillCommandSpec> skillCommands) {
        String trimmed = commandBodyNormalized.trim();
        if (!trimmed.startsWith("/"))
            return null;

        Matcher m = SKILL_INVOCATION.matcher(trimmed);
        if (!m.matches())
            return null;

        String commandName = m.group(1).trim().toLowerCase();
        String remainder = m.group(2) != null ? m.group(2).trim() : null;

        // Handle /skill <name> [input]
        if ("skill".equals(commandName)) {
            if (remainder == null || remainder.isEmpty())
                return null;
            Matcher skillM = SKILL_INVOCATION.matcher("/" + remainder);
            if (!skillM.matches())
                return null;
            String skillName = skillM.group(1).trim();
            SkillCommandSpec cmd = findSkillCommand(skillCommands, skillName);
            if (cmd == null)
                return null;
            String args = skillM.group(2) != null ? skillM.group(2).trim() : null;
            return new SkillCommandInvocation(cmd, args);
        }

        SkillCommandSpec cmd = skillCommands.stream()
                .filter(c -> c.name().toLowerCase().equals(commandName))
                .findFirst().orElse(null);
        if (cmd == null)
            return null;
        return new SkillCommandInvocation(cmd, remainder);
    }

    private static SkillCommandSpec findSkillCommand(
            List<SkillCommandSpec> skillCommands, String rawName) {
        String trimmed = rawName.trim();
        if (trimmed.isEmpty())
            return null;
        String lowered = trimmed.toLowerCase();
        String normalized = normalizeSkillCommandLookup(trimmed);
        return skillCommands.stream()
                .filter(e -> e.name().toLowerCase().equals(lowered)
                        || e.skillName().toLowerCase().equals(lowered)
                        || normalizeSkillCommandLookup(e.name()).equals(normalized)
                        || normalizeSkillCommandLookup(e.skillName()).equals(normalized))
                .findFirst().orElse(null);
    }

    private static String normalizeSkillCommandLookup(String value) {
        return value.trim().toLowerCase().replaceAll("[\\s_]+", "-");
    }
}

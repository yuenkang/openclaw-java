package com.openclaw.agent.tools.policy;

import java.util.*;

/**
 * Shared tool policy types — allow/deny lists, tool profiles, plugin groups.
 * Corresponds to TypeScript tool-policy.ts (types + groups + profiles).
 */
public final class ToolPolicyTypes {

    private ToolPolicyTypes() {
    }

    /** A tool policy: allow and/or deny lists. */
    public record ToolPolicy(List<String> allow, List<String> deny) {
        public ToolPolicy {
            allow = allow != null ? List.copyOf(allow) : null;
            deny = deny != null ? List.copyOf(deny) : null;
        }

        public ToolPolicy() {
            this((List<String>) null, null);
        }
    }

    /** Tool profile IDs. */
    public enum ToolProfileId {
        minimal, coding, messaging, full
    }

    /** Plugin-contributed tool groups. */
    public record PluginToolGroups(
            List<String> all,
            Map<String, List<String>> byPlugin) {
    }

    /** Result of stripping plugin-only allowlists. */
    public record AllowlistResolution(
            ToolPolicy policy,
            List<String> unknownAllowlist,
            boolean strippedAllowlist) {
    }
}

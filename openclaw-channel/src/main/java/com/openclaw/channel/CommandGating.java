package com.openclaw.channel;

import java.util.List;

/**
 * Command authorization gating logic.
 * Corresponds to TypeScript's channels/command-gating.ts.
 */
public final class CommandGating {

    private CommandGating() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    public record CommandAuthorizer(boolean configured, boolean allowed) {
    }

    /** Mode when access groups are off: "allow", "deny", or "configured". */
    public static final String MODE_ALLOW = "allow";
    public static final String MODE_DENY = "deny";
    public static final String MODE_CONFIGURED = "configured";

    /** Result of control command gating. */
    public record ControlCommandGateResult(boolean commandAuthorized, boolean shouldBlock) {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve whether a command is authorized based on authorizers and access
     * groups.
     */
    public static boolean resolveCommandAuthorized(boolean useAccessGroups,
            List<CommandAuthorizer> authorizers,
            String modeWhenAccessGroupsOff) {
        String mode = modeWhenAccessGroupsOff != null ? modeWhenAccessGroupsOff : MODE_ALLOW;

        if (!useAccessGroups) {
            if (MODE_ALLOW.equals(mode)) {
                return true;
            }
            if (MODE_DENY.equals(mode)) {
                return false;
            }
            // "configured" mode
            boolean anyConfigured = authorizers.stream().anyMatch(CommandAuthorizer::configured);
            if (!anyConfigured) {
                return true;
            }
            return authorizers.stream().anyMatch(a -> a.configured() && a.allowed());
        }
        return authorizers.stream().anyMatch(a -> a.configured() && a.allowed());
    }

    /**
     * Resolve control command gating (combines authorization with text command
     * check).
     */
    public static ControlCommandGateResult resolveControlCommandGate(
            boolean useAccessGroups,
            List<CommandAuthorizer> authorizers,
            boolean allowTextCommands,
            boolean hasControlCommand,
            String modeWhenAccessGroupsOff) {
        boolean commandAuthorized = resolveCommandAuthorized(
                useAccessGroups, authorizers, modeWhenAccessGroupsOff);
        boolean shouldBlock = allowTextCommands && hasControlCommand && !commandAuthorized;
        return new ControlCommandGateResult(commandAuthorized, shouldBlock);
    }
}

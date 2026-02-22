package com.openclaw.node;

import com.openclaw.common.config.OpenClawConfig;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * Node command allowlisting policy — controls which commands a node
 * is permitted to execute, based on platform and config overrides.
 * Corresponds to TypeScript's node-command-policy.ts.
 */
@Slf4j
public class NodeCommandPolicy {

    // ==================== Command Groups ====================

    private static final List<String> CANVAS_COMMANDS = List.of(
            "canvas.present", "canvas.hide", "canvas.navigate",
            "canvas.eval", "canvas.snapshot",
            "canvas.a2ui.push", "canvas.a2ui.pushJSONL", "canvas.a2ui.reset"
    );

    private static final List<String> CAMERA_COMMANDS = List.of(
            "camera.list", "camera.snap", "camera.clip"
    );

    private static final List<String> SCREEN_COMMANDS = List.of("screen.record");

    private static final List<String> LOCATION_COMMANDS = List.of("location.get");

    private static final List<String> SMS_COMMANDS = List.of("sms.send");

    private static final List<String> SYSTEM_COMMANDS = List.of(
            "system.run", "system.which", "system.notify",
            "system.execApprovals.get", "system.execApprovals.set",
            "browser.proxy"
    );

    // ==================== Platform Defaults ====================

    private static final Map<String, List<String>> PLATFORM_DEFAULTS;

    static {
        Map<String, List<String>> m = new LinkedHashMap<>();

        m.put("ios", merge(CANVAS_COMMANDS, CAMERA_COMMANDS, SCREEN_COMMANDS, LOCATION_COMMANDS));
        m.put("android", merge(CANVAS_COMMANDS, CAMERA_COMMANDS, SCREEN_COMMANDS,
                LOCATION_COMMANDS, SMS_COMMANDS));
        m.put("macos", merge(CANVAS_COMMANDS, CAMERA_COMMANDS, SCREEN_COMMANDS,
                LOCATION_COMMANDS, SYSTEM_COMMANDS));
        m.put("linux", new ArrayList<>(SYSTEM_COMMANDS));
        m.put("windows", new ArrayList<>(SYSTEM_COMMANDS));
        m.put("unknown", merge(CANVAS_COMMANDS, CAMERA_COMMANDS, SCREEN_COMMANDS,
                LOCATION_COMMANDS, SMS_COMMANDS, SYSTEM_COMMANDS));

        PLATFORM_DEFAULTS = Collections.unmodifiableMap(m);
    }

    // ==================== Platform Normalization ====================

    /**
     * Normalize raw platform string / deviceFamily into a canonical platform ID.
     */
    public static String normalizePlatformId(String platform, String deviceFamily) {
        String raw = (platform != null ? platform.trim().toLowerCase() : "");

        if (raw.startsWith("ios")) return "ios";
        if (raw.startsWith("android")) return "android";
        if (raw.startsWith("mac") || raw.startsWith("darwin")) return "macos";
        if (raw.startsWith("win")) return "windows";
        if (raw.startsWith("linux")) return "linux";

        String family = (deviceFamily != null ? deviceFamily.trim().toLowerCase() : "");
        if (family.contains("iphone") || family.contains("ipad") || family.contains("ios")) return "ios";
        if (family.contains("android")) return "android";
        if (family.contains("mac")) return "macos";
        if (family.contains("windows")) return "windows";
        if (family.contains("linux")) return "linux";

        return "unknown";
    }

    // ==================== Allowlist Resolution ====================

    /**
     * Resolve the set of allowed commands for a given node, based on its
     * platform and the config's allow/deny overrides.
     */
    public static Set<String> resolveAllowlist(OpenClawConfig config,
                                                String platform,
                                                String deviceFamily) {
        String platformId = normalizePlatformId(platform, deviceFamily);
        List<String> base = PLATFORM_DEFAULTS.getOrDefault(platformId,
                PLATFORM_DEFAULTS.get("unknown"));

        // Merge config overrides
        List<String> extraAllow = Collections.emptyList();
        List<String> extraDeny = Collections.emptyList();
        if (config != null && config.getGateway() != null) {
            var nodes = config.getGateway().getNodes();
            if (nodes != null) {
                if (nodes.getAllowCommands() != null) {
                    extraAllow = nodes.getAllowCommands();
                }
                if (nodes.getDenyCommands() != null) {
                    extraDeny = nodes.getDenyCommands();
                }
            }
        }

        Set<String> allowed = new LinkedHashSet<>();
        for (String cmd : base) {
            String trimmed = cmd.trim();
            if (!trimmed.isEmpty()) allowed.add(trimmed);
        }
        for (String cmd : extraAllow) {
            String trimmed = cmd.trim();
            if (!trimmed.isEmpty()) allowed.add(trimmed);
        }

        // Remove denied commands
        Set<String> denied = new HashSet<>();
        for (String cmd : extraDeny) {
            String trimmed = cmd.trim();
            if (!trimmed.isEmpty()) denied.add(trimmed);
        }
        allowed.removeAll(denied);

        return Collections.unmodifiableSet(allowed);
    }

    // ==================== Policy Check ====================

    /**
     * Check result for {@link #isCommandAllowed}.
     */
    public record CommandCheckResult(boolean ok, String reason) {
        public static CommandCheckResult allowed() {
            return new CommandCheckResult(true, null);
        }
        public static CommandCheckResult denied(String reason) {
            return new CommandCheckResult(false, reason);
        }
    }

    /**
     * Check if a command is allowed for a node, given the resolved allowlist
     * and the commands the node declared at connect time.
     */
    public static CommandCheckResult isCommandAllowed(String command,
                                                       List<String> declaredCommands,
                                                       Set<String> allowlist) {
        String trimmed = (command != null ? command.trim() : "");
        if (trimmed.isEmpty()) {
            return CommandCheckResult.denied("command required");
        }
        if (!allowlist.contains(trimmed)) {
            return CommandCheckResult.denied("command not allowlisted");
        }
        if (declaredCommands != null && !declaredCommands.isEmpty()) {
            if (!declaredCommands.contains(trimmed)) {
                return CommandCheckResult.denied("command not declared by node");
            }
        } else {
            return CommandCheckResult.denied("node did not declare commands");
        }
        return CommandCheckResult.allowed();
    }

    // ==================== Helpers ====================

    @SafeVarargs
    private static List<String> merge(List<String>... lists) {
        List<String> result = new ArrayList<>();
        for (List<String> list : lists) {
            result.addAll(list);
        }
        return result;
    }
}

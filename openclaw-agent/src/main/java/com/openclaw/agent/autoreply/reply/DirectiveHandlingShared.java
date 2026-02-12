package com.openclaw.agent.autoreply.reply;

import java.util.ArrayList;
import java.util.List;

/**
 * Directive acknowledgement formatting, elevated/reasoning event text,
 * and elevated unavailability messages.
 * Mirrors {@code auto-reply/reply/directive-handling.shared.ts}.
 */
public final class DirectiveHandlingShared {

    private DirectiveHandlingShared() {
    }

    public static final String SYSTEM_MARK = "⚙️";

    /** Format text with a system mark prefix. */
    public static String formatDirectiveAck(String text) {
        if (text == null || text.isEmpty())
            return text;
        if (text.startsWith(SYSTEM_MARK))
            return text;
        return SYSTEM_MARK + " " + text;
    }

    public static String formatOptionsLine(String options) {
        return "Options: " + options + ".";
    }

    public static String withOptions(String line, String options) {
        return line + "\n" + formatOptionsLine(options);
    }

    public static String formatElevatedRuntimeHint() {
        return SYSTEM_MARK + " Runtime is direct; sandboxing does not apply.";
    }

    /** Format an elevated level change event line. */
    public static String formatElevatedEvent(String level) {
        if ("full".equals(level)) {
            return "Elevated FULL — exec runs on host with auto-approval.";
        }
        if ("ask".equals(level) || "on".equals(level)) {
            return "Elevated ASK — exec runs on host; approvals may still apply.";
        }
        return "Elevated OFF — exec stays in sandbox.";
    }

    /** Format a reasoning level change event line. */
    public static String formatReasoningEvent(String level) {
        if ("stream".equals(level)) {
            return "Reasoning STREAM — emit live <think>.";
        }
        if ("on".equals(level)) {
            return "Reasoning ON — include <think>.";
        }
        return "Reasoning OFF — hide <think>.";
    }

    /** Result of gate failures. */
    public record GateFailure(String gate, String key) {
    }

    /** Format elevated-unavailable text with failure details. */
    public static String formatElevatedUnavailableText(
            boolean runtimeSandboxed,
            List<GateFailure> failures,
            String sessionKey) {
        List<String> lines = new ArrayList<>();
        lines.add("elevated is not available right now (runtime=" +
                (runtimeSandboxed ? "sandboxed" : "direct") + ").");
        if (failures != null && !failures.isEmpty()) {
            String failureText = failures.stream()
                    .map(f -> f.gate() + " (" + f.key() + ")")
                    .reduce((a, b) -> a + ", " + b)
                    .orElse("");
            lines.add("Failing gates: " + failureText);
        } else {
            lines.add(
                    "Fix-it keys: tools.elevated.enabled, tools.elevated.allowFrom.<provider>, agents.list[].tools.elevated.*");
        }
        if (sessionKey != null && !sessionKey.isEmpty()) {
            lines.add("See: `openclaw sandbox explain --session " + sessionKey + "`");
        }
        return String.join("\n", lines);
    }
}

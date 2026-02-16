package com.openclaw.app.commands;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Approve command: /approve.
 * Mirrors TypeScript's {@code commands-approve.ts}.
 * Handles exec approval decisions (allow-once, allow-always, deny).
 */
@Slf4j
@Component
public class ApproveCommands {

    private static final Map<String, String> DECISION_ALIASES = Map.ofEntries(
            Map.entry("allow", "allow-once"),
            Map.entry("once", "allow-once"),
            Map.entry("allow-once", "allow-once"),
            Map.entry("allowonce", "allow-once"),
            Map.entry("always", "allow-always"),
            Map.entry("allow-always", "allow-always"),
            Map.entry("allowalways", "allow-always"),
            Map.entry("deny", "deny"),
            Map.entry("reject", "deny"),
            Map.entry("block", "deny"));

    public CommandResult handleApprove(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        if (args.isEmpty()) {
            return CommandResult.text("❌ 用法: `/approve <id> allow-once|allow-always|deny`");
        }

        String[] tokens = args.trim().split("\\s+");
        if (tokens.length < 2) {
            return CommandResult.text("❌ 用法: `/approve <id> allow-once|allow-always|deny`");
        }

        // Try both orderings: /approve <id> <decision> or /approve <decision> <id>
        String id;
        String decision;

        String firstLower = tokens[0].toLowerCase();
        String secondLower = tokens[1].toLowerCase();

        if (DECISION_ALIASES.containsKey(firstLower)) {
            decision = DECISION_ALIASES.get(firstLower);
            id = tokens[1];
        } else if (DECISION_ALIASES.containsKey(secondLower)) {
            decision = DECISION_ALIASES.get(secondLower);
            id = tokens[0];
        } else {
            return CommandResult.text("❌ 未知的审批决定。可选: allow-once, allow-always, deny");
        }

        log.info("Approval submitted: id={}, decision={}, session={}", id, decision, sessionKey);

        // TODO: Wire to gateway exec.approval.resolve when gateway client is available
        return CommandResult.text(String.format("✅ 审批已提交: `%s` → %s\n\n🚧 _此功能尚未完全实现，需接入 Gateway 审批系统_", id, decision));
    }
}

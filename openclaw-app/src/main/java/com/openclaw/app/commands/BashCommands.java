package com.openclaw.app.commands;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.TimeUnit;

/**
 * Bash command: /bash.
 * Mirrors TypeScript's {@code commands-bash.ts} + {@code bash-command.ts}.
 * Executes shell commands on the host machine.
 */
@Slf4j
@Component
public class BashCommands {

    private static final int DEFAULT_TIMEOUT_SECONDS = 30;
    private static final int MAX_OUTPUT_LENGTH = 4000;

    public CommandResult handleBash(String args, CommandContext ctx) {
        var sessionKey = ctx.sessionKey();
        if (args.isEmpty()) {
            return CommandResult.text("""
                    🖥️ *Bash 命令*

                    用法:
                    `/bash <command>` — 执行 shell 命令
                    `/bash help` — 显示此帮助

                    示例:
                    `/bash ls -la`
                    `/bash echo hello`
                    `/bash pwd`

                    ⚠️ 命令在服务器上执行，请谨慎使用。""");
        }

        if ("help".equalsIgnoreCase(args)) {
            return handleBash("", ctx);
        }

        log.info("Executing bash command for session {}: {}", sessionKey, args);

        try {
            ProcessBuilder pb = new ProcessBuilder("bash", "-c", args);
            pb.redirectErrorStream(true);
            pb.environment().put("OPENCLAW_SESSION", sessionKey);

            Process process = pb.start();

            StringBuilder output = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (output.length() > 0) {
                        output.append("\n");
                    }
                    output.append(line);
                    if (output.length() > MAX_OUTPUT_LENGTH) {
                        output.append("\n... (输出已截断)");
                        break;
                    }
                }
            }

            boolean finished = process.waitFor(DEFAULT_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return CommandResult.text(String.format("⏱️ 命令超时 (%ds)\n\n部分输出:\n```\n%s\n```",
                        DEFAULT_TIMEOUT_SECONDS, output));
            }

            int exitCode = process.exitValue();
            String outputStr = output.toString();
            if (outputStr.isEmpty()) {
                outputStr = "(无输出)";
            }

            if (exitCode == 0) {
                return CommandResult.text(String.format("```\n%s\n```", outputStr));
            } else {
                return CommandResult.text(String.format("⚠️ 退出码: %d\n```\n%s\n```", exitCode, outputStr));
            }

        } catch (Exception e) {
            log.error("Bash command failed: {}", e.getMessage(), e);
            return CommandResult.text("❌ 命令执行失败: " + e.getMessage());
        }
    }
}

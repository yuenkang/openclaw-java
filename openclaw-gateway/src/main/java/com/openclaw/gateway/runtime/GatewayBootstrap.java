package com.openclaw.gateway.runtime;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Gateway boot — loads and runs BOOT.md on startup.
 * Corresponds to TS {@code gateway/boot.ts}.
 *
 * <p>
 * BOOT.md is a workspace-level file that can trigger an agent run
 * at gateway startup (e.g., to send notifications, run health checks, etc.).
 */
@Slf4j
public class GatewayBootstrap {

    private static final String BOOT_FILENAME = "BOOT.md";

    /** Result of a boot run attempt. */
    public enum BootStatus {
        /** BOOT.md not found or empty — skipped. */
        SKIPPED,
        /** BOOT.md found and agent ran successfully. */
        RAN,
        /** BOOT.md found but agent run failed. */
        FAILED
    }

    public record BootResult(BootStatus status, String reason) {
        public static BootResult skipped(String reason) {
            return new BootResult(BootStatus.SKIPPED, reason);
        }

        public static BootResult ran() {
            return new BootResult(BootStatus.RAN, null);
        }

        public static BootResult failed(String reason) {
            return new BootResult(BootStatus.FAILED, reason);
        }
    }

    /**
     * Load the BOOT.md file from the workspace directory.
     *
     * @return the trimmed content, or null if missing/empty.
     */
    public static String loadBootFile(String workspaceDir) {
        Path bootPath = Path.of(workspaceDir, BOOT_FILENAME);
        if (!Files.exists(bootPath)) {
            log.debug("boot: {} not found in {}", BOOT_FILENAME, workspaceDir);
            return null;
        }
        try {
            String content = Files.readString(bootPath).trim();
            if (content.isEmpty()) {
                log.debug("boot: {} is empty", BOOT_FILENAME);
                return null;
            }
            return content;
        } catch (IOException e) {
            log.warn("boot: failed to read {}: {}", bootPath, e.getMessage());
            return null;
        }
    }

    /**
     * Build the boot prompt from BOOT.md content.
     */
    public static String buildBootPrompt(String bootContent) {
        return String.join("\n",
                "You are running a boot check. Follow BOOT.md instructions exactly.",
                "",
                "BOOT.md:",
                bootContent,
                "",
                "If BOOT.md asks you to send a message, use the message tool (action=send with channel + target).",
                "Use the `target` field (not `to`) for message tool destinations.",
                "After sending with the message tool, reply with ONLY: [SILENT].",
                "If nothing needs attention, reply with ONLY: [SILENT].");
    }

    /**
     * Execute the boot sequence: load BOOT.md, run agent if exists.
     *
     * <p>
     * Note: Actual agent invocation is deferred to the caller
     * (e.g., GatewayStartup) because it depends on the full agent
     * runtime which is outside the gateway module.
     *
     * @param workspaceDir workspace directory path
     * @return BootResult with prompt content (if RAN) or skip/fail reason
     */
    public static BootResult tryLoadBoot(String workspaceDir) {
        try {
            String content = loadBootFile(workspaceDir);
            if (content == null) {
                return BootResult.skipped("missing or empty");
            }
            log.info("boot: found {} ({} chars), preparing agent run",
                    BOOT_FILENAME, content.length());
            return BootResult.ran();
        } catch (Exception e) {
            String msg = e.getMessage() != null ? e.getMessage() : e.toString();
            log.error("boot: failed: {}", msg);
            return BootResult.failed(msg);
        }
    }
}

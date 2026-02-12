package com.openclaw.agent.sandbox;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * Default constants for sandbox configuration.
 * Corresponds to TypeScript sandbox/constants.ts.
 */
public final class SandboxConstants {

    private SandboxConstants() {
    }

    // ── Workspace ───────────────────────────────────────────────────

    public static final String DEFAULT_SANDBOX_WORKSPACE_ROOT = Paths
            .get(System.getProperty("user.home"), ".openclaw", "sandboxes").toString();

    // ── Docker ──────────────────────────────────────────────────────

    public static final String DEFAULT_SANDBOX_IMAGE = "openclaw-sandbox:bookworm-slim";
    public static final String DEFAULT_SANDBOX_CONTAINER_PREFIX = "openclaw-sbx-";
    public static final String DEFAULT_SANDBOX_WORKDIR = "/workspace";
    public static final int DEFAULT_SANDBOX_IDLE_HOURS = 24;
    public static final int DEFAULT_SANDBOX_MAX_AGE_DAYS = 7;

    // ── Tool allow/deny defaults ────────────────────────────────────

    public static final List<String> DEFAULT_TOOL_ALLOW = List.of(
            "exec", "process", "read", "write", "edit", "apply_patch",
            "image", "sessions_list", "sessions_history", "sessions_send",
            "sessions_spawn", "session_status");

    public static final List<String> DEFAULT_TOOL_DENY = List.of(
            "browser", "canvas", "nodes", "cron", "gateway",
            // Channel IDs (aligned with TS CHANNEL_IDS)
            "discord", "slack", "telegram", "whatsapp", "line", "imessage", "signal");

    // ── Browser sandbox ─────────────────────────────────────────────

    public static final String DEFAULT_SANDBOX_BROWSER_IMAGE = "openclaw-sandbox-browser:bookworm-slim";
    public static final String DEFAULT_SANDBOX_COMMON_IMAGE = "openclaw-sandbox-common:bookworm-slim";
    public static final String DEFAULT_SANDBOX_BROWSER_PREFIX = "openclaw-sbx-browser-";
    public static final int DEFAULT_SANDBOX_BROWSER_CDP_PORT = 9222;
    public static final int DEFAULT_SANDBOX_BROWSER_VNC_PORT = 5900;
    public static final int DEFAULT_SANDBOX_BROWSER_NOVNC_PORT = 6080;
    public static final int DEFAULT_SANDBOX_BROWSER_AUTOSTART_TIMEOUT_MS = 12_000;

    public static final String SANDBOX_AGENT_WORKSPACE_MOUNT = "/agent";

    // ── State directories ───────────────────────────────────────────

    private static final String STATE_DIR = System.getenv().getOrDefault("OPENCLAW_STATE_DIR",
            Paths.get(System.getProperty("user.home"), ".openclaw").toString());

    public static final Path SANDBOX_STATE_DIR = Paths.get(STATE_DIR, "sandbox");
    public static final Path SANDBOX_REGISTRY_PATH = SANDBOX_STATE_DIR.resolve("containers.json");
    public static final Path SANDBOX_BROWSER_REGISTRY_PATH = SANDBOX_STATE_DIR.resolve("browsers.json");
}

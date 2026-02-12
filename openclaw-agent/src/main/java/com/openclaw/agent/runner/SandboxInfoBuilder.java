package com.openclaw.agent.runner;

import java.util.Map;

/**
 * Build sandbox info for embedded agent runs.
 * Mirrors {@code agents/pi-embedded-runner/sandbox-info.ts}.
 */
public final class SandboxInfoBuilder {

    private SandboxInfoBuilder() {
    }

    /**
     * Build an {@link RunnerTypes.EmbeddedSandboxInfo} from sandbox context and
     * elevated defaults.
     *
     * @param sandbox      sandbox context map (keys: enabled, workspaceDir,
     *                     workspaceAccess, browser, browserAllowHostControl)
     * @param execElevated elevated defaults map (keys: enabled, allowed,
     *                     defaultLevel)
     * @return the sandbox info, or null if sandbox is not enabled
     */
    @SuppressWarnings("unchecked")
    public static RunnerTypes.EmbeddedSandboxInfo buildEmbeddedSandboxInfo(
            Map<String, Object> sandbox,
            Map<String, Object> execElevated) {
        if (sandbox == null)
            return null;
        Object enabledObj = sandbox.get("enabled");
        if (!Boolean.TRUE.equals(enabledObj))
            return null;

        String workspaceDir = (String) sandbox.get("workspaceDir");
        String workspaceAccess = (String) sandbox.get("workspaceAccess");
        String agentWorkspaceMount = "ro".equals(workspaceAccess) ? "/agent" : null;

        // Browser sub-object
        String browserBridgeUrl = null;
        String browserNoVncUrl = null;
        Object browserObj = sandbox.get("browser");
        if (browserObj instanceof Map<?, ?> browser) {
            browserBridgeUrl = (String) browser.get("bridgeUrl");
            browserNoVncUrl = (String) browser.get("noVncUrl");
        }
        boolean hostBrowserAllowed = Boolean.TRUE.equals(sandbox.get("browserAllowHostControl"));

        // Elevated
        boolean elevatedAllowed = false;
        RunnerTypes.ElevatedConfig elevated = null;
        if (execElevated != null) {
            elevatedAllowed = Boolean.TRUE.equals(execElevated.get("enabled"))
                    && Boolean.TRUE.equals(execElevated.get("allowed"));
        }
        if (elevatedAllowed) {
            String defaultLevel = execElevated.get("defaultLevel") instanceof String s ? s : "off";
            elevated = new RunnerTypes.ElevatedConfig(true, defaultLevel);
        }

        return new RunnerTypes.EmbeddedSandboxInfo(
                true, workspaceDir, workspaceAccess, agentWorkspaceMount,
                browserBridgeUrl, browserNoVncUrl, hostBrowserAllowed, elevated);
    }
}

package com.openclaw.browser.server;

import com.microsoft.playwright.Page;
import com.openclaw.browser.PlaywrightSession;
import com.openclaw.browser.cdp.CdpHelpers;
import com.openclaw.browser.cdp.CdpOperations;
import com.openclaw.browser.cdp.CdpTypes;
import com.openclaw.browser.screenshot.ScreenshotNormalizer;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Dual-channel bridge — routes browser operations to CDP direct or Playwright
 * depending on which channel is optimal for the task.
 *
 * <p>Routing strategy:
 * <ul>
 *   <li><b>Screenshot</b>: CDP direct (lower latency) → Playwright fallback</li>
 *   <li><b>Snapshot (aria/dom)</b>: CDP direct → Playwright fallback</li>
 *   <li><b>JS Evaluate</b>: CDP direct → Playwright fallback</li>
 *   <li><b>Interactions (click/type/fill)</b>: Playwright only (auto-wait)</li>
 *   <li><b>Navigation, tabs, console</b>: Playwright only (richer API)</li>
 * </ul>
 */
@Slf4j
public class DualChannelBridge {

    @Getter private final PlaywrightSession playwrightSession;
    private volatile String cdpWsUrl;
    @Getter private volatile String cdpBaseUrl;
    @Getter private volatile boolean cdpAvailable;

    public DualChannelBridge(PlaywrightSession playwrightSession) {
        this.playwrightSession = playwrightSession;
    }

    // ==================== CDP channel setup ====================

    /**
     * Try to discover CDP WebSocket URL from the base CDP URL.
     * Call this after browser is started/connected.
     */
    public void discoverCdp(String baseCdpUrl) {
        if (baseCdpUrl == null || baseCdpUrl.isBlank()) {
            cdpAvailable = false;
            return;
        }
        this.cdpBaseUrl = baseCdpUrl;
        try {
            String versionUrl = CdpHelpers.appendCdpPath(baseCdpUrl, "/json/version");
            CdpTypes.ChromeVersion version = CdpHelpers.fetchJson(versionUrl, 3000,
                    CdpTypes.ChromeVersion.class);
            if (version != null && version.getWebSocketDebuggerUrl() != null) {
                this.cdpWsUrl = CdpHelpers.normalizeCdpWsUrl(
                        version.getWebSocketDebuggerUrl(), baseCdpUrl);
                this.cdpAvailable = true;
                log.info("CDP direct channel discovered: {}", cdpWsUrl);
            } else {
                cdpAvailable = false;
            }
        } catch (Exception e) {
            log.debug("CDP discovery failed for {}: {}", baseCdpUrl, e.getMessage());
            cdpAvailable = false;
        }
    }

    /**
     * Set CDP WebSocket URL directly (skip discovery).
     */
    public void setCdpWsUrl(String wsUrl) {
        this.cdpWsUrl = wsUrl;
        this.cdpAvailable = wsUrl != null && !wsUrl.isBlank();
    }

    // ==================== Screenshot (CDP-first) ====================

    /**
     * Take a screenshot, trying CDP direct first for lower latency.
     * Result is normalized to stay within byte limits.
     */
    public PlaywrightSession.ScreenshotResult screenshotDual(String targetId, boolean fullPage,
                                                              String ref, String element) {
        // CDP direct for simple viewport screenshots (non-element, non-ref, non-fullPage)
        if (cdpAvailable && ref == null && element == null && !fullPage) {
            try {
                byte[] data = CdpOperations.captureScreenshotPng(cdpWsUrl, false);
                if (data != null && data.length > 0) {
                    byte[] normalized = ScreenshotNormalizer.normalize(data);
                    Page page = safeResolvePage(targetId);
                    return new PlaywrightSession.ScreenshotResult(
                            normalized,
                            page != null ? page.url() : "",
                            page != null ? page.title() : "",
                            "image/png");
                }
            } catch (Exception e) {
                log.debug("CDP screenshot failed, falling back to Playwright: {}",
                        e.getMessage());
            }
        }

        // Playwright fallback — supports fullPage, ref, element
        PlaywrightSession.ScreenshotResult shot = playwrightSession.screenshot(
                targetId, fullPage, ref, element);
        if (shot != null && shot.getBuffer() != null) {
            byte[] normalized = ScreenshotNormalizer.normalize(shot.getBuffer());
            return new PlaywrightSession.ScreenshotResult(
                    normalized, shot.getUrl(), shot.getTitle(), shot.getContentType());
        }
        return shot;
    }

    // ==================== Snapshot (CDP-first) ====================

    /**
     * Get aria snapshot, trying CDP direct first.
     */
    public PlaywrightSession.SnapshotResult snapshotDual(String targetId) {
        if (cdpAvailable) {
            try {
                var ariaNodes = CdpOperations.snapshotAria(cdpWsUrl, 500);
                String ariaTree = ariaNodes != null ? ariaNodes.toString() : "";
                if (ariaTree != null && !ariaTree.isBlank()) {
                    Page page = safeResolvePage(targetId);
                    return new PlaywrightSession.SnapshotResult(
                            ariaTree,
                            page != null ? page.url() : "",
                            page != null ? page.title() : "");
                }
            } catch (Exception e) {
                log.debug("CDP aria snapshot failed, using Playwright: {}", e.getMessage());
            }
        }
        return playwrightSession.snapshot(targetId);
    }

    // ==================== Evaluate (CDP-first) ====================

    /**
     * Evaluate JavaScript, CDP direct first.
     */
    public Map<String, Object> evaluateDual(String expression, String targetId) {
        if (cdpAvailable) {
            try {
                CdpTypes.CdpEvalResult result = CdpOperations.evaluateJavaScript(
                        cdpWsUrl, expression, true, true);
                Map<String, Object> response = new LinkedHashMap<>();
                response.put("ok", true);
                response.put("channel", "cdp");
                if (result != null) {
                    CdpTypes.CdpRemoteObject remoteObj = result.getResult();
                    response.put("value", remoteObj != null && remoteObj.getValue() != null
                            ? remoteObj.getValue().toString() : null);
                    response.put("type", remoteObj != null ? remoteObj.getType() : null);
                    if (result.getExceptionDetails() != null) {
                        response.put("error", result.getExceptionDetails().getText());
                    }
                }
                return response;
            } catch (Exception e) {
                log.debug("CDP evaluate failed, using Playwright: {}", e.getMessage());
            }
        }

        // Playwright fallback
        Page page = safeResolvePage(targetId);
        if (page != null) {
            try {
                Object result = page.evaluate(expression);
                Map<String, Object> response = new LinkedHashMap<>();
                response.put("ok", true);
                response.put("channel", "playwright");
                response.put("value", result != null ? result.toString() : null);
                return response;
            } catch (Exception e) {
                return Map.of("ok", false, "error", e.getMessage());
            }
        }
        return Map.of("ok", false, "error", "No page available");
    }

    // ==================== Channel info ====================

    /**
     * Get info about both channels' status.
     */
    public Map<String, Object> getChannelInfo() {
        Map<String, Object> info = new LinkedHashMap<>();
        info.put("playwright", playwrightSession.isRunning());
        info.put("cdpDirect", cdpAvailable);
        info.put("cdpWsUrl", cdpWsUrl);
        return info;
    }

    // ==================== Private ====================

    private Page safeResolvePage(String targetId) {
        try {
            if (playwrightSession.isRunning()) {
                return playwrightSession.resolveTargetPage(targetId);
            }
        } catch (Exception e) {
            // ignore
        }
        return null;
    }
}

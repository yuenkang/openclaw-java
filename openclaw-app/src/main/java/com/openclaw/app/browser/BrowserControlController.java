package com.openclaw.app.browser;

import com.openclaw.agent.tools.builtin.browser.PlaywrightSession;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

/**
 * REST controller exposing browser control endpoints.
 * Corresponds to TypeScript's browser/routes/ directory.
 *
 * <p>
 * Provides HTTP endpoints for browser lifecycle management, tab operations,
 * navigation, snapshots, screenshots, actions, and console message retrieval.
 * The {@link com.openclaw.agent.tools.builtin.browser.BrowserClient} sends
 * requests to these endpoints.
 * </p>
 */
@Slf4j
@RestController
@RequestMapping("/browser")
public class BrowserControlController {

    private final PlaywrightSession session = new PlaywrightSession();
    private boolean headless = true;

    // =========================================================================
    // Status & Lifecycle
    // =========================================================================

    @GetMapping
    public ResponseEntity<Map<String, Object>> status() {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("enabled", true);
        body.put("running", session.isRunning());
        body.put("headless", headless);
        return ResponseEntity.ok(body);
    }

    @PostMapping("/start")
    public ResponseEntity<Map<String, Object>> start(@RequestBody(required = false) Map<String, Object> body) {
        Map<String, Object> result = new LinkedHashMap<>();
        try {
            if (session.isRunning()) {
                result.put("ok", true);
                result.put("message", "already running");
                return ResponseEntity.ok(result);
            }

            String cdpUrl = body != null ? (String) body.get("cdpUrl") : null;
            if (body != null && body.containsKey("headless")) {
                headless = Boolean.TRUE.equals(body.get("headless"));
            }

            if (cdpUrl != null && !cdpUrl.isBlank()) {
                session.connectCDP(cdpUrl);
            } else {
                session.launch(headless);
            }
            result.put("ok", true);
        } catch (Exception e) {
            log.error("Failed to start browser: {}", e.getMessage(), e);
            result.put("ok", false);
            result.put("error", e.getMessage());
            return ResponseEntity.status(500).body(result);
        }
        return ResponseEntity.ok(result);
    }

    @PostMapping("/stop")
    public ResponseEntity<Map<String, Object>> stop() {
        Map<String, Object> result = new LinkedHashMap<>();
        boolean stopped = session.stop();
        result.put("ok", true);
        result.put("stopped", stopped);
        return ResponseEntity.ok(result);
    }

    @GetMapping("/profiles")
    public ResponseEntity<Map<String, Object>> profiles() {
        Map<String, Object> profile = new LinkedHashMap<>();
        profile.put("name", "default");
        profile.put("running", session.isRunning());

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("profiles", List.of(profile));
        return ResponseEntity.ok(result);
    }

    // =========================================================================
    // Tabs
    // =========================================================================

    @GetMapping("/tabs")
    public ResponseEntity<Map<String, Object>> listTabs() {
        Map<String, Object> result = new LinkedHashMap<>();
        if (!session.isRunning()) {
            result.put("running", false);
            result.put("tabs", List.of());
            return ResponseEntity.ok(result);
        }
        try {
            List<PlaywrightSession.TabInfo> tabs = session.listTabs();
            result.put("running", true);
            result.put("tabs", tabs.stream().map(this::tabToMap).toList());
        } catch (Exception e) {
            result.put("error", e.getMessage());
            return ResponseEntity.status(500).body(result);
        }
        return ResponseEntity.ok(result);
    }

    @PostMapping("/tabs/open")
    public ResponseEntity<Map<String, Object>> openTab(@RequestBody Map<String, Object> body) {
        String url = (String) body.get("url");
        if (url == null || url.isBlank()) {
            return badRequest("url is required");
        }
        try {
            ensureBrowserStarted();
            PlaywrightSession.TabInfo tab = session.openTab(url);
            return ResponseEntity.ok(tabToMap(tab));
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    @PostMapping("/tabs/focus")
    public ResponseEntity<Map<String, Object>> focusTab(@RequestBody Map<String, Object> body) {
        String targetId = (String) body.get("targetId");
        if (targetId == null || targetId.isBlank()) {
            return badRequest("targetId is required");
        }
        try {
            session.focusTab(targetId);
            return okResponse();
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    @DeleteMapping("/tabs/{targetId}")
    public ResponseEntity<Map<String, Object>> closeTab(@PathVariable String targetId) {
        try {
            session.closeTab(targetId);
            return okResponse();
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    // =========================================================================
    // Navigation
    // =========================================================================

    @PostMapping("/navigate")
    public ResponseEntity<Map<String, Object>> navigate(@RequestBody Map<String, Object> body) {
        String url = (String) body.get("url");
        String targetId = (String) body.get("targetId");
        if (url == null || url.isBlank()) {
            return badRequest("url is required");
        }
        try {
            ensureBrowserStarted();
            PlaywrightSession.NavigateResult nav = session.navigate(url, targetId);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("url", nav.getUrl());
            result.put("title", nav.getTitle());
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    // =========================================================================
    // Snapshot
    // =========================================================================

    @GetMapping("/snapshot")
    public ResponseEntity<Map<String, Object>> snapshot(
            @RequestParam(required = false) String targetId) {
        try {
            ensureBrowserStarted();
            PlaywrightSession.SnapshotResult snap = session.snapshot(targetId);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("format", "aria");
            result.put("snapshot", snap.getSnapshot());
            result.put("url", snap.getUrl());
            result.put("title", snap.getTitle());
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    // =========================================================================
    // Screenshot
    // =========================================================================

    @PostMapping("/screenshot")
    public ResponseEntity<Map<String, Object>> screenshot(
            @RequestBody(required = false) Map<String, Object> body) {
        try {
            ensureBrowserStarted();
            String targetId = body != null ? (String) body.get("targetId") : null;
            boolean fullPage = body != null && Boolean.TRUE.equals(body.get("fullPage"));

            PlaywrightSession.ScreenshotResult shot = session.screenshot(targetId, fullPage);

            String base64 = Base64.getEncoder().encodeToString(shot.getBuffer());

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("data", base64);
            result.put("contentType", shot.getContentType());
            result.put("url", shot.getUrl());
            result.put("title", shot.getTitle());
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    // =========================================================================
    // Act
    // =========================================================================

    @PostMapping("/act")
    public ResponseEntity<Map<String, Object>> act(@RequestBody Map<String, Object> body) {
        String kind = (String) body.get("kind");
        String targetId = (String) body.get("targetId");
        if (kind == null || kind.isBlank()) {
            return badRequest("kind is required");
        }
        try {
            ensureBrowserStarted();
            Map<String, Object> result = session.act(kind, targetId, body);
            return ResponseEntity.ok(result);
        } catch (IllegalArgumentException e) {
            return badRequest(e.getMessage());
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    // =========================================================================
    // Console
    // =========================================================================

    @GetMapping("/console")
    public ResponseEntity<Map<String, Object>> console(
            @RequestParam(required = false) String targetId) {
        try {
            if (!session.isRunning()) {
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("ok", true);
                result.put("messages", List.of());
                return ResponseEntity.ok(result);
            }
            List<PlaywrightSession.ConsoleEntry> messages = session.getConsoleMessages(targetId);
            List<Map<String, Object>> mapped = messages.stream().map(e -> {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("type", e.getType());
                m.put("text", e.getText());
                m.put("timestamp", e.getTimestamp());
                return m;
            }).toList();

            Map<String, Object> result = new LinkedHashMap<>();
            result.put("ok", true);
            result.put("messages", mapped);
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return errorResponse(e);
        }
    }

    // =========================================================================
    // Hooks (stubs)
    // =========================================================================

    @PostMapping("/hooks/file-chooser")
    public ResponseEntity<Map<String, Object>> fileChooser(@RequestBody Map<String, Object> body) {
        return okResponse();
    }

    @PostMapping("/hooks/dialog")
    public ResponseEntity<Map<String, Object>> dialog(@RequestBody Map<String, Object> body) {
        return okResponse();
    }

    @PostMapping("/pdf")
    public ResponseEntity<Map<String, Object>> pdf(@RequestBody(required = false) Map<String, Object> body) {
        return badRequest("PDF export not yet implemented");
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private void ensureBrowserStarted() {
        if (!session.isRunning()) {
            session.launch(headless);
        }
    }

    private Map<String, Object> tabToMap(PlaywrightSession.TabInfo tab) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("targetId", tab.getTargetId());
        m.put("url", tab.getUrl());
        m.put("title", tab.getTitle());
        return m;
    }

    private static ResponseEntity<Map<String, Object>> okResponse() {
        return ResponseEntity.ok(Map.of("ok", true));
    }

    private static ResponseEntity<Map<String, Object>> badRequest(String message) {
        return ResponseEntity.badRequest().body(Map.of("ok", false, "error", message));
    }

    private static ResponseEntity<Map<String, Object>> errorResponse(Exception e) {
        log.error("Browser control error: {}", e.getMessage(), e);
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("ok", false);
        body.put("error", e.getMessage());
        return ResponseEntity.status(500).body(body);
    }
}

package com.openclaw.browser;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.microsoft.playwright.*;
import com.microsoft.playwright.options.MouseButton;
import com.microsoft.playwright.options.KeyboardModifier;
import com.microsoft.playwright.options.ScreenshotType;
import com.microsoft.playwright.options.WaitForSelectorState;
import com.microsoft.playwright.options.WaitUntilState;
import com.openclaw.browser.playwright.PageState;
import com.openclaw.browser.playwright.PwToolsShared;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Manages Playwright browser sessions.
 * Handles launching/connecting to Chromium, managing pages/tabs,
 * performing navigation, snapshots, screenshots, and browser actions.
 */
@Slf4j
public class PlaywrightSession implements AutoCloseable {

    private static final int MAX_CONSOLE_MESSAGES = 200;

    private Playwright playwright;
    private Browser browser;
    private BrowserContext context;
    private final AtomicBoolean closed = new AtomicBoolean(false);
    private final Map<Page, List<ConsoleEntry>> pageConsoleMessages = new WeakHashMap<>();

    // =========================================================================
    // Data classes
    // =========================================================================

    @Data
    public static class ConsoleEntry {
        private final String type;
        private final String text;
        private final long timestamp;
    }

    @Data
    public static class TabInfo {
        private final String targetId;
        private final String url;
        private final String title;
    }

    @Data
    public static class SnapshotResult {
        private final String snapshot;
        private final String url;
        private final String title;
    }

    @Data
    public static class ScreenshotResult {
        private final byte[] buffer;
        private final String url;
        private final String title;
        private final String contentType;
    }

    @Data
    public static class NavigateResult {
        private final String url;
        private final String title;
    }

    // =========================================================================
    // Lifecycle
    // =========================================================================

    /**
     * Launch a new Chromium browser.
     */
    public synchronized void launch(boolean headless) {
        if (browser != null && browser.isConnected()) {
            log.debug("Browser already running");
            return;
        }
        log.info("Launching Playwright Chromium (headless={})", headless);
        playwright = Playwright.create();
        browser = playwright.chromium().launch(
                new BrowserType.LaunchOptions()
                        .setHeadless(headless)
                        .setArgs(List.of(
                                "--no-first-run",
                                "--no-default-browser-check",
                                "--disable-background-timer-throttling",
                                "--disable-backgrounding-occluded-windows",
                                "--disable-renderer-backgrounding")));
        context = browser.newContext(
                new Browser.NewContextOptions()
                        .setViewportSize(1280, 720));
        Page initialPage = context.newPage();
        setupPageListeners(initialPage);
        log.info("Browser launched successfully");
    }

    /**
     * Connect to an existing browser via CDP URL.
     */
    public synchronized void connectCDP(String cdpUrl) {
        if (browser != null && browser.isConnected()) {
            log.debug("Browser already connected");
            return;
        }
        log.info("Connecting to browser via CDP: {}", cdpUrl);
        playwright = Playwright.create();
        browser = playwright.chromium().connectOverCDP(cdpUrl);
        List<BrowserContext> contexts = browser.contexts();
        if (!contexts.isEmpty()) {
            context = contexts.get(0);
        } else {
            context = browser.newContext();
        }
        for (Page page : context.pages()) {
            setupPageListeners(page);
        }
        log.info("Connected to browser via CDP");
    }

    public boolean isRunning() {
        return browser != null && browser.isConnected();
    }

    public synchronized boolean stop() {
        if (browser == null) {
            return false;
        }
        try {
            if (browser.isConnected()) {
                browser.close();
            }
        } catch (Exception e) {
            log.debug("Error closing browser: {}", e.getMessage());
        }
        try {
            if (playwright != null) {
                playwright.close();
            }
        } catch (Exception e) {
            log.debug("Error closing playwright: {}", e.getMessage());
        }
        browser = null;
        context = null;
        playwright = null;
        pageConsoleMessages.clear();
        log.info("Browser stopped");
        return true;
    }

    @Override
    public void close() {
        if (closed.compareAndSet(false, true)) {
            stop();
        }
    }

    // =========================================================================
    // Tab Management
    // =========================================================================

    public List<TabInfo> listTabs() {
        ensureContext();
        List<TabInfo> tabs = new ArrayList<>();
        for (Page page : context.pages()) {
            tabs.add(new TabInfo(pageId(page), page.url(), page.title()));
        }
        return tabs;
    }

    public TabInfo openTab(String url) {
        ensureContext();
        Page page = context.newPage();
        setupPageListeners(page);
        if (url != null && !url.isBlank() && !url.equals("about:blank")) {
            page.navigate(url, new Page.NavigateOptions().setWaitUntil(WaitUntilState.DOMCONTENTLOADED));
        }
        return new TabInfo(pageId(page), page.url(), page.title());
    }

    public void closeTab(String targetId) {
        Page page = findPage(targetId);
        if (page != null) {
            page.close();
        }
    }

    public void focusTab(String targetId) {
        Page page = findPage(targetId);
        if (page != null) {
            page.bringToFront();
        }
    }

    // =========================================================================
    // Navigation
    // =========================================================================

    public NavigateResult navigate(String url, String targetId) {
        Page page = resolveTargetPage(targetId);
        page.navigate(url, new Page.NavigateOptions().setWaitUntil(WaitUntilState.DOMCONTENTLOADED));
        return new NavigateResult(page.url(), page.title());
    }

    // =========================================================================
    // Snapshot
    // =========================================================================

    public SnapshotResult snapshot(String targetId) {
        Page page = resolveTargetPage(targetId);
        String snapshot;
        try {
            snapshot = page.locator("body").ariaSnapshot();
        } catch (Exception e) {
            log.debug("ariaSnapshot failed, falling back to innerText: {}", e.getMessage());
            snapshot = page.locator("body").innerText();
            if (snapshot.length() > 50000) {
                snapshot = snapshot.substring(0, 50000) + "\n... (truncated)";
            }
        }
        return new SnapshotResult(snapshot, page.url(), page.title());
    }

    // =========================================================================
    // Screenshot
    // =========================================================================

    /**
     * Take a screenshot. Supports full page, element-by-ref, and
     * element-by-selector.
     */
    public ScreenshotResult screenshot(String targetId, boolean fullPage) {
        return screenshot(targetId, fullPage, null, null);
    }

    public ScreenshotResult screenshot(String targetId, boolean fullPage, String ref, String element) {
        Page page = resolveTargetPage(targetId);

        byte[] buffer;
        if (ref != null && !ref.isBlank()) {
            // Element screenshot via ref
            Locator loc = resolveRef(page, ref);
            buffer = loc.screenshot(new Locator.ScreenshotOptions().setType(ScreenshotType.PNG));
        } else if (element != null && !element.isBlank()) {
            // Element screenshot via CSS selector
            Locator loc = page.locator(element).first();
            buffer = loc.screenshot(new Locator.ScreenshotOptions().setType(ScreenshotType.PNG));
        } else {
            // Full page or viewport screenshot
            buffer = page.screenshot(new Page.ScreenshotOptions()
                    .setFullPage(fullPage)
                    .setType(ScreenshotType.PNG));
        }
        return new ScreenshotResult(buffer, page.url(), page.title(), "image/png");
    }

    /**
     * Take a screenshot with precise bounding-box labels for interactive elements.
     * Uses Playwright locator.boundingBox() for accurate positioning.
     * Corresponds to TS screenshotWithLabelsViaPlaywright.
     */
    public ScreenshotWithLabelsResult screenshotWithLabels(String targetId,
                                                            Map<String, com.openclaw.browser.snapshot.RoleSnapshot.RoleRef> refs,
                                                            int maxLabels) {
        Page page = resolveTargetPage(targetId);
        byte[] screenshotBytes = page.screenshot(new Page.ScreenshotOptions().setType(ScreenshotType.PNG));

        if (refs == null || refs.isEmpty()) {
            return new ScreenshotWithLabelsResult(screenshotBytes, 0, 0, page.url(), page.title());
        }

        try {
            var image = javax.imageio.ImageIO.read(new java.io.ByteArrayInputStream(screenshotBytes));
            if (image == null) {
                return new ScreenshotWithLabelsResult(screenshotBytes, 0, 0, page.url(), page.title());
            }

            var g = image.createGraphics();
            g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON);
            g.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING, java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int labelSize = Math.max(14, Math.min(24, image.getWidth() / 60));
            var font = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.BOLD, labelSize - 2);
            g.setFont(font);
            var fm = g.getFontMetrics();

            int labeled = 0;
            int skipped = 0;
            int limit = maxLabels > 0 ? maxLabels : 50;

            for (var entry : refs.entrySet()) {
                if (labeled >= limit) { skipped++; continue; }
                String refKey = entry.getKey();
                var roleRef = entry.getValue();
                if (!com.openclaw.browser.snapshot.RoleSnapshot.isInteractiveRole(roleRef.getRole())) {
                    continue;
                }

                try {
                    Locator loc = resolveRef(page, refKey);
                    com.microsoft.playwright.options.BoundingBox box = loc.boundingBox(
                            new Locator.BoundingBoxOptions().setTimeout(500));
                    if (box == null) { skipped++; continue; }

                    // Clamp to image bounds
                    int x = (int) Math.max(0, Math.min(box.x, image.getWidth() - 1));
                    int y = (int) Math.max(0, Math.min(box.y, image.getHeight() - 1));

                    // Draw badge at top-left of element
                    String label = refKey.startsWith("e") ? refKey.substring(1) : refKey;
                    int textW = fm.stringWidth(label) + 6;
                    int textH = fm.getHeight() + 2;

                    // Badge background
                    g.setColor(new java.awt.Color(220, 38, 38, 230));
                    g.fillRoundRect(x, y, textW, textH, 4, 4);
                    // Badge border
                    g.setColor(java.awt.Color.WHITE);
                    g.setStroke(new java.awt.BasicStroke(1f));
                    g.drawRoundRect(x, y, textW, textH, 4, 4);
                    // Label text
                    g.drawString(label, x + 3, y + fm.getAscent() + 1);

                    labeled++;
                } catch (Exception e) {
                    skipped++;
                }
            }

            g.dispose();

            var baos = new java.io.ByteArrayOutputStream();
            javax.imageio.ImageIO.write(image, "png", baos);
            return new ScreenshotWithLabelsResult(baos.toByteArray(), labeled, skipped, page.url(), page.title());
        } catch (Exception e) {
            log.warn("screenshotWithLabels failed, returning unlabeled: {}", e.getMessage());
            return new ScreenshotWithLabelsResult(screenshotBytes, 0, refs.size(), page.url(), page.title());
        }
    }

    @Data
    public static class ScreenshotWithLabelsResult {
        private final byte[] buffer;
        private final int labels;
        private final int skipped;
        private final String url;
        private final String title;
    }

    /**
     * Get the CDP target ID for a page.
     * Falls back to identityHashCode if CDP session is not available.
     */
    public String pageTargetId(Page page) {
        try {
            // Try to get real target ID via CDP session
            com.google.gson.JsonObject params = new com.google.gson.JsonObject();
            com.google.gson.JsonObject result = page.context().browser()
                    .newBrowserCDPSession()
                    .send("Target.getTargets", params);
            if (result != null && result.has("targetInfos")) {
                var targets = result.getAsJsonArray("targetInfos");
                for (var target : targets) {
                    var info = target.getAsJsonObject();
                    if ("page".equals(info.get("type").getAsString())) {
                        String url = info.has("url") ? info.get("url").getAsString() : "";
                        if (url.equals(page.url())) {
                            return info.get("targetId").getAsString();
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.debug("CDP target ID lookup failed, falling back: {}", e.getMessage());
        }
        // Fallback
        return "page-" + Integer.toHexString(System.identityHashCode(page));
    }

    // =========================================================================
    // Console Messages
    // =========================================================================

    public List<ConsoleEntry> getConsoleMessages(String targetId) {
        Page page = resolveTargetPage(targetId);
        List<ConsoleEntry> entries = pageConsoleMessages.get(page);
        return entries != null ? new ArrayList<>(entries) : List.of();
    }

    // =========================================================================
    // Actions (act)
    // =========================================================================

    /**
     * Execute a browser action. Supports: click, type, press, hover,
     * scrollIntoView, drag, select, fill, resize, wait, evaluate,
     * goBack, goForward, close.
     */
    public Map<String, Object> act(String kind, String targetId, Map<String, Object> params) {
        Page page = resolveTargetPage(targetId);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("ok", true);
        result.put("targetId", pageId(page));
        result.put("url", page.url());

        try {
            switch (kind) {
                case "click" -> {
                    String ref = requireParam(params, "ref");
                    Locator loc = resolveRef(page, ref);
                    boolean doubleClick = getBool(params, "doubleClick");
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    String buttonStr = getStr(params, "button");
                    MouseButton mouseBtn = switch (buttonStr != null ? buttonStr : "") {
                        case "right" -> MouseButton.RIGHT;
                        case "middle" -> MouseButton.MIDDLE;
                        default -> MouseButton.LEFT;
                    };
                    @SuppressWarnings("unchecked")
                    List<String> modList = (List<String>) params.get("modifiers");
                    List<KeyboardModifier> modifiers = parseModifiers(modList);
                    if (doubleClick) {
                        var opts = new Locator.DblclickOptions().setTimeout(timeout).setButton(mouseBtn);
                        if (!modifiers.isEmpty()) opts.setModifiers(modifiers);
                        loc.dblclick(opts);
                    } else {
                        var opts = new Locator.ClickOptions().setTimeout(timeout).setButton(mouseBtn);
                        if (!modifiers.isEmpty()) opts.setModifiers(modifiers);
                        loc.click(opts);
                    }
                }
                case "type" -> {
                    String ref = requireParam(params, "ref");
                    String text = requireParam(params, "text");
                    Locator loc = resolveRef(page, ref);
                    boolean submit = getBool(params, "submit");
                    boolean slowly = getBool(params, "slowly");
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    if (slowly) {
                        loc.click(new Locator.ClickOptions().setTimeout(timeout));
                        loc.pressSequentially(text, new Locator.PressSequentiallyOptions().setDelay(75));
                    } else {
                        loc.fill(text, new Locator.FillOptions().setTimeout(timeout));
                    }
                    if (submit) {
                        loc.press("Enter", new Locator.PressOptions().setTimeout(timeout));
                    }
                }
                case "press" -> {
                    String key = requireParam(params, "key");
                    int delay = getInt(params, "delayMs") != null ? getInt(params, "delayMs") : 0;
                    page.keyboard().press(key, new Keyboard.PressOptions().setDelay(Math.max(0, delay)));
                }
                case "hover" -> {
                    String ref = requireParam(params, "ref");
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    resolveRef(page, ref).hover(new Locator.HoverOptions().setTimeout(timeout));
                }
                case "scrollIntoView" -> {
                    String ref = requireParam(params, "ref");
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 20000);
                    resolveRef(page, ref).scrollIntoViewIfNeeded(
                            new Locator.ScrollIntoViewIfNeededOptions().setTimeout(timeout));
                }
                case "drag" -> {
                    String startRef = requireParam(params, "startRef");
                    String endRef = requireParam(params, "endRef");
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    resolveRef(page, startRef).dragTo(resolveRef(page, endRef),
                            new Locator.DragToOptions().setTimeout(timeout));
                }
                case "select" -> {
                    String ref = requireParam(params, "ref");
                    @SuppressWarnings("unchecked")
                    List<String> values = (List<String>) params.get("values");
                    if (values == null || values.isEmpty()) {
                        throw new IllegalArgumentException("values are required for select");
                    }
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    resolveRef(page, ref).selectOption(values.toArray(new String[0]),
                            new Locator.SelectOptionOptions().setTimeout(timeout));
                }
                case "fill" -> {
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> fields = (List<Map<String, Object>>) params.get("fields");
                    if (fields == null || fields.isEmpty()) {
                        throw new IllegalArgumentException("fields are required for fill");
                    }
                    int timeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    for (Map<String, Object> field : fields) {
                        String ref = getStr(field, "ref");
                        String type = getStr(field, "type");
                        if (ref == null || type == null)
                            continue;
                        Locator loc = resolveRef(page, ref);
                        if ("checkbox".equals(type) || "radio".equals(type)) {
                            Object val = field.get("value");
                            boolean checked = Boolean.TRUE.equals(val)
                                    || "true".equals(String.valueOf(val))
                                    || "1".equals(String.valueOf(val));
                            loc.setChecked(checked, new Locator.SetCheckedOptions().setTimeout(timeout));
                        } else {
                            String value = field.get("value") != null ? String.valueOf(field.get("value")) : "";
                            loc.fill(value, new Locator.FillOptions().setTimeout(timeout));
                        }
                    }
                }
                case "resize" -> {
                    Integer width = getInt(params, "width");
                    Integer height = getInt(params, "height");
                    if (width == null || height == null) {
                        throw new IllegalArgumentException("width and height are required for resize");
                    }
                    page.setViewportSize(width, height);
                }
                case "wait" -> {
                    doWait(page, params);
                }
                case "evaluate" -> {
                    String fn = requireParam(params, "fn");
                    String ref = getStr(params, "ref");
                    Object evalResult;
                    if (ref != null && !ref.isBlank()) {
                        evalResult = resolveRef(page, ref).evaluate(fn);
                    } else {
                        evalResult = page.evaluate(fn);
                    }
                    result.put("result", evalResult != null ? evalResult.toString() : null);
                }
                case "goBack" -> {
                    page.goBack(new Page.GoBackOptions().setWaitUntil(WaitUntilState.DOMCONTENTLOADED));
                    result.put("url", page.url());
                    result.put("title", page.title());
                }
                case "goForward" -> {
                    page.goForward(new Page.GoForwardOptions().setWaitUntil(WaitUntilState.DOMCONTENTLOADED));
                    result.put("url", page.url());
                    result.put("title", page.title());
                }
                case "close" -> {
                    page.close();
                }
                case "form_fill" -> {
                    // Alias for fill — same batch form-fill logic
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> formFields = (List<Map<String, Object>>) params.get("fields");
                    if (formFields == null || formFields.isEmpty()) {
                        throw new IllegalArgumentException("fields are required for form_fill");
                    }
                    int ffTimeout = clampTimeout(getInt(params, "timeoutMs"), 8000);
                    for (Map<String, Object> field : formFields) {
                        String fRef = getStr(field, "ref");
                        if (fRef == null) continue;
                        Locator loc = resolveRef(page, fRef);
                        Object checkedVal = field.get("checked");
                        if (checkedVal != null) {
                            boolean checked = Boolean.TRUE.equals(checkedVal)
                                    || "true".equals(String.valueOf(checkedVal));
                            loc.setChecked(checked, new Locator.SetCheckedOptions().setTimeout(ffTimeout));
                        } else {
                            @SuppressWarnings("unchecked")
                            List<String> options = (List<String>) field.get("options");
                            if (options != null && !options.isEmpty()) {
                                loc.selectOption(options.toArray(new String[0]),
                                        new Locator.SelectOptionOptions().setTimeout(ffTimeout));
                            } else {
                                String val = field.get("value") != null ? String.valueOf(field.get("value")) : "";
                                loc.fill(val, new Locator.FillOptions().setTimeout(ffTimeout));
                            }
                        }
                    }
                }
                case "download" -> {
                    String dlRef = requireParam(params, "ref");
                    String dlPath = requireParam(params, "path");
                    int dlTimeout = clampTimeout(getInt(params, "timeoutMs"), 30000);
                    Locator dlLoc = resolveRef(page, dlRef);
                    Download download = page.waitForDownload(() ->
                            dlLoc.click(new Locator.ClickOptions().setTimeout(dlTimeout)));
                    Path outPath = Path.of(dlPath);
                    try {
                        Files.createDirectories(outPath.getParent());
                    } catch (Exception ignored) {}
                    download.saveAs(outPath);
                    result.put("downloadUrl", download.url());
                    result.put("suggestedFilename", download.suggestedFilename());
                    result.put("path", outPath.toAbsolutePath().toString());
                }
                case "wait_for_download" -> {
                    int wdTimeout = clampTimeout(getInt(params, "timeoutMs"), 30000);
                    String wdPath = getStr(params, "path");
                    Download dl = page.waitForDownload(
                            new Page.WaitForDownloadOptions().setTimeout(wdTimeout),
                            () -> {});
                    String suggested = dl.suggestedFilename();
                    Path savePath;
                    if (wdPath != null && !wdPath.isBlank()) {
                        savePath = Path.of(wdPath);
                    } else {
                        try {
                            Path dir = Path.of("/tmp/openclaw/downloads");
                            Files.createDirectories(dir);
                            savePath = dir.resolve(UUID.randomUUID() + "-" + suggested);
                        } catch (Exception e) {
                            throw new RuntimeException("Failed to create download path: " + e.getMessage());
                        }
                    }
                    dl.saveAs(savePath);
                    result.put("downloadUrl", dl.url());
                    result.put("suggestedFilename", suggested);
                    result.put("path", savePath.toAbsolutePath().toString());
                }
                case "set_locale" -> {
                    String locale = requireParam(params, "locale");
                    // Apply via CDP Emulation.setLocaleOverride
                    try {
                        var cdpSession = page.context().newCDPSession(page);
                        JsonObject localeParams = new JsonObject();
                        localeParams.addProperty("locale", locale);
                        cdpSession.send("Emulation.setLocaleOverride", localeParams);
                    } catch (Exception e) {
                        String msg = e.getMessage();
                        if (msg == null || !msg.contains("Another locale override is already in effect")) {
                            log.debug("CDP locale override failed: {}", msg);
                        }
                    }
                    result.put("locale", locale);
                }
                case "set_timezone" -> {
                    String timezone = requireParam(params, "timezone");
                    page.evaluate("(tz) => { try { Intl.DateTimeFormat(undefined, {timeZone: tz}); } catch(e) { throw new Error('Invalid timezone: ' + tz); } }", timezone);
                    // Apply via CDP if available
                    try {
                        var cdpSession = page.context().newCDPSession(page);
                        JsonObject tzParams = new JsonObject();
                        tzParams.addProperty("timezoneId", timezone);
                        cdpSession.send("Emulation.setTimezoneOverride", tzParams);
                    } catch (Exception e) {
                        log.debug("CDP timezone override failed, emulation may be incomplete: {}", e.getMessage());
                    }
                    result.put("timezone", timezone);
                }
                case "set_device" -> {
                    Integer devWidth = getInt(params, "width");
                    Integer devHeight = getInt(params, "height");
                    Double deviceScaleFactor = params.get("deviceScaleFactor") != null
                            ? ((Number) params.get("deviceScaleFactor")).doubleValue() : null;
                    Boolean isMobile = params.get("isMobile") != null
                            ? Boolean.TRUE.equals(params.get("isMobile")) : null;
                    String userAgent = getStr(params, "userAgent");
                    if (devWidth != null && devHeight != null) {
                        page.setViewportSize(devWidth, devHeight);
                    }
                    // Apply device emulation via CDP
                    try {
                        var cdpSession = page.context().newCDPSession(page);
                        JsonObject cdpDeviceParams = new JsonObject();
                        cdpDeviceParams.addProperty("width", devWidth != null ? devWidth : 0);
                        cdpDeviceParams.addProperty("height", devHeight != null ? devHeight : 0);
                        cdpDeviceParams.addProperty("deviceScaleFactor", deviceScaleFactor != null ? deviceScaleFactor : 0);
                        cdpDeviceParams.addProperty("mobile", isMobile != null && isMobile);
                        cdpSession.send("Emulation.setDeviceMetricsOverride", cdpDeviceParams);
                        if (userAgent != null) {
                            JsonObject uaParams = new JsonObject();
                            uaParams.addProperty("userAgent", userAgent);
                            cdpSession.send("Emulation.setUserAgentOverride", uaParams);
                        }
                    } catch (Exception e) {
                        log.debug("CDP device emulation failed: {}", e.getMessage());
                    }
                }
                case "response_body" -> {
                    String rbUrl = requireParam(params, "url");
                    int rbTimeout = clampTimeout(getInt(params, "timeoutMs"), 20000);
                    int rbMaxChars = getInt(params, "maxChars") != null ? getInt(params, "maxChars") : 200_000;
                    Map<String, Object> rbResult = responseBody(targetId, rbUrl, rbTimeout, rbMaxChars);
                    result.putAll(rbResult);
                }
                case "set_input_files" -> {
                    String sifRef = getStr(params, "ref");
                    String sifElement = getStr(params, "element");
                    @SuppressWarnings("unchecked")
                    List<String> sifPaths = (List<String>) params.get("paths");
                    if (sifPaths == null || sifPaths.isEmpty()) {
                        throw new IllegalArgumentException("paths are required for set_input_files");
                    }
                    Path[] files = sifPaths.stream().map(Path::of).toArray(Path[]::new);
                    Locator loc;
                    if (sifRef != null && !sifRef.isBlank()) {
                        loc = resolveRef(page, sifRef);
                    } else if (sifElement != null && !sifElement.isBlank()) {
                        loc = page.locator(sifElement).first();
                    } else {
                        throw new IllegalArgumentException("ref or element is required for set_input_files");
                    }
                    loc.setInputFiles(files);
                }
                default -> throw new IllegalArgumentException("Unsupported act kind: " + kind);
            }
        } catch (PlaywrightException e) {
            throw new RuntimeException(toAiFriendlyError(e), e);
        }

        return result;
    }

    // =========================================================================
    // Wait logic (extracted for readability)
    // =========================================================================

    private void doWait(Page page, Map<String, Object> params) {
        Integer timeMs = getInt(params, "timeMs");
        String text = getStr(params, "text");
        String textGone = getStr(params, "textGone");
        String selector = getStr(params, "selector");
        String url = getStr(params, "url");
        String loadState = getStr(params, "loadState");
        String fn = getStr(params, "fn");
        double timeout = clampTimeout(getInt(params, "timeoutMs"), 20000);

        if (timeMs == null && text == null && textGone == null
                && selector == null && url == null && loadState == null && fn == null) {
            throw new IllegalArgumentException(
                    "wait requires at least one of: timeMs, text, textGone, selector, url, loadState, fn");
        }

        if (timeMs != null) {
            page.waitForTimeout(Math.max(0, timeMs));
        }
        if (text != null) {
            page.getByText(text).first().waitFor(
                    new Locator.WaitForOptions().setState(WaitForSelectorState.VISIBLE).setTimeout(timeout));
        }
        if (textGone != null) {
            page.getByText(textGone).first().waitFor(
                    new Locator.WaitForOptions().setState(WaitForSelectorState.HIDDEN).setTimeout(timeout));
        }
        if (selector != null && !selector.isBlank()) {
            page.locator(selector).first().waitFor(
                    new Locator.WaitForOptions().setState(WaitForSelectorState.VISIBLE).setTimeout(timeout));
        }
        if (url != null && !url.isBlank()) {
            page.waitForURL(url, new Page.WaitForURLOptions().setTimeout(timeout));
        }
        if (loadState != null) {
            com.microsoft.playwright.options.LoadState ls = switch (loadState) {
                case "load" -> com.microsoft.playwright.options.LoadState.LOAD;
                case "networkidle" -> com.microsoft.playwright.options.LoadState.NETWORKIDLE;
                default -> com.microsoft.playwright.options.LoadState.DOMCONTENTLOADED;
            };
            page.waitForLoadState(ls, new Page.WaitForLoadStateOptions().setTimeout(timeout));
        }
        if (fn != null && !fn.isBlank()) {
            page.waitForFunction(fn, new Page.WaitForFunctionOptions().setTimeout(timeout));
        }
    }

    // =========================================================================
    // Internals
    // =========================================================================

    private void ensureContext() {
        if (context == null) {
            throw new IllegalStateException("Browser not started. Call launch() or connectCDP() first.");
        }
    }

    public Page resolveTargetPage(String targetId) {
        ensureContext();
        if (targetId != null && !targetId.isBlank()) {
            Page page = findPage(targetId);
            if (page != null)
                return page;
        }
        List<Page> pages = context.pages();
        if (pages.isEmpty()) {
            Page page = context.newPage();
            setupPageListeners(page);
            return page;
        }
        return pages.get(pages.size() - 1);
    }

    private Page findPage(String targetId) {
        if (context == null || targetId == null)
            return null;
        for (Page page : context.pages()) {
            if (pageId(page).equals(targetId)) {
                return page;
            }
        }
        return null;
    }

    private String pageId(Page page) {
        return "page-" + Integer.toHexString(System.identityHashCode(page));
    }

    /**
     * Resolve a ref string to a Playwright Locator.
     * <p>
     * For role refs (e.g. "e5", "@e5", "ref=e5"):
     * - Looks up stored roleRefs from the last snapshot
     * - Resolves to getByRole(role, {name, exact:true}).nth(n)
     * - Supports frameSelector scoping
     * <p>
     * For other refs: XPath (//...), CSS (#, ., [), text= prefix, or plain text match.
     */
    private Locator resolveRef(Page page, String ref) {
        // Normalize ref: strip @ or ref= prefix
        String normalized = ref;
        if (normalized.startsWith("@")) {
            normalized = normalized.substring(1);
        } else if (normalized.startsWith("ref=")) {
            normalized = normalized.substring(4);
        }

        // Role ref pattern: e1, e2, e123, etc.
        if (normalized.matches("^e\\d+$")) {
            PageState state = PageState.getPageState(page);

            // If using aria mode, use aria-ref locator
            if (state != null && "aria".equals(state.getStoredRefsMode())) {
                String frameSelector = state.getStoredFrameSelector();
                if (frameSelector != null && !frameSelector.isEmpty()) {
                    return page.frameLocator(frameSelector)
                            .locator("[aria-ref=\"" + normalized + "\"]");
                }
                // Use internal Playwright snapshot ref syntax
                return page.locator("internal:attr=[__playwright_ref=\"" + normalized + "\"]");
            }

            // Role mode: look up stored refs
            if (state != null && state.getStoredRoleRefs() != null) {
                @SuppressWarnings("unchecked")
                Map<String, com.openclaw.browser.snapshot.RoleSnapshot.RoleRef> refs =
                        (Map<String, com.openclaw.browser.snapshot.RoleSnapshot.RoleRef>)
                                (Map<String, ?>) state.getStoredRoleRefs();
                com.openclaw.browser.snapshot.RoleSnapshot.RoleRef info = refs.get(normalized);
                if (info != null) {
                    String frameSelector = state.getStoredFrameSelector();
                    // Build getByRole locator
                    com.microsoft.playwright.options.AriaRole ariaRole = parseAriaRole(info.getRole());
                    Locator locator;
                    if (frameSelector != null && !frameSelector.isEmpty()) {
                        FrameLocator frame = page.frameLocator(frameSelector);
                        if (info.getName() != null) {
                            locator = frame.getByRole(ariaRole,
                                    new FrameLocator.GetByRoleOptions().setName(info.getName()).setExact(true));
                        } else {
                            locator = frame.getByRole(ariaRole);
                        }
                    } else {
                        if (info.getName() != null) {
                            locator = page.getByRole(ariaRole,
                                    new Page.GetByRoleOptions().setName(info.getName()).setExact(true));
                        } else {
                            locator = page.getByRole(ariaRole);
                        }
                    }
                    // Apply nth if needed
                    if (info.getNth() != null && info.getNth() > 0) {
                        locator = locator.nth(info.getNth());
                    }
                    return locator;
                }
            }

            // No stored refs — error with guidance
            throw new IllegalArgumentException(
                    "Unknown ref \"" + normalized + "\". Run a new snapshot and use a ref from that snapshot.");
        }

        // Non-role refs: XPath, CSS, text
        if (normalized.startsWith("//")) {
            return page.locator("xpath=" + normalized);
        }
        if (normalized.startsWith("#") || normalized.startsWith(".") || normalized.startsWith("[")) {
            return page.locator(normalized);
        }
        if (normalized.startsWith("text=")) {
            return page.getByText(normalized.substring(5));
        }
        return page.getByText(normalized).first();
    }

    /**
     * Parse an ARIA role string to Playwright's AriaRole enum.
     */
    private static com.microsoft.playwright.options.AriaRole parseAriaRole(String role) {
        if (role == null) return com.microsoft.playwright.options.AriaRole.GENERIC;
        return switch (role.toLowerCase()) {
            case "button" -> com.microsoft.playwright.options.AriaRole.BUTTON;
            case "link" -> com.microsoft.playwright.options.AriaRole.LINK;
            case "textbox" -> com.microsoft.playwright.options.AriaRole.TEXTBOX;
            case "checkbox" -> com.microsoft.playwright.options.AriaRole.CHECKBOX;
            case "radio" -> com.microsoft.playwright.options.AriaRole.RADIO;
            case "combobox" -> com.microsoft.playwright.options.AriaRole.COMBOBOX;
            case "listbox" -> com.microsoft.playwright.options.AriaRole.LISTBOX;
            case "menuitem" -> com.microsoft.playwright.options.AriaRole.MENUITEM;
            case "menuitemcheckbox" -> com.microsoft.playwright.options.AriaRole.MENUITEMCHECKBOX;
            case "menuitemradio" -> com.microsoft.playwright.options.AriaRole.MENUITEMRADIO;
            case "option" -> com.microsoft.playwright.options.AriaRole.OPTION;
            case "searchbox" -> com.microsoft.playwright.options.AriaRole.SEARCHBOX;
            case "slider" -> com.microsoft.playwright.options.AriaRole.SLIDER;
            case "spinbutton" -> com.microsoft.playwright.options.AriaRole.SPINBUTTON;
            case "switch" -> com.microsoft.playwright.options.AriaRole.SWITCH;
            case "tab" -> com.microsoft.playwright.options.AriaRole.TAB;
            case "treeitem" -> com.microsoft.playwright.options.AriaRole.TREEITEM;
            case "heading" -> com.microsoft.playwright.options.AriaRole.HEADING;
            case "cell" -> com.microsoft.playwright.options.AriaRole.CELL;
            case "gridcell" -> com.microsoft.playwright.options.AriaRole.GRIDCELL;
            case "columnheader" -> com.microsoft.playwright.options.AriaRole.COLUMNHEADER;
            case "rowheader" -> com.microsoft.playwright.options.AriaRole.ROWHEADER;
            case "listitem" -> com.microsoft.playwright.options.AriaRole.LISTITEM;
            case "article" -> com.microsoft.playwright.options.AriaRole.ARTICLE;
            case "region" -> com.microsoft.playwright.options.AriaRole.REGION;
            case "main" -> com.microsoft.playwright.options.AriaRole.MAIN;
            case "navigation" -> com.microsoft.playwright.options.AriaRole.NAVIGATION;
            case "img" -> com.microsoft.playwright.options.AriaRole.IMG;
            case "dialog" -> com.microsoft.playwright.options.AriaRole.DIALOG;
            case "table" -> com.microsoft.playwright.options.AriaRole.TABLE;
            case "row" -> com.microsoft.playwright.options.AriaRole.ROW;
            case "group" -> com.microsoft.playwright.options.AriaRole.GROUP;
            case "list" -> com.microsoft.playwright.options.AriaRole.LIST;
            case "tree" -> com.microsoft.playwright.options.AriaRole.TREE;
            case "menu" -> com.microsoft.playwright.options.AriaRole.MENU;
            case "menubar" -> com.microsoft.playwright.options.AriaRole.MENUBAR;
            case "toolbar" -> com.microsoft.playwright.options.AriaRole.TOOLBAR;
            case "tablist" -> com.microsoft.playwright.options.AriaRole.TABLIST;
            default -> com.microsoft.playwright.options.AriaRole.GENERIC;
        };
    }

    private void setupPageListeners(Page page) {
        List<ConsoleEntry> entries = new CopyOnWriteArrayList<>();
        pageConsoleMessages.put(page, entries);

        // Initialize PageState for error/request tracking
        PageState state = PageState.ensurePageState(page);

        // Console messages
        page.onConsoleMessage(msg -> {
            if (entries.size() >= MAX_CONSOLE_MESSAGES) {
                entries.remove(0);
            }
            entries.add(new ConsoleEntry(msg.type(), msg.text(), System.currentTimeMillis()));
        });

        // Page errors (uncaught exceptions)
        page.onPageError(errorMsg -> {
            state.addError(new PageState.PageError(errorMsg, page.url()));
        });

        // Network request lifecycle tracking
        // Track request start
        page.onRequest(request -> {
            try {
                String reqId = PageState.bumpRequestId();
                state.trackRequestStart(reqId, request);
                state.addRequest(new PageState.NetworkRequest(
                        reqId,
                        request.method(),
                        request.url(),
                        0,  // status unknown at start
                        request.resourceType(),
                        false,
                        null
                ));
            } catch (Exception e) {
                // Ignore detached request errors
            }
        });

        // Track request completion with status
        page.onResponse(response -> {
            try {
                state.updateRequestStatus(
                        response.request(),
                        response.status(),
                        response.ok()
                );
            } catch (Exception e) {
                // Ignore detached response errors
            }
        });

        // Track request failures
        page.onRequestFailed(request -> {
            try {
                String failure = request.failure();
                state.updateRequestFailure(request, failure != null ? failure : "net::ERR_FAILED");
            } catch (Exception e) {
                // Ignore detached request errors
            }
        });

        // Auto-dismiss dialogs to prevent blocking
        page.onDialog(dialog -> {
            log.debug("Auto-dismissing dialog: type={}, message={}", dialog.type(), dialog.message());
            dialog.dismiss();
        });

        // Track popups (new windows/tabs opened by page)
        page.onPopup(popup -> {
            log.debug("Popup detected: {}", popup.url());
            setupPageListeners(popup);
        });
    }

    // =========================================================================
    // Error helpers
    // =========================================================================

    /**
     * Convert Playwright exceptions to AI-friendly messages.
     */
    private static String toAiFriendlyError(PlaywrightException e) {
        String msg = e.getMessage();
        if (msg == null)
            return "Unknown browser error";

        // Timeout errors
        if (msg.contains("Timeout") || msg.contains("timeout")) {
            if (msg.contains("waiting for locator")) {
                // Extract just the locator part
                int idx = msg.indexOf("waiting for locator");
                String locPart = idx >= 0 ? msg.substring(idx) : msg;
                // Truncate to first meaningful line
                int nl = locPart.indexOf('\n');
                return nl > 0 ? locPart.substring(0, nl) : locPart;
            }
            return "Timeout: element not found or not actionable in time";
        }

        // Element not found
        if (msg.contains("strict mode violation")) {
            return "Multiple elements match the ref. Use a more specific selector.";
        }

        // Not visible / not attached
        if (msg.contains("not visible") || msg.contains("Element is not visible")) {
            return "Element exists but is not visible. Try scrollIntoView first or wait for it.";
        }
        if (msg.contains("Element is outside") || msg.contains("intercepts pointer")) {
            return "Element is covered by another element. Close overlays/modals first.";
        }

        // Keep first line only for readability
        int nl = msg.indexOf('\n');
        return nl > 0 ? msg.substring(0, nl) : msg;
    }

    // =========================================================================
    // Param helpers
    // =========================================================================

    private static String requireParam(Map<String, Object> params, String key) {
        String v = getStr(params, key);
        if (v == null || v.isBlank()) {
            throw new IllegalArgumentException(key + " is required");
        }
        return v;
    }

    private static String getStr(Map<String, Object> params, String key) {
        Object v = params.get(key);
        return v != null ? v.toString() : null;
    }

    private static boolean getBool(Map<String, Object> params, String key) {
        Object v = params.get(key);
        if (v instanceof Boolean b)
            return b;
        if (v instanceof String s)
            return "true".equalsIgnoreCase(s);
        return false;
    }

    private static Integer getInt(Map<String, Object> params, String key) {
        Object v = params.get(key);
        if (v instanceof Number n)
            return n.intValue();
        if (v instanceof String s) {
            try {
                return Integer.parseInt(s);
            } catch (NumberFormatException e) {
                return null;
            }
        }
        return null;
    }

    /**
     * Clamp timeout to [500, 60000]ms range with a fallback default.
     */
    private static int clampTimeout(Integer timeoutMs, int defaultMs) {
        int ms = timeoutMs != null ? timeoutMs : defaultMs;
        return Math.max(500, Math.min(60000, ms));
    }

    /**
     * Parse modifier keys list.
     */
    private static List<KeyboardModifier> parseModifiers(List<String> modList) {
        if (modList == null || modList.isEmpty()) return List.of();
        List<KeyboardModifier> mods = new ArrayList<>();
        for (String m : modList) {
            switch (m) {
                case "Alt" -> mods.add(KeyboardModifier.ALT);
                case "Control" -> mods.add(KeyboardModifier.CONTROL);
                case "ControlOrMeta" -> mods.add(KeyboardModifier.CONTROLORMETA);
                case "Meta" -> mods.add(KeyboardModifier.META);
                case "Shift" -> mods.add(KeyboardModifier.SHIFT);
            }
        }
        return mods;
    }

    // =========================================================================
    // Page error/request access
    // =========================================================================

    /**
     * Get page errors (uncaught exceptions).
     */
    public List<PageState.PageError> getPageErrors(String targetId, boolean clear) {
        Page page = resolveTargetPage(targetId);
        PageState state = PageState.ensurePageState(page);
        List<PageState.PageError> errors = new ArrayList<>(state.getErrors());
        if (clear) state.clearErrors();
        return errors;
    }

    /**
     * Get network requests.
     */
    public List<PageState.NetworkRequest> getNetworkRequests(String targetId, String filter, boolean clear) {
        Page page = resolveTargetPage(targetId);
        PageState state = PageState.ensurePageState(page);
        List<PageState.NetworkRequest> requests = new ArrayList<>(state.getRequests());
        if (filter != null && !filter.isBlank()) {
            requests = requests.stream()
                    .filter(r -> r.getUrl().contains(filter))
                    .toList();
        }
        if (clear) state.clearRequests();
        return requests;
    }

    // =========================================================================
    // Async event arming (file upload, dialog, download)
    // =========================================================================

    /**
     * Pre-register a file upload handler.
     * When a file chooser dialog appears, it will be auto-filled with the given paths.
     * Corresponds to TS armFileUploadViaPlaywright.
     */
    public void armFileUpload(String targetId, List<String> paths, int timeoutMs) {
        Page page = resolveTargetPage(targetId);
        PageState state = PageState.ensurePageState(page);
        int armId = PwToolsShared.bumpUploadArmId();
        state.setArmIdUpload(armId);

        int timeout = Math.max(500, Math.min(120_000, timeoutMs > 0 ? timeoutMs : 120_000));

        // Run in background thread to not block
        Thread bgThread = new Thread(() -> {
            try {
                FileChooser fileChooser = page.waitForFileChooser(
                        new Page.WaitForFileChooserOptions().setTimeout(timeout),
                        () -> {});
                // Check the arm is still active (not superseded)
                if (state.getArmIdUpload() != armId) return;

                if (paths == null || paths.isEmpty()) {
                    // Cancel: press Escape
                    try { page.keyboard().press("Escape"); } catch (Exception ignored) {}
                    return;
                }
                java.nio.file.Path[] filePaths = paths.stream()
                        .map(java.nio.file.Path::of)
                        .toArray(java.nio.file.Path[]::new);
                fileChooser.setFiles(filePaths);
            } catch (Exception e) {
                log.debug("armFileUpload waiter completed (timeout or cancelled): {}", e.getMessage());
            }
        });
        bgThread.setDaemon(true);
        bgThread.setName("arm-file-upload-" + armId);
        bgThread.start();
    }

    /**
     * Pre-register a dialog handler.
     * When a dialog appears, it will be auto-accepted or dismissed.
     * Corresponds to TS armDialogViaPlaywright.
     */
    public void armDialog(String targetId, boolean accept, String promptText, int timeoutMs) {
        Page page = resolveTargetPage(targetId);
        PageState state = PageState.ensurePageState(page);
        int armId = PwToolsShared.bumpDialogArmId();
        state.setArmIdDialog(armId);

        int timeout = Math.max(500, Math.min(120_000, timeoutMs > 0 ? timeoutMs : 120_000));

        // Remove existing auto-dismiss listener and install custom one
        // Note: Playwright Java doesn't support removing specific listeners easily,
        // so we use the PageState armId to guard against stale handlers.
        page.onDialog(dialog -> {
            if (state.getArmIdDialog() != armId) {
                // This arm was superseded, auto-dismiss
                dialog.dismiss();
                return;
            }
            try {
                if (accept) {
                    dialog.accept(promptText);
                } else {
                    dialog.dismiss();
                }
            } catch (Exception e) {
                log.debug("Dialog handling failed: {}", e.getMessage());
            }
        });
    }

    /**
     * Wait for a network response matching a URL pattern and return its body.
     * Corresponds to TS responseBodyViaPlaywright.
     */
    public Map<String, Object> responseBody(String targetId, String urlPattern, int timeoutMs, int maxChars) {
        Page page = resolveTargetPage(targetId);
        String pattern = urlPattern != null ? urlPattern.trim() : "";
        if (pattern.isEmpty()) {
            throw new IllegalArgumentException("url is required");
        }
        int timeout = Math.max(500, Math.min(120_000, timeoutMs > 0 ? timeoutMs : 20_000));
        int maxC = maxChars > 0 ? Math.min(5_000_000, maxChars) : 200_000;

        // Wait for matching response
        Response response = page.waitForResponse(
                resp -> matchUrlPattern(pattern, resp.url()),
                new Page.WaitForResponseOptions().setTimeout(timeout),
                () -> {});

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("url", response.url());
        result.put("status", response.status());

        // Get headers
        Map<String, String> headers = response.headers();
        result.put("headers", headers);

        // Get body
        try {
            String bodyText = new String(response.body(), java.nio.charset.StandardCharsets.UTF_8);
            boolean truncated = bodyText.length() > maxC;
            result.put("body", truncated ? bodyText.substring(0, maxC) : bodyText);
            if (truncated) result.put("truncated", true);
        } catch (Exception e) {
            result.put("body", null);
            result.put("error", "Failed to read response body: " + e.getMessage());
        }

        return result;
    }

    /**
     * Get console messages, optionally filtered by level.
     */
    public List<ConsoleEntry> getConsoleMessages(String targetId, String level) {
        Page page = resolveTargetPage(targetId);
        List<ConsoleEntry> entries = pageConsoleMessages.get(page);
        if (entries == null) return List.of();
        if (level == null || level.isBlank()) return new ArrayList<>(entries);

        int minPriority = consolePriority(level);
        return entries.stream()
                .filter(e -> consolePriority(e.getType()) >= minPriority)
                .toList();
    }

    /**
     * URL pattern matching (supports * glob).
     */
    private static boolean matchUrlPattern(String pattern, String url) {
        if (pattern.equals(url)) return true;
        if (pattern.contains("*")) {
            String regex = pattern.replaceAll("[|\\\\{}()\\[\\]^$+?.]", "\\\\$0")
                    .replace("**", ".*")
                    .replace("*", ".*");
            return url.matches("^" + regex + "$");
        }
        return url.contains(pattern);
    }

    /**
     * Console message priority for filtering.
     */
    private static int consolePriority(String level) {
        return switch (level != null ? level : "") {
            case "error" -> 3;
            case "warning" -> 2;
            case "info", "log" -> 1;
            case "debug" -> 0;
            default -> 1;
        };
    }
}

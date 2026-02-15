package com.openclaw.agent.tools.builtin.browser;

import com.microsoft.playwright.*;
import com.microsoft.playwright.options.ScreenshotType;
import com.microsoft.playwright.options.WaitForSelectorState;
import com.microsoft.playwright.options.WaitUntilState;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

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
                    if (doubleClick) {
                        loc.dblclick(new Locator.DblclickOptions().setTimeout(timeout));
                    } else {
                        loc.click(new Locator.ClickOptions().setTimeout(timeout));
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

    private Page resolveTargetPage(String targetId) {
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
     * Supports: XPath (//...), CSS (#, ., [), text= prefix, or plain text match.
     */
    private Locator resolveRef(Page page, String ref) {
        if (ref.startsWith("//")) {
            return page.locator("xpath=" + ref);
        }
        if (ref.startsWith("#") || ref.startsWith(".") || ref.startsWith("[")) {
            return page.locator(ref);
        }
        if (ref.startsWith("text=")) {
            return page.getByText(ref.substring(5));
        }
        return page.getByText(ref).first();
    }

    private void setupPageListeners(Page page) {
        List<ConsoleEntry> entries = new CopyOnWriteArrayList<>();
        pageConsoleMessages.put(page, entries);

        // Console messages
        page.onConsoleMessage(msg -> {
            if (entries.size() >= MAX_CONSOLE_MESSAGES) {
                entries.remove(0);
            }
            entries.add(new ConsoleEntry(msg.type(), msg.text(), System.currentTimeMillis()));
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
}

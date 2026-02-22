package com.openclaw.browser.playwright;

import com.microsoft.playwright.*;
import com.openclaw.browser.BrowserConfig;
import com.openclaw.browser.cdp.CdpHelpers;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Enhanced Playwright session manager — persistent connections, page state, and ref locators.
 * Corresponds to TypeScript's pw-session.ts.
 */
@Slf4j
public class PlaywrightSession implements AutoCloseable {

    @Getter private final BrowserConfig.ResolvedBrowserConfig config;
    @Getter private Playwright playwright;
    @Getter private Browser browser;
    @Getter private BrowserContext context;
    @Getter private Page currentPage;

    private final ConcurrentHashMap<Page, PwToolsCore.PageState> pageStates = new ConcurrentHashMap<>();
    private final CopyOnWriteArrayList<String> consoleLog = new CopyOnWriteArrayList<>();

    // Limits
    private static final int MAX_CONSOLE_MESSAGES = 200;
    private static final int MAX_ERRORS = 100;
    private static final int MAX_NETWORK_REQUESTS = 500;

    public PlaywrightSession(BrowserConfig.ResolvedBrowserConfig config) {
        this.config = config;
    }

    // ==================== Connection ====================

    /**
     * Connect to the browser via CDP.
     *
     * @param profile Resolved profile with CDP URL
     */
    public void connect(BrowserConfig.ResolvedBrowserProfile profile) {
        if (playwright == null) {
            playwright = Playwright.create();
        }

        String cdpUrl = profile.getCdpUrl();
        log.info("Connecting Playwright to CDP: {}", cdpUrl);

        // Add relay auth headers if available
        Map<String, String> headers = CdpHelpers.getAuthHeaders(cdpUrl);

        browser = playwright.chromium().connectOverCDP(cdpUrl,
                new BrowserType.ConnectOverCDPOptions()
                        .setHeaders(headers));

        // Use existing context and page if available
        List<BrowserContext> contexts = browser.contexts();
        if (!contexts.isEmpty()) {
            context = contexts.get(0);
            List<Page> pages = context.pages();
            if (!pages.isEmpty()) {
                currentPage = pages.get(0);
            }
        }

        if (context == null) {
            context = browser.newContext();
        }
        if (currentPage == null) {
            currentPage = context.newPage();
        }

        // Attach state tracking
        trackPage(currentPage);

        log.info("Playwright session connected (pages: {})",
                context.pages().size());
    }

    /**
     * Check if the session is connected.
     */
    public boolean isConnected() {
        return browser != null && browser.isConnected();
    }

    // ==================== Page management ====================

    /**
     * Get the active page or create one if needed.
     */
    public Page ensurePage() {
        if (currentPage != null && !currentPage.isClosed()) {
            return currentPage;
        }
        if (context != null) {
            List<Page> pages = context.pages();
            if (!pages.isEmpty()) {
                currentPage = pages.get(0);
                trackPage(currentPage);
                return currentPage;
            }
            currentPage = context.newPage();
            trackPage(currentPage);
        }
        return currentPage;
    }

    /**
     * Switch to a specific page by index.
     */
    public Page switchToPage(int index) {
        if (context == null) return null;
        List<Page> pages = context.pages();
        if (index < 0 || index >= pages.size()) return null;
        currentPage = pages.get(index);
        if (!pageStates.containsKey(currentPage)) {
            trackPage(currentPage);
        }
        return currentPage;
    }

    /**
     * Create a new page in the current context.
     */
    public Page newPage() {
        if (context == null) return null;
        currentPage = context.newPage();
        trackPage(currentPage);
        return currentPage;
    }

    /**
     * Get the page state for the current page.
     */
    public PwToolsCore.PageState getPageState() {
        return currentPage != null ? pageStates.get(currentPage) : null;
    }

    /**
     * Get all page states.
     */
    public Map<Page, PwToolsCore.PageState> getAllPageStates() {
        return Map.copyOf(pageStates);
    }

    // ==================== Screenshot ====================

    /**
     * Capture a screenshot of the current page.
     */
    public byte[] screenshot(boolean fullPage) {
        Page page = ensurePage();
        if (page == null) return new byte[0];
        return page.screenshot(new Page.ScreenshotOptions()
                .setFullPage(fullPage));
    }

    /**
     * Capture a screenshot as JPEG with quality.
     */
    public byte[] screenshotJpeg(boolean fullPage, int quality) {
        Page page = ensurePage();
        if (page == null) return new byte[0];
        return page.screenshot(new Page.ScreenshotOptions()
                .setFullPage(fullPage)
                .setType(com.microsoft.playwright.options.ScreenshotType.JPEG)
                .setQuality(quality));
    }

    // ==================== Lifecycle ====================

    @Override
    public void close() {
        pageStates.clear();
        try { if (context != null) context.close(); } catch (Exception e) { /* ignore */ }
        try { if (browser != null) browser.close(); } catch (Exception e) { /* ignore */ }
        try { if (playwright != null) playwright.close(); } catch (Exception e) { /* ignore */ }
        context = null;
        browser = null;
        playwright = null;
        currentPage = null;
        log.info("Playwright session closed");
    }

    // ==================== Private ====================

    private void trackPage(Page page) {
        if (page == null || pageStates.containsKey(page)) return;

        PwToolsCore.PageState state = PwToolsCore.trackPageState(
                page, MAX_CONSOLE_MESSAGES, MAX_ERRORS, MAX_NETWORK_REQUESTS);
        pageStates.put(page, state);

        page.onClose(p -> {
            pageStates.remove(p);
            if (p == currentPage) {
                currentPage = null;
            }
        });
    }
}

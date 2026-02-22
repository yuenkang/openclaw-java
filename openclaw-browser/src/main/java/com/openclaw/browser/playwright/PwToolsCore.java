package com.openclaw.browser.playwright;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.playwright.*;
import com.microsoft.playwright.options.Cookie;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Core Playwright tools — interactions, snapshots, state, storage, tracing.
 * Corresponds to TypeScript's pw-tools-core.*.ts files.
 */
@Slf4j
public class PwToolsCore {

    private static final ObjectMapper mapper = new ObjectMapper();

    // ==================== Snapshot ====================

    /**
     * Get accessibility snapshot of the current page.
     * Uses Playwright's ariaSnapshot() on the body locator.
     */
    public static String getAccessibilitySnapshot(Page page) {
        try {
            return page.locator("body").ariaSnapshot();
        } catch (Exception e) {
            log.debug("Accessibility snapshot failed: {}", e.getMessage());
            return "[]";
        }
    }

    /**
     * Get page content as HTML.
     */
    public static String getPageContent(Page page) {
        try {
            return page.content();
        } catch (Exception e) {
            log.debug("Page content failed: {}", e.getMessage());
            return "";
        }
    }

    /**
     * Get inner text of the page body.
     */
    public static String getPageText(Page page) {
        try {
            return page.innerText("body");
        } catch (Exception e) {
            log.debug("Page text failed: {}", e.getMessage());
            return "";
        }
    }

    // ==================== Interactions ====================

    /**
     * Click on an element by selector.
     */
    public static void click(Page page, String selector, Map<String, Object> options) {
        Page.ClickOptions opts = new Page.ClickOptions();
        if (options != null) {
            if (options.containsKey("button")) {
                String button = String.valueOf(options.get("button"));
                opts.setButton(com.microsoft.playwright.options.MouseButton.valueOf(
                        button.toUpperCase()));
            }
            if (options.containsKey("clickCount")) {
                opts.setClickCount(((Number) options.get("clickCount")).intValue());
            }
            if (options.containsKey("delay")) {
                opts.setDelay(((Number) options.get("delay")).doubleValue());
            }
            if (options.containsKey("force")) {
                opts.setForce((Boolean) options.get("force"));
            }
            if (options.containsKey("timeout")) {
                opts.setTimeout(((Number) options.get("timeout")).doubleValue());
            }
        }
        page.click(selector, opts);
    }

    /**
     * Type text into an element.
     */
    public static void type(Page page, String selector, String text, int delayMs) {
        page.locator(selector).pressSequentially(text,
                new Locator.PressSequentiallyOptions().setDelay(delayMs));
    }

    /**
     * Fill a form field.
     */
    public static void fill(Page page, String selector, String value) {
        page.fill(selector, value);
    }

    /**
     * Press a key or key combination.
     */
    public static void press(Page page, String selector, String key) {
        if (selector != null && !selector.isBlank()) {
            page.locator(selector).press(key);
        } else {
            page.keyboard().press(key);
        }
    }

    /**
     * Focus an element.
     */
    public static void focus(Page page, String selector) {
        page.focus(selector);
    }

    /**
     * Select options in a select element.
     */
    public static List<String> selectOption(Page page, String selector, String value) {
        return page.selectOption(selector, value);
    }

    /**
     * Check a checkbox or radio.
     */
    public static void check(Page page, String selector) {
        page.check(selector);
    }

    /**
     * Uncheck a checkbox.
     */
    public static void uncheck(Page page, String selector) {
        page.uncheck(selector);
    }

    /**
     * Hover over an element.
     */
    public static void hover(Page page, String selector) {
        page.hover(selector);
    }

    /**
     * Scroll an element into view.
     */
    public static void scrollIntoView(Page page, String selector) {
        page.locator(selector).scrollIntoViewIfNeeded();
    }

    /**
     * Wait for a selector to appear.
     */
    public static void waitForSelector(Page page, String selector, int timeoutMs) {
        page.waitForSelector(selector,
                new Page.WaitForSelectorOptions().setTimeout(timeoutMs));
    }

    // ==================== State ====================

    /**
     * Get cookies for the current context.
     */
    public static List<Cookie> getCookies(BrowserContext context) {
        return context.cookies();
    }

    /**
     * Clear cookies.
     */
    public static void clearCookies(BrowserContext context) {
        context.clearCookies();
    }

    /**
     * Get localStorage entries.
     */
    public static Map<String, String> getLocalStorage(Page page) {
        Map<String, String> storage = new LinkedHashMap<>();
        try {
            Object result = page.evaluate("() => {"
                    + "const items = {};"
                    + "for (let i = 0; i < localStorage.length; i++) {"
                    + "  const key = localStorage.key(i);"
                    + "  items[key] = localStorage.getItem(key);"
                    + "}"
                    + "return items;"
                    + "}");
            if (result instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> map = (Map<String, Object>) result;
                map.forEach((k, v) -> storage.put(k, v != null ? v.toString() : null));
            }
        } catch (Exception e) {
            log.debug("getLocalStorage failed: {}", e.getMessage());
        }
        return storage;
    }

    /**
     * Set localStorage entries.
     */
    public static void setLocalStorage(Page page, Map<String, String> entries) {
        for (Map.Entry<String, String> entry : entries.entrySet()) {
            page.evaluate("([k, v]) => localStorage.setItem(k, v)",
                    List.of(entry.getKey(), entry.getValue()));
        }
    }

    /**
     * Clear localStorage.
     */
    public static void clearLocalStorage(Page page) {
        page.evaluate("() => localStorage.clear()");
    }

    // ==================== Downloads ====================

    /**
     * Wait for a download to start after performing an action.
     */
    public static Download waitForDownload(Page page, Runnable action) {
        return page.waitForDownload(action);
    }

    /**
     * Save a download to a specific path.
     */
    public static void saveDownload(Download download, Path path) {
        download.saveAs(path);
    }

    // ==================== Tracing ====================

    /**
     * Start tracing.
     */
    public static void startTracing(BrowserContext context, boolean screenshots, boolean snapshots) {
        context.tracing().start(new Tracing.StartOptions()
                .setScreenshots(screenshots)
                .setSnapshots(snapshots));
    }

    /**
     * Stop tracing and save to file.
     */
    public static void stopTracing(BrowserContext context, Path outputPath) {
        context.tracing().stop(new Tracing.StopOptions().setPath(outputPath));
    }

    // ==================== Navigation helpers ====================

    /**
     * Navigate to a URL and wait for load.
     */
    public static Response navigate(Page page, String url, int timeoutMs) {
        return page.navigate(url,
                new Page.NavigateOptions().setTimeout(timeoutMs));
    }

    /**
     * Go back in history.
     */
    public static Response goBack(Page page, int timeoutMs) {
        return page.goBack(new Page.GoBackOptions().setTimeout(timeoutMs));
    }

    /**
     * Go forward in history.
     */
    public static Response goForward(Page page, int timeoutMs) {
        return page.goForward(new Page.GoForwardOptions().setTimeout(timeoutMs));
    }

    /**
     * Reload the page.
     */
    public static Response reload(Page page, int timeoutMs) {
        return page.reload(new Page.ReloadOptions().setTimeout(timeoutMs));
    }

    // ==================== Page state tracking ====================

    /**
     * Tracked state for a Playwright page — console logs, network requests, errors.
     */
    @Data
    @Builder
    public static class PageState {
        private final List<String> consoleMessages;
        private final List<String> errors;
        private final List<String> networkRequests;
        private final long createdAt;

        public static PageState create() {
            return PageState.builder()
                    .consoleMessages(new CopyOnWriteArrayList<>())
                    .errors(new CopyOnWriteArrayList<>())
                    .networkRequests(new CopyOnWriteArrayList<>())
                    .createdAt(System.currentTimeMillis())
                    .build();
        }
    }

    /**
     * Attach event listeners to track page state.
     * Returns the PageState object that will accumulate events.
     */
    public static PageState trackPageState(Page page, int maxConsoleMessages,
                                           int maxErrors, int maxNetworkRequests) {
        PageState state = PageState.create();

        page.onConsoleMessage(msg -> {
            if (state.getConsoleMessages().size() < maxConsoleMessages) {
                state.getConsoleMessages().add(
                        "[" + msg.type() + "] " + msg.text());
            }
        });

        page.onPageError(error -> {
            if (state.getErrors().size() < maxErrors) {
                state.getErrors().add(error);
            }
        });

        page.onRequest(request -> {
            if (state.getNetworkRequests().size() < maxNetworkRequests) {
                state.getNetworkRequests().add(
                        request.method() + " " + request.url());
            }
        });

        return state;
    }

    // ==================== Dialog handling ====================

    /**
     * Accept a dialog (alert, confirm, prompt).
     */
    public static void acceptDialog(Page page, String promptText) {
        page.onDialog(dialog -> {
            if (promptText != null) {
                dialog.accept(promptText);
            } else {
                dialog.accept();
            }
        });
    }

    /**
     * Dismiss a dialog.
     */
    public static void dismissDialog(Page page) {
        page.onDialog(Dialog::dismiss);
    }
}

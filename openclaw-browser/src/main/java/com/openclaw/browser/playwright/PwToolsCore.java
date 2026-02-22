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

    // ==================== State (pw-tools-core.state.ts) ====================

    /**
     * Set offline mode for the browser context.
     */
    public static void setOffline(BrowserContext context, boolean offline) {
        context.setOffline(offline);
    }

    /**
     * Set extra HTTP headers.
     */
    public static void setExtraHTTPHeaders(BrowserContext context, Map<String, String> headers) {
        context.setExtraHTTPHeaders(headers);
    }

    /**
     * Set HTTP credentials for the context.
     * Note: Playwright Java API does not expose setHTTPCredentials directly.
     * We implement this by routing requests through a handler that adds Authorization headers.
     */
    public static void setHttpCredentials(BrowserContext context, String username, String password,
                                          boolean clear) {
        if (clear) {
            // Remove any previously set auth routes
            context.unrouteAll();
            return;
        }
        if (username == null || username.isEmpty()) {
            throw new IllegalArgumentException("username is required (or set clear=true)");
        }
        String credentials = username + ":" + (password != null ? password : "");
        String encoded = java.util.Base64.getEncoder().encodeToString(credentials.getBytes());
        context.route("**/*", route -> {
            Map<String, String> headers = new LinkedHashMap<>(route.request().headers());
            headers.put("authorization", "Basic " + encoded);
            route.resume(new com.microsoft.playwright.Route.ResumeOptions()
                    .setHeaders(headers));
        });
    }

    /**
     * Set geolocation.
     */
    public static void setGeolocation(BrowserContext context, Double latitude, Double longitude,
                                       Double accuracy, boolean clear) {
        if (clear) {
            context.setGeolocation(null);
            try {
                context.clearPermissions();
            } catch (Exception ignored) {
            }
            return;
        }
        if (latitude == null || longitude == null) {
            throw new IllegalArgumentException("latitude and longitude required (or set clear=true)");
        }
        var geo = new com.microsoft.playwright.options.Geolocation(latitude, longitude);
        if (accuracy != null) {
            geo.setAccuracy(accuracy);
        }
        context.setGeolocation(geo);
    }

    /**
     * Emulate media color scheme.
     */
    public static void emulateMedia(Page page, String colorScheme) {
        var opts = new Page.EmulateMediaOptions();
        if (colorScheme != null) {
            opts.setColorScheme(switch (colorScheme.toLowerCase()) {
                case "dark" -> com.microsoft.playwright.options.ColorScheme.DARK;
                case "light" -> com.microsoft.playwright.options.ColorScheme.LIGHT;
                case "no-preference" -> com.microsoft.playwright.options.ColorScheme.NO_PREFERENCE;
                default -> null;
            });
        }
        page.emulateMedia(opts);
    }

    /**
     * Resize viewport.
     */
    public static void resizeViewport(Page page, int width, int height) {
        page.setViewportSize(Math.max(1, width), Math.max(1, height));
    }

    // ==================== Storage (pw-tools-core.storage.ts) ====================

    /**
     * Set a cookie in the browser context.
     */
    public static void setCookie(BrowserContext context, Map<String, Object> cookieData) {
        String name = (String) cookieData.get("name");
        String value = (String) cookieData.get("value");
        if (name == null || value == null) {
            throw new IllegalArgumentException("cookie name and value are required");
        }
        var cookie = new Cookie(name, value);
        if (cookieData.containsKey("url")) cookie.setUrl((String) cookieData.get("url"));
        if (cookieData.containsKey("domain")) cookie.setDomain((String) cookieData.get("domain"));
        if (cookieData.containsKey("path")) cookie.setPath((String) cookieData.get("path"));
        if (cookieData.containsKey("expires"))
            cookie.setExpires(((Number) cookieData.get("expires")).doubleValue());
        if (cookieData.containsKey("httpOnly"))
            cookie.setHttpOnly((Boolean) cookieData.get("httpOnly"));
        if (cookieData.containsKey("secure")) cookie.setSecure((Boolean) cookieData.get("secure"));
        if (cookieData.containsKey("sameSite")) {
            String ss = (String) cookieData.get("sameSite");
            cookie.setSameSite(com.microsoft.playwright.options.SameSiteAttribute.valueOf(
                    ss.toUpperCase()));
        }
        context.addCookies(List.of(cookie));
    }

    /**
     * Get sessionStorage entries.
     */
    public static Map<String, String> getSessionStorage(Page page) {
        Map<String, String> storage = new LinkedHashMap<>();
        try {
            Object result = page.evaluate("() => {"
                    + "const items = {};"
                    + "for (let i = 0; i < sessionStorage.length; i++) {"
                    + "  const key = sessionStorage.key(i);"
                    + "  items[key] = sessionStorage.getItem(key);"
                    + "}"
                    + "return items;"
                    + "}");
            if (result instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> map = (Map<String, Object>) result;
                map.forEach((k, v) -> storage.put(k, v != null ? v.toString() : null));
            }
        } catch (Exception e) {
            log.debug("getSessionStorage failed: {}", e.getMessage());
        }
        return storage;
    }

    /**
     * Set sessionStorage entries.
     */
    public static void setSessionStorage(Page page, Map<String, String> entries) {
        for (Map.Entry<String, String> entry : entries.entrySet()) {
            page.evaluate("([k, v]) => sessionStorage.setItem(k, v)",
                    List.of(entry.getKey(), entry.getValue()));
        }
    }

    /**
     * Clear sessionStorage.
     */
    public static void clearSessionStorage(Page page) {
        page.evaluate("() => sessionStorage.clear()");
    }

    /**
     * Generic storage get — local or session.
     */
    public static Map<String, String> getStorage(Page page, String kind, String key) {
        boolean isSession = "session".equals(kind);
        String storageName = isSession ? "sessionStorage" : "localStorage";
        if (key != null && !key.isEmpty()) {
            // Get single key
            Object val = page.evaluate(
                    "([s, k]) => { const v = window[s].getItem(k); return v === null ? null : v; }",
                    List.of(storageName, key));
            if (val == null) return Map.of();
            return Map.of(key, val.toString());
        }
        return isSession ? getSessionStorage(page) : getLocalStorage(page);
    }

    /**
     * Generic storage set — local or session.
     */
    public static void setStorage(Page page, String kind, String key, String value) {
        boolean isSession = "session".equals(kind);
        String storageName = isSession ? "sessionStorage" : "localStorage";
        page.evaluate("([s, k, v]) => window[s].setItem(k, v)",
                List.of(storageName, key, value));
    }

    /**
     * Generic storage clear — local or session.
     */
    public static void clearStorage(Page page, String kind) {
        boolean isSession = "session".equals(kind);
        String storageName = isSession ? "sessionStorage" : "localStorage";
        page.evaluate("(s) => window[s].clear()", storageName);
    }

    // ==================== Responses (pw-tools-core.responses.ts) ====================

    /**
     * Highlight an element by adding a visual overlay.
     */
    public static void highlight(Page page, String selector) {
        page.evaluate("(sel) => {"
                + "const el = document.querySelector(sel);"
                + "if (!el) return;"
                + "const old = el.style.outline;"
                + "el.style.outline = '3px solid red';"
                + "setTimeout(() => { el.style.outline = old; }, 3000);"
                + "}", selector);
    }

    /**
     * Set input files on a file input element.
     */
    public static void setInputFiles(Page page, String selector, List<Path> files) {
        page.locator(selector).setInputFiles(files.stream()
                .map(Path::toAbsolutePath)
                .toArray(java.nio.file.Path[]::new));
    }

    // ==================== PDF ====================

    /**
     * Save page as PDF. Only works in headless Chromium.
     */
    public static byte[] generatePdf(Page page) {
        return page.pdf(new Page.PdfOptions().setPrintBackground(true));
    }
}

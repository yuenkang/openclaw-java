package com.openclaw.browser.playwright;

import lombok.Data;

import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Per-page runtime state for tracking errors, network requests, role refs, etc.
 * Corresponds to TypeScript's pw-session.ts PageState.
 */
@Data
public class PageState {

    /** Page errors (uncaught exceptions). */
    private final List<PageError> errors = new CopyOnWriteArrayList<>();

    /** Captured network requests. */
    private final List<NetworkRequest> requests = new CopyOnWriteArrayList<>();

    /** Arm IDs for file upload, dialog, download waiters. */
    private volatile int armIdUpload = 0;
    private volatile int armIdDialog = 0;
    private volatile int armIdDownload = 0;

    /** Stored role refs from the last snapshot (for refLocator resolution). */
    private volatile Map<String, Object> storedRoleRefs = null;
    private volatile String storedRefsMode = null;  // "role" or "aria"
    private volatile String storedFrameSelector = null;

    // Max items to keep
    private static final int MAX_ERRORS = 100;
    private static final int MAX_REQUESTS = 500;

    public void addError(PageError error) {
        if (errors.size() >= MAX_ERRORS) {
            errors.remove(0);
        }
        errors.add(error);
    }

    public void addRequest(NetworkRequest request) {
        if (requests.size() >= MAX_REQUESTS) {
            requests.remove(0);
        }
        requests.add(request);
    }

    public void clearErrors() {
        errors.clear();
    }

    public void clearRequests() {
        requests.clear();
    }

    // ===== Inner types =====

    @Data
    public static class PageError {
        private final String message;
        private final String url;
        private final long timestamp;

        public PageError(String message, String url) {
            this.message = message;
            this.url = url;
            this.timestamp = System.currentTimeMillis();
        }
    }

    @Data
    public static class NetworkRequest {
        private final String id;
        private final String method;
        private final String url;
        private int status;
        private final String resourceType;
        private boolean ok;
        private String failureText;
        private final long timestamp;

        public NetworkRequest(String id, String method, String url, int status,
                              String resourceType, boolean ok, String failureText) {
            this.id = id;
            this.method = method;
            this.url = url;
            this.status = status;
            this.resourceType = resourceType;
            this.ok = ok;
            this.failureText = failureText;
            this.timestamp = System.currentTimeMillis();
        }
    }

    // ===== Static page state registry =====

    private static final WeakHashMap<Object, PageState> states = new WeakHashMap<>();
    private static int nextRequestId = 1;

    public static synchronized String bumpRequestId() {
        return "req-" + (nextRequestId++);
    }

    /** Map Playwright Request objects to our request IDs. */
    private final java.util.IdentityHashMap<Object, String> requestIdMap = new java.util.IdentityHashMap<>();

    /**
     * Track the start of a network request.
     */
    public void trackRequestStart(String reqId, Object playwrightRequest) {
        requestIdMap.put(playwrightRequest, reqId);
    }

    /**
     * Update request status when response is received.
     */
    public void updateRequestStatus(Object playwrightRequest, int status, boolean ok) {
        String reqId = requestIdMap.get(playwrightRequest);
        if (reqId == null) return;
        for (NetworkRequest r : requests) {
            if (reqId.equals(r.getId())) {
                r.setStatus(status);
                r.setOk(ok);
                return;
            }
        }
    }

    /**
     * Update request failure text when request fails.
     */
    public void updateRequestFailure(Object playwrightRequest, String failureText) {
        String reqId = requestIdMap.get(playwrightRequest);
        if (reqId == null) return;
        for (NetworkRequest r : requests) {
            if (reqId.equals(r.getId())) {
                r.setFailureText(failureText);
                return;
            }
        }
    }

    /**
     * Get or create page state for a Playwright Page.
     */
    public static synchronized PageState ensurePageState(Object page) {
        return states.computeIfAbsent(page, k -> new PageState());
    }

    /**
     * Get page state if it exists, without creating one.
     */
    public static synchronized PageState getPageState(Object page) {
        return states.get(page);
    }

    /**
     * Store role refs for a page (from snapshot).
     */
    public static void storeRoleRefs(Object page, Map<String, Object> refs,
                                      String mode, String frameSelector) {
        PageState state = ensurePageState(page);
        state.storedRoleRefs = refs;
        state.storedRefsMode = mode;
        state.storedFrameSelector = frameSelector;
    }

    /**
     * Get stored role refs for a page.
     */
    public static Map<String, Object> getStoredRoleRefs(Object page) {
        PageState state = getPageState(page);
        return state != null ? state.storedRoleRefs : null;
    }
}

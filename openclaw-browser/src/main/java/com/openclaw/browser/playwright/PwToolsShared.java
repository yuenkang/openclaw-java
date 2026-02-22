package com.openclaw.browser.playwright;

import com.openclaw.browser.snapshot.RoleSnapshot;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Shared utilities for Playwright tools.
 * Corresponds to TypeScript's pw-tools-core.shared.ts.
 */
public final class PwToolsShared {

    private PwToolsShared() {
    }

    // ===== Arm ID counters =====

    private static final AtomicInteger nextUploadArmId = new AtomicInteger(0);
    private static final AtomicInteger nextDialogArmId = new AtomicInteger(0);
    private static final AtomicInteger nextDownloadArmId = new AtomicInteger(0);

    public static int bumpUploadArmId() {
        return nextUploadArmId.incrementAndGet();
    }

    public static int bumpDialogArmId() {
        return nextDialogArmId.incrementAndGet();
    }

    public static int bumpDownloadArmId() {
        return nextDownloadArmId.incrementAndGet();
    }

    // ===== Ref handling =====

    /**
     * Parse and validate a ref string. Supports role refs (e.g. "e5") and @-prefixed refs.
     */
    public static String requireRef(String value) {
        String raw = value != null ? value.trim() : "";
        if (raw.isEmpty()) {
            throw new IllegalArgumentException("ref is required");
        }
        // Try to parse as role ref (e.g. "e5")
        if (RoleSnapshot.isValidRef(raw)) {
            return raw;
        }
        // Strip @ prefix if present
        if (raw.startsWith("@")) {
            return raw.substring(1);
        }
        return raw;
    }

    // ===== Timeout normalization =====

    /**
     * Clamp timeout to [500, 120000] ms range with a fallback default.
     */
    public static int normalizeTimeoutMs(Integer timeoutMs, int fallback) {
        int value = timeoutMs != null ? timeoutMs : fallback;
        return Math.max(500, Math.min(120_000, value));
    }

    // ===== AI-friendly error messages =====

    private static final Pattern STRICT_MODE_COUNT =
            Pattern.compile("resolved to (\\d+) elements");

    /**
     * Convert Playwright errors into AI-friendly error messages.
     * Maps common failure patterns to actionable suggestions.
     */
    public static RuntimeException toAIFriendlyError(Throwable error, String selector) {
        String message = error.getMessage() != null ? error.getMessage() : String.valueOf(error);

        // Strict mode violation — multiple elements matched
        if (message.contains("strict mode violation")) {
            Matcher m = STRICT_MODE_COUNT.matcher(message);
            String count = m.find() ? m.group(1) : "multiple";
            return new RuntimeException(
                    "Selector \"" + selector + "\" matched " + count + " elements. " +
                    "Run a new snapshot to get updated refs, or use a different ref.");
        }

        // Element not visible / timeout waiting for visibility
        if ((message.contains("Timeout") || message.contains("waiting for"))
                && (message.contains("to be visible") || message.contains("not visible"))) {
            return new RuntimeException(
                    "Element \"" + selector + "\" not found or not visible. " +
                    "Run a new snapshot to see current page elements.");
        }

        // Element not interactable (covered, hidden)
        if (message.contains("intercepts pointer events")
                || message.contains("not visible")
                || message.contains("not receive pointer events")) {
            return new RuntimeException(
                    "Element \"" + selector + "\" is not interactable (hidden or covered). " +
                    "Try scrolling it into view, closing overlays, or re-snapshotting.");
        }

        return error instanceof RuntimeException
                ? (RuntimeException) error
                : new RuntimeException(message, error);
    }

    /**
     * Build a Playwright locator selector string from a role ref.
     * Role refs like "e5" map to internal:role= selectors via Playwright's ariaSnapshot binding.
     * For non-role refs, returns as CSS selector.
     */
    public static String refToSelector(String ref) {
        if (ref == null || ref.isEmpty()) {
            throw new IllegalArgumentException("ref is required");
        }
        if (RoleSnapshot.isValidRef(ref)) {
            // For role-based refs, use internal aria snapshot ref syntax
            return "[ref=\"" + ref + "\"]";
        }
        return ref;
    }
}

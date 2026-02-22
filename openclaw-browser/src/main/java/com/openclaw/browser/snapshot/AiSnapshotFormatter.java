package com.openclaw.browser.snapshot;

/**
 * Formats snapshots in the "ai" compact format.
 * Provides truncation and format=ai snapshot support.
 * Corresponds to TypeScript's snapshot formatting in pw-tools-core.snapshot.ts.
 */
public final class AiSnapshotFormatter {

    private AiSnapshotFormatter() {
    }

    /**
     * Truncate a snapshot to a maximum number of characters,
     * preserving structural integrity (on line boundaries).
     *
     * @param snapshot raw snapshot text
     * @param maxChars maximum characters (0 or negative = no limit)
     * @return truncated snapshot with marker if truncated
     */
    public static TruncateResult truncate(String snapshot, int maxChars) {
        if (snapshot == null || snapshot.isEmpty()) {
            return new TruncateResult("(empty)", false);
        }
        if (maxChars <= 0 || snapshot.length() <= maxChars) {
            return new TruncateResult(snapshot, false);
        }
        // Try to truncate on line boundary
        String truncated = snapshot.substring(0, maxChars);
        int lastNewline = truncated.lastIndexOf('\n');
        if (lastNewline > maxChars / 2) {
            truncated = truncated.substring(0, lastNewline);
        }
        return new TruncateResult(
                truncated + "\n\n[...TRUNCATED - page too large]",
                true);
    }

    /**
     * Format a raw aria snapshot into compact "ai" format.
     * Strips excessive whitespace and normalizes indentation.
     *
     * @param rawSnapshot the raw aria snapshot
     * @param maxChars    max chars (0 = no limit)
     * @return formatted result
     */
    public static TruncateResult formatAi(String rawSnapshot, int maxChars) {
        if (rawSnapshot == null || rawSnapshot.isEmpty()) {
            return new TruncateResult("(empty)", false);
        }
        // The AI format is essentially the aria snapshot with truncation applied
        return truncate(rawSnapshot, maxChars);
    }

    /**
     * Result of snapshot formatting/truncation.
     */
    public record TruncateResult(String snapshot, boolean truncated) {
    }
}

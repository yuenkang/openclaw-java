package com.openclaw.gateway.protocol;

import java.util.List;

/**
 * Validation error formatting utility.
 * <p>
 * In TypeScript the gateway uses AJV for runtime JSON-Schema validation.
 * In Java we rely on Jackson deserialization and programmatic checks.
 * This class provides the equivalent of {@code formatValidationErrors()}
 * from {@code protocol/index.ts}.
 */
public final class ProtocolValidation {

    private ProtocolValidation() {
    }

    /**
     * Format a list of validation-error messages into a single summary string,
     * matching the semicolon-delimited style of the TS gateway.
     *
     * @param errors individual error messages (may be null or empty)
     * @return formatted error string, never null
     */
    public static String formatValidationErrors(List<String> errors) {
        if (errors == null || errors.isEmpty()) {
            return "unknown validation error";
        }
        // De-dupe while preserving order.
        List<String> unique = errors.stream()
                .filter(e -> e != null && !e.isBlank())
                .distinct()
                .toList();
        if (unique.isEmpty()) {
            return "unknown validation error";
        }
        return String.join("; ", unique);
    }
}

package com.openclaw.channel;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Sender label resolution utilities.
 * Corresponds to TypeScript's channels/sender-label.ts.
 */
public final class SenderLabel {

    private SenderLabel() {
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Resolve a display label from sender identity fields.
     *
     * @return formatted label like "Name (phone)" or "username", or null if none
     *         available
     */
    public static String resolve(String name, String username, String tag,
            String e164, String id) {
        name = norm(name);
        username = norm(username);
        tag = norm(tag);
        e164 = norm(e164);
        id = norm(id);

        String display = name != null ? name : (username != null ? username : (tag != null ? tag : ""));
        String idPart = e164 != null ? e164 : (id != null ? id : "");

        if (!display.isEmpty() && !idPart.isEmpty() && !display.equals(idPart)) {
            return display + " (" + idPart + ")";
        }
        String result = !display.isEmpty() ? display : idPart;
        return result.isEmpty() ? null : result;
    }

    /**
     * List all unique sender label candidates.
     */
    public static List<String> listCandidates(String name, String username, String tag,
            String e164, String id) {
        LinkedHashSet<String> candidates = new LinkedHashSet<>();
        addIfPresent(candidates, norm(name));
        addIfPresent(candidates, norm(username));
        addIfPresent(candidates, norm(tag));
        addIfPresent(candidates, norm(e164));
        addIfPresent(candidates, norm(id));

        String resolved = resolve(name, username, tag, e164, id);
        if (resolved != null) {
            candidates.add(resolved);
        }
        return new ArrayList<>(candidates);
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private static String norm(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private static void addIfPresent(LinkedHashSet<String> set, String value) {
        if (value != null) {
            set.add(value);
        }
    }
}

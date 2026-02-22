package com.openclaw.browser.snapshot;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Role-based snapshot with ref annotations.
 * Converts Playwright ariaSnapshot into a compact tree with [ref=eN] markers
 * that the Agent can use to precisely identify and interact with UI elements.
 * <p>
 * Corresponds to TypeScript's {@code pw-role-snapshot.ts}.
 */
public final class RoleSnapshot {

    private RoleSnapshot() {
    }

    // =========================================================================
    // Data types
    // =========================================================================

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleRef {
        private String role;
        private String name;
        /** Index used only when role+name duplicates exist. */
        private Integer nth;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleSnapshotResult {
        private String snapshot;
        private Map<String, RoleRef> refs;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleSnapshotStats {
        private int lines;
        private int chars;
        private int refs;
        private int interactive;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleSnapshotOptions {
        /** Only include interactive elements (buttons, links, inputs, etc.). */
        private boolean interactive;
        /** Maximum depth to include (-1 = unlimited). */
        @Builder.Default
        private int maxDepth = -1;
        /** Remove unnamed structural elements and empty branches. */
        private boolean compact;
    }

    // =========================================================================
    // Role sets
    // =========================================================================

    private static final Set<String> INTERACTIVE_ROLES = Set.of(
            "button", "link", "textbox", "checkbox", "radio", "combobox",
            "listbox", "menuitem", "menuitemcheckbox", "menuitemradio",
            "option", "searchbox", "slider", "spinbutton", "switch",
            "tab", "treeitem"
    );

    private static final Set<String> CONTENT_ROLES = Set.of(
            "heading", "cell", "gridcell", "columnheader", "rowheader",
            "listitem", "article", "region", "main", "navigation"
    );

    private static final Set<String> STRUCTURAL_ROLES = Set.of(
            "generic", "group", "list", "table", "row", "rowgroup",
            "grid", "treegrid", "menu", "menubar", "toolbar", "tablist",
            "tree", "directory", "document", "application", "presentation", "none"
    );

    // =========================================================================
    // Regex
    // =========================================================================

    // Matches: "  - role "name" [attr] ..." 
    private static final Pattern LINE_PATTERN =
            Pattern.compile("^(\\s*-\\s*)(\\w+)(?:\\s+\"([^\"]*)\")?(.*)$");

    private static final Pattern AI_REF_PATTERN =
            Pattern.compile("\\[ref=(e\\d+)]", Pattern.CASE_INSENSITIVE);

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Build a role snapshot from Playwright ariaSnapshot output.
     * Generates new ref IDs (e1, e2, ...) for interactive and named content elements.
     */
    public static RoleSnapshotResult buildFromAriaSnapshot(String ariaSnapshot) {
        return buildFromAriaSnapshot(ariaSnapshot, RoleSnapshotOptions.builder().build());
    }

    public static RoleSnapshotResult buildFromAriaSnapshot(String ariaSnapshot,
                                                            RoleSnapshotOptions options) {
        String[] lines = (ariaSnapshot != null ? ariaSnapshot : "").split("\n");
        Map<String, RoleRef> refs = new LinkedHashMap<>();
        RoleNameTracker tracker = new RoleNameTracker();
        int[] counter = {0};

        if (options.isInteractive()) {
            return buildInteractiveOnly(lines, refs, tracker, counter, options);
        }

        List<String> result = new ArrayList<>();
        for (String line : lines) {
            String processed = processLine(line, refs, options, tracker, counter);
            if (processed != null) {
                result.add(processed);
            }
        }

        removeNthFromNonDuplicates(refs, tracker);

        String tree = result.isEmpty() ? "(empty)" : String.join("\n", result);
        return RoleSnapshotResult.builder()
                .snapshot(options.isCompact() ? compactTree(tree) : tree)
                .refs(refs)
                .build();
    }

    /**
     * Build a role snapshot from Playwright's AI snapshot output, preserving
     * Playwright's own ref ids (e.g. ref=e13).
     */
    public static RoleSnapshotResult buildFromAiSnapshot(String aiSnapshot) {
        return buildFromAiSnapshot(aiSnapshot, RoleSnapshotOptions.builder().build());
    }

    public static RoleSnapshotResult buildFromAiSnapshot(String aiSnapshot,
                                                          RoleSnapshotOptions options) {
        String[] lines = (aiSnapshot != null ? aiSnapshot : "").split("\n");
        Map<String, RoleRef> refs = new LinkedHashMap<>();

        if (options.isInteractive()) {
            List<String> out = new ArrayList<>();
            for (String line : lines) {
                int depth = getIndentLevel(line);
                if (options.getMaxDepth() >= 0 && depth > options.getMaxDepth()) {
                    continue;
                }
                Matcher m = LINE_PATTERN.matcher(line);
                if (!m.matches()) continue;
                String roleRaw = m.group(2);
                String name = m.group(3);
                String suffix = m.group(4);
                if (roleRaw.startsWith("/")) continue;
                String role = roleRaw.toLowerCase();
                if (!INTERACTIVE_ROLES.contains(role)) continue;
                String ref = parseAiRef(suffix);
                if (ref == null) continue;
                RoleRef rr = new RoleRef(role, name, null);
                refs.put(ref, rr);
                out.add(String.format("- %s%s%s",
                        roleRaw, name != null ? " \"" + name + "\"" : "", suffix));
            }
            return RoleSnapshotResult.builder()
                    .snapshot(out.isEmpty() ? "(no interactive elements)" : String.join("\n", out))
                    .refs(refs)
                    .build();
        }

        List<String> out = new ArrayList<>();
        for (String line : lines) {
            int depth = getIndentLevel(line);
            if (options.getMaxDepth() >= 0 && depth > options.getMaxDepth()) {
                continue;
            }
            Matcher m = LINE_PATTERN.matcher(line);
            if (!m.matches()) {
                out.add(line);
                continue;
            }
            String roleRaw = m.group(2);
            String name = m.group(3);
            String suffix = m.group(4);
            if (roleRaw.startsWith("/")) {
                out.add(line);
                continue;
            }
            String role = roleRaw.toLowerCase();
            if (options.isCompact() && STRUCTURAL_ROLES.contains(role) && name == null) {
                continue;
            }
            String ref = parseAiRef(suffix);
            if (ref != null) {
                refs.put(ref, new RoleRef(role, name, null));
            }
            out.add(line);
        }

        String tree = out.isEmpty() ? "(empty)" : String.join("\n", out);
        return RoleSnapshotResult.builder()
                .snapshot(options.isCompact() ? compactTree(tree) : tree)
                .refs(refs)
                .build();
    }

    /**
     * Get statistics for a role snapshot.
     */
    public static RoleSnapshotStats getStats(String snapshot, Map<String, RoleRef> refs) {
        long interactive = refs.values().stream()
                .filter(r -> INTERACTIVE_ROLES.contains(r.getRole()))
                .count();
        return RoleSnapshotStats.builder()
                .lines(snapshot.split("\n").length)
                .chars(snapshot.length())
                .refs(refs.size())
                .interactive((int) interactive)
                .build();
    }

    /**
     * Parse a ref string like "@e5", "ref=e5", or "e5".
     * Returns normalized form "e5" or null if invalid.
     */
    public static String parseRoleRef(String raw) {
        if (raw == null || raw.isBlank()) return null;
        String trimmed = raw.trim();
        String normalized;
        if (trimmed.startsWith("@")) {
            normalized = trimmed.substring(1);
        } else if (trimmed.startsWith("ref=")) {
            normalized = trimmed.substring(4);
        } else {
            normalized = trimmed;
        }
        return normalized.matches("^e\\d+$") ? normalized : null;
    }

    /**
     * Check if a role is interactive.
     */
    public static boolean isInteractiveRole(String role) {
        return INTERACTIVE_ROLES.contains(role != null ? role.toLowerCase() : "");
    }

    /**
     * Check if a string is a valid role ref (e.g. "e5", "e123").
     */
    public static boolean isValidRef(String value) {
        return value != null && value.matches("^e\\d+$");
    }

    // =========================================================================
    // Internals
    // =========================================================================

    private static RoleSnapshotResult buildInteractiveOnly(
            String[] lines, Map<String, RoleRef> refs,
            RoleNameTracker tracker, int[] counter,
            RoleSnapshotOptions options) {

        List<String> result = new ArrayList<>();
        for (String line : lines) {
            int depth = getIndentLevel(line);
            if (options.getMaxDepth() >= 0 && depth > options.getMaxDepth()) {
                continue;
            }
            Matcher m = LINE_PATTERN.matcher(line);
            if (!m.matches()) continue;
            String roleRaw = m.group(2);
            String name = m.group(3);
            String suffix = m.group(4);
            if (roleRaw.startsWith("/")) continue;
            String role = roleRaw.toLowerCase();
            if (!INTERACTIVE_ROLES.contains(role)) continue;

            counter[0]++;
            String ref = "e" + counter[0];
            int nth = tracker.getNextIndex(role, name);
            tracker.trackRef(role, name, ref);
            refs.put(ref, new RoleRef(role, name, nth));

            StringBuilder enhanced = new StringBuilder("- ").append(roleRaw);
            if (name != null) {
                enhanced.append(" \"").append(name).append("\"");
            }
            enhanced.append(" [ref=").append(ref).append("]");
            if (nth > 0) {
                enhanced.append(" [nth=").append(nth).append("]");
            }
            if (suffix != null && suffix.contains("[")) {
                enhanced.append(suffix);
            }
            result.add(enhanced.toString());
        }

        removeNthFromNonDuplicates(refs, tracker);

        return RoleSnapshotResult.builder()
                .snapshot(result.isEmpty() ? "(no interactive elements)" : String.join("\n", result))
                .refs(refs)
                .build();
    }

    private static String processLine(String line, Map<String, RoleRef> refs,
                                       RoleSnapshotOptions options,
                                       RoleNameTracker tracker, int[] counter) {
        int depth = getIndentLevel(line);
        if (options.getMaxDepth() >= 0 && depth > options.getMaxDepth()) {
            return null;
        }

        Matcher m = LINE_PATTERN.matcher(line);
        if (!m.matches()) {
            return options.isInteractive() ? null : line;
        }

        String prefix = m.group(1);
        String roleRaw = m.group(2);
        String name = m.group(3);
        String suffix = m.group(4);

        if (roleRaw.startsWith("/")) {
            return options.isInteractive() ? null : line;
        }

        String role = roleRaw.toLowerCase();
        boolean isInteractive = INTERACTIVE_ROLES.contains(role);
        boolean isContent = CONTENT_ROLES.contains(role);
        boolean isStructural = STRUCTURAL_ROLES.contains(role);

        if (options.isInteractive() && !isInteractive) {
            return null;
        }
        if (options.isCompact() && isStructural && name == null) {
            return null;
        }

        boolean shouldHaveRef = isInteractive || (isContent && name != null);
        if (!shouldHaveRef) {
            return line;
        }

        counter[0]++;
        String ref = "e" + counter[0];
        int nth = tracker.getNextIndex(role, name);
        tracker.trackRef(role, name, ref);
        refs.put(ref, new RoleRef(role, name, nth));

        StringBuilder enhanced = new StringBuilder(prefix).append(roleRaw);
        if (name != null) {
            enhanced.append(" \"").append(name).append("\"");
        }
        enhanced.append(" [ref=").append(ref).append("]");
        if (nth > 0) {
            enhanced.append(" [nth=").append(nth).append("]");
        }
        if (suffix != null && !suffix.isEmpty()) {
            enhanced.append(suffix);
        }
        return enhanced.toString();
    }

    private static String compactTree(String tree) {
        String[] lines = tree.split("\n");
        List<String> result = new ArrayList<>();

        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            // Keep lines with refs
            if (line.contains("[ref=")) {
                result.add(line);
                continue;
            }
            // Keep lines with content (colon but not ending with colon)
            if (line.contains(":") && !line.stripTrailing().endsWith(":")) {
                result.add(line);
                continue;
            }
            // Keep structural lines only if they have children with refs
            int currentIndent = getIndentLevel(line);
            boolean hasRelevantChildren = false;
            for (int j = i + 1; j < lines.length; j++) {
                int childIndent = getIndentLevel(lines[j]);
                if (childIndent <= currentIndent) break;
                if (lines[j].contains("[ref=")) {
                    hasRelevantChildren = true;
                    break;
                }
            }
            if (hasRelevantChildren) {
                result.add(line);
            }
        }

        return String.join("\n", result);
    }

    private static int getIndentLevel(String line) {
        int spaces = 0;
        for (int i = 0; i < line.length(); i++) {
            if (line.charAt(i) == ' ') spaces++;
            else break;
        }
        return spaces / 2;
    }

    private static String parseAiRef(String suffix) {
        if (suffix == null || suffix.isEmpty()) return null;
        Matcher m = AI_REF_PATTERN.matcher(suffix);
        return m.find() ? m.group(1) : null;
    }

    private static void removeNthFromNonDuplicates(Map<String, RoleRef> refs,
                                                     RoleNameTracker tracker) {
        Set<String> duplicates = tracker.getDuplicateKeys();
        for (RoleRef ref : refs.values()) {
            String key = tracker.getKey(ref.getRole(), ref.getName());
            if (!duplicates.contains(key)) {
                ref.setNth(null);
            }
        }
    }

    // =========================================================================
    // RoleNameTracker
    // =========================================================================

    private static class RoleNameTracker {
        private final Map<String, Integer> counts = new HashMap<>();
        private final Map<String, List<String>> refsByKey = new HashMap<>();

        String getKey(String role, String name) {
            return role + ":" + (name != null ? name : "");
        }

        int getNextIndex(String role, String name) {
            String key = getKey(role, name);
            int current = counts.getOrDefault(key, 0);
            counts.put(key, current + 1);
            return current;
        }

        void trackRef(String role, String name, String ref) {
            String key = getKey(role, name);
            refsByKey.computeIfAbsent(key, k -> new ArrayList<>()).add(ref);
        }

        Set<String> getDuplicateKeys() {
            Set<String> out = new HashSet<>();
            for (Map.Entry<String, List<String>> entry : refsByKey.entrySet()) {
                if (entry.getValue().size() > 1) {
                    out.add(entry.getKey());
                }
            }
            return out;
        }
    }
}

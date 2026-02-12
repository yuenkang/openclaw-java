package com.openclaw.agent.extensions;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * Tool pruning predicate builder for context pruning.
 * Compiles allow/deny glob patterns into a predicate that decides
 * whether a tool result is eligible for pruning.
 * Mirrors pi-extensions/context-pruning/tools.ts.
 */
public final class ContextPruningTools {

    private ContextPruningTools() {
    }

    // --- Pattern compilation ---

    private sealed interface CompiledPattern {
        record All() implements CompiledPattern {
        }

        record Exact(String value) implements CompiledPattern {
        }

        record Regex(Pattern value) implements CompiledPattern {
        }
    }

    private static List<String> normalizePatterns(List<String> patterns) {
        if (patterns == null || patterns.isEmpty())
            return List.of();
        List<String> result = new ArrayList<>();
        for (String p : patterns) {
            if (p == null)
                continue;
            String trimmed = p.strip().toLowerCase();
            if (!trimmed.isEmpty())
                result.add(trimmed);
        }
        return result;
    }

    private static CompiledPattern compile(String pattern) {
        if ("*".equals(pattern))
            return new CompiledPattern.All();
        if (!pattern.contains("*"))
            return new CompiledPattern.Exact(pattern);
        // Escape regex-special chars, then replace \* with .*
        String escaped = Pattern.quote(pattern).replace("\\*", "\\E.*\\Q");
        // Clean up empty \Q\E segments
        escaped = escaped.replace("\\Q\\E", "");
        return new CompiledPattern.Regex(Pattern.compile("^" + escaped + "$"));
    }

    private static List<CompiledPattern> compileAll(List<String> patterns) {
        return normalizePatterns(patterns).stream()
                .map(ContextPruningTools::compile)
                .toList();
    }

    private static boolean matchesAny(String toolName, List<CompiledPattern> patterns) {
        for (CompiledPattern p : patterns) {
            if (p instanceof CompiledPattern.All) {
                return true;
            } else if (p instanceof CompiledPattern.Exact e) {
                if (toolName.equals(e.value()))
                    return true;
            } else if (p instanceof CompiledPattern.Regex r) {
                if (r.value().matcher(toolName).matches())
                    return true;
            }
        }
        return false;
    }

    /**
     * Build a predicate that returns true if a tool's result is eligible for
     * pruning.
     */
    public static Predicate<String> makeToolPrunablePredicate(
            ContextPruningSettings.ToolMatch match) {
        if (match == null)
            match = new ContextPruningSettings.ToolMatch();
        List<CompiledPattern> deny = compileAll(match.getDeny());
        List<CompiledPattern> allow = compileAll(match.getAllow());

        return toolName -> {
            String normalized = toolName.strip().toLowerCase();
            if (matchesAny(normalized, deny))
                return false;
            if (allow.isEmpty())
                return true;
            return matchesAny(normalized, allow);
        };
    }
}

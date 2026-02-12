package com.openclaw.agent.runner;

import java.util.*;

/**
 * Tool splitting for the embedded agent SDK.
 * In the Java port we always use customTools; builtInTools is empty.
 * Mirrors {@code agents/pi-embedded-runner/tool-split.ts}.
 */
public final class ToolSplitter {

    private ToolSplitter() {
    }

    /** Result of splitting tools into builtIn vs custom. */
    public record SplitResult(
            List<Map<String, Object>> builtInTools,
            List<Map<String, Object>> customTools) {
    }

    /**
     * Split agent tools into built-in and custom tool lists.
     * In practice all tools are passed as customTools.
     */
    public static SplitResult splitSdkTools(List<Map<String, Object>> tools) {
        return new SplitResult(List.of(), tools != null ? tools : List.of());
    }
}

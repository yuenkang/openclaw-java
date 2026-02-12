package com.openclaw.agent.autoreply;

import java.util.*;

/**
 * Thinking, verbose, notice, elevated, reasoning and usage display level
 * normalization.
 * Mirrors {@code auto-reply/thinking.ts}.
 */
public final class ThinkingLevels {

    private ThinkingLevels() {
    }

    // ── ThinkLevel ──────────────────────────────────────────────────

    private static final List<String> XHIGH_MODEL_REFS = List.of(
            "openai/gpt-5.2",
            "openai-codex/gpt-5.3-codex",
            "openai-codex/gpt-5.2-codex",
            "openai-codex/gpt-5.1-codex");

    private static final Set<String> XHIGH_MODEL_SET;
    private static final Set<String> XHIGH_MODEL_IDS;

    static {
        Set<String> refs = new HashSet<>();
        Set<String> ids = new HashSet<>();
        for (String ref : XHIGH_MODEL_REFS) {
            refs.add(ref.toLowerCase());
            int slash = ref.indexOf('/');
            if (slash >= 0) {
                ids.add(ref.substring(slash + 1).toLowerCase());
            }
        }
        XHIGH_MODEL_SET = refs;
        XHIGH_MODEL_IDS = ids;
    }

    private static String normalizeProviderId(String provider) {
        if (provider == null || provider.isBlank())
            return "";
        String norm = provider.trim().toLowerCase();
        if ("z.ai".equals(norm) || "z-ai".equals(norm))
            return "zai";
        return norm;
    }

    public static boolean isBinaryThinkingProvider(String provider) {
        return "zai".equals(normalizeProviderId(provider));
    }

    /** Normalize a user-provided thinking level string to the canonical value. */
    public static String normalizeThinkLevel(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String key = raw.trim().toLowerCase();
        String collapsed = key.replaceAll("[\\s_-]+", "");
        if ("xhigh".equals(collapsed) || "extrahigh".equals(collapsed))
            return "xhigh";
        if ("off".equals(key))
            return "off";
        if (Set.of("on", "enable", "enabled").contains(key))
            return "low";
        if (Set.of("min", "minimal").contains(key))
            return "minimal";
        if (Set.of("low", "thinkhard", "think-hard", "think_hard").contains(key))
            return "low";
        if (Set.of("mid", "med", "medium", "thinkharder", "think-harder", "harder").contains(key))
            return "medium";
        if (Set.of("high", "ultra", "ultrathink", "thinkhardest", "highest", "max").contains(key))
            return "high";
        if ("think".equals(key))
            return "minimal";
        return null;
    }

    public static boolean supportsXHighThinking(String provider, String model) {
        if (model == null || model.isBlank())
            return false;
        String modelKey = model.trim().toLowerCase();
        String provKey = provider != null ? provider.trim().toLowerCase() : null;
        if (provKey != null && !provKey.isEmpty()) {
            return XHIGH_MODEL_SET.contains(provKey + "/" + modelKey);
        }
        return XHIGH_MODEL_IDS.contains(modelKey);
    }

    public static List<String> listThinkingLevels(String provider, String model) {
        List<String> levels = new ArrayList<>(List.of("off", "minimal", "low", "medium", "high"));
        if (supportsXHighThinking(provider, model))
            levels.add("xhigh");
        return levels;
    }

    public static List<String> listThinkingLevelLabels(String provider, String model) {
        if (isBinaryThinkingProvider(provider))
            return List.of("off", "on");
        return listThinkingLevels(provider, model);
    }

    public static String formatThinkingLevels(String provider, String model, String separator) {
        if (separator == null)
            separator = ", ";
        return String.join(separator, listThinkingLevelLabels(provider, model));
    }

    public static String formatXHighModelHint() {
        if (XHIGH_MODEL_REFS.isEmpty())
            return "unknown model";
        if (XHIGH_MODEL_REFS.size() == 1)
            return XHIGH_MODEL_REFS.get(0);
        if (XHIGH_MODEL_REFS.size() == 2)
            return XHIGH_MODEL_REFS.get(0) + " or " + XHIGH_MODEL_REFS.get(1);
        return String.join(", ", XHIGH_MODEL_REFS.subList(0, XHIGH_MODEL_REFS.size() - 1))
                + " or " + XHIGH_MODEL_REFS.get(XHIGH_MODEL_REFS.size() - 1);
    }

    // ── VerboseLevel ────────────────────────────────────────────────

    public static String normalizeVerboseLevel(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String key = raw.toLowerCase();
        if (Set.of("off", "false", "no", "0").contains(key))
            return "off";
        if (Set.of("full", "all", "everything").contains(key))
            return "full";
        if (Set.of("on", "minimal", "true", "yes", "1").contains(key))
            return "on";
        return null;
    }

    // ── NoticeLevel ─────────────────────────────────────────────────

    public static String normalizeNoticeLevel(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String key = raw.toLowerCase();
        if (Set.of("off", "false", "no", "0").contains(key))
            return "off";
        if (Set.of("full", "all", "everything").contains(key))
            return "full";
        if (Set.of("on", "minimal", "true", "yes", "1").contains(key))
            return "on";
        return null;
    }

    // ── UsageDisplayLevel ───────────────────────────────────────────

    public static String normalizeUsageDisplay(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String key = raw.toLowerCase();
        if (Set.of("off", "false", "no", "0", "disable", "disabled").contains(key))
            return "off";
        if (Set.of("on", "true", "yes", "1", "enable", "enabled").contains(key))
            return "tokens";
        if (Set.of("tokens", "token", "tok", "minimal", "min").contains(key))
            return "tokens";
        if (Set.of("full", "session").contains(key))
            return "full";
        return null;
    }

    public static String resolveResponseUsageMode(String raw) {
        String level = normalizeUsageDisplay(raw);
        return level != null ? level : "off";
    }

    // ── ElevatedLevel ───────────────────────────────────────────────

    public static String normalizeElevatedLevel(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String key = raw.toLowerCase();
        if (Set.of("off", "false", "no", "0").contains(key))
            return "off";
        if (Set.of("full", "auto", "auto-approve", "autoapprove").contains(key))
            return "full";
        if (Set.of("ask", "prompt", "approval", "approve").contains(key))
            return "ask";
        if (Set.of("on", "true", "yes", "1").contains(key))
            return "on";
        return null;
    }

    /** Resolve elevated to a mode (off/ask/full). "on" maps to "ask". */
    public static String resolveElevatedMode(String level) {
        if (level == null || "off".equals(level))
            return "off";
        if ("full".equals(level))
            return "full";
        return "ask";
    }

    // ── ReasoningLevel ──────────────────────────────────────────────

    public static String normalizeReasoningLevel(String raw) {
        if (raw == null || raw.isBlank())
            return null;
        String key = raw.toLowerCase();
        if (Set.of("off", "false", "no", "0", "hide", "hidden", "disable", "disabled").contains(key))
            return "off";
        if (Set.of("on", "true", "yes", "1", "show", "visible", "enable", "enabled").contains(key))
            return "on";
        if (Set.of("stream", "streaming", "draft", "live").contains(key))
            return "stream";
        return null;
    }
}

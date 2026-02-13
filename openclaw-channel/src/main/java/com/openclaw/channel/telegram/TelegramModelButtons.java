package com.openclaw.channel.telegram;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Telegram inline button utilities for model selection.
 * Corresponds to TypeScript's telegram/model-buttons.ts.
 */
public final class TelegramModelButtons {

    private TelegramModelButtons() {
    }

    public static final int PAGE_SIZE = 8;
    public static final int MAX_CALLBACK_DATA_BYTES = 64;

    // =========================================================================
    // Types
    // =========================================================================

    public record Button(String text, String callbackData) {
    }

    public record ProviderInfo(String id, int count) {
    }

    public sealed interface ParsedCallback {
        record Providers() implements ParsedCallback {
        }

        record ModelList(String provider, int page) implements ParsedCallback {
        }

        record Select(String provider, String model) implements ParsedCallback {
        }

        record Back() implements ParsedCallback {
        }
    }

    // =========================================================================
    // Callback parsing
    // =========================================================================

    private static final Pattern LIST_PATTERN = Pattern.compile(
            "^mdl_list_([a-zA-Z0-9_-]+)_(\\d+)$");
    private static final Pattern SEL_PATTERN = Pattern.compile("^mdl_sel_(.+)$");

    public static ParsedCallback parseCallbackData(String data) {
        String trimmed = data.trim();
        if (!trimmed.startsWith("mdl_"))
            return null;
        if ("mdl_prov".equals(trimmed))
            return new ParsedCallback.Providers();
        if ("mdl_back".equals(trimmed))
            return new ParsedCallback.Back();

        Matcher listMatch = LIST_PATTERN.matcher(trimmed);
        if (listMatch.matches()) {
            int page = Integer.parseInt(listMatch.group(2));
            if (page >= 1)
                return new ParsedCallback.ModelList(listMatch.group(1), page);
        }

        Matcher selMatch = SEL_PATTERN.matcher(trimmed);
        if (selMatch.matches()) {
            String modelRef = selMatch.group(1);
            int slash = modelRef.indexOf('/');
            if (slash > 0 && slash < modelRef.length() - 1) {
                return new ParsedCallback.Select(
                        modelRef.substring(0, slash), modelRef.substring(slash + 1));
            }
        }
        return null;
    }

    // =========================================================================
    // Keyboard builders
    // =========================================================================

    public static List<List<Button>> buildProviderKeyboard(List<ProviderInfo> providers) {
        if (providers.isEmpty())
            return List.of();
        List<List<Button>> rows = new ArrayList<>();
        List<Button> current = new ArrayList<>();
        for (ProviderInfo p : providers) {
            current.add(new Button(p.id() + " (" + p.count() + ")",
                    "mdl_list_" + p.id() + "_1"));
            if (current.size() == 2) {
                rows.add(List.copyOf(current));
                current.clear();
            }
        }
        if (!current.isEmpty())
            rows.add(List.copyOf(current));
        return rows;
    }

    public static List<List<Button>> buildModelsKeyboard(String provider, List<String> models,
            String currentModel, int currentPage,
            int totalPages) {
        if (models.isEmpty())
            return List.of(List.of(new Button("<< Back", "mdl_back")));
        List<List<Button>> rows = new ArrayList<>();
        int start = (currentPage - 1) * PAGE_SIZE;
        int end = Math.min(start + PAGE_SIZE, models.size());

        String currentModelId = currentModel != null && currentModel.contains("/")
                ? currentModel.substring(currentModel.indexOf('/') + 1)
                : currentModel;

        for (int i = start; i < end; i++) {
            String model = models.get(i);
            String callbackData = "mdl_sel_" + provider + "/" + model;
            if (callbackData.getBytes(java.nio.charset.StandardCharsets.UTF_8).length > MAX_CALLBACK_DATA_BYTES)
                continue;
            boolean isCurrent = model.equals(currentModelId);
            String display = truncateModelId(model, 38);
            rows.add(List.of(new Button(isCurrent ? display + " ✓" : display, callbackData)));
        }

        if (totalPages > 1) {
            List<Button> nav = new ArrayList<>();
            if (currentPage > 1)
                nav.add(new Button("◀ Prev", "mdl_list_" + provider + "_" + (currentPage - 1)));
            nav.add(new Button(currentPage + "/" + totalPages,
                    "mdl_list_" + provider + "_" + currentPage));
            if (currentPage < totalPages)
                nav.add(new Button("Next ▶", "mdl_list_" + provider + "_" + (currentPage + 1)));
            rows.add(List.copyOf(nav));
        }
        rows.add(List.of(new Button("<< Back", "mdl_back")));
        return rows;
    }

    public static List<List<Button>> buildBrowseProvidersButton() {
        return List.of(List.of(new Button("Browse providers", "mdl_prov")));
    }

    public static int calculateTotalPages(int totalModels) {
        return PAGE_SIZE > 0 ? (int) Math.ceil((double) totalModels / PAGE_SIZE) : 1;
    }

    private static String truncateModelId(String modelId, int maxLen) {
        if (modelId.length() <= maxLen)
            return modelId;
        return "…" + modelId.substring(modelId.length() - (maxLen - 1));
    }
}

package com.openclaw.app.commands;

import com.openclaw.agent.models.ModelProvider;
import com.openclaw.agent.models.ModelProviderRegistry;
import com.openclaw.common.config.ConfigRuntimeOverrides;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Model commands: /models, /model.
 * Mirrors TypeScript's {@code commands-models.ts}.
 */
@Slf4j
@Component
public class ModelCommands {

    private final ModelProviderRegistry modelProviderRegistry;

    public ModelCommands(ModelProviderRegistry modelProviderRegistry) {
        this.modelProviderRegistry = modelProviderRegistry;
    }

    public CommandResult handleModels(String args, CommandContext ctx) {
        var providerIds = modelProviderRegistry.getProviderIds();
        if (providerIds.isEmpty()) {
            return CommandResult.text("⚠️ 没有已注册的模型 Provider。请检查 API Key 配置。");
        }

        if (!args.isEmpty()) {
            String targetProvider = args.trim().toLowerCase();
            if (!modelProviderRegistry.hasProvider(targetProvider)) {
                return CommandResult.text(String.format("❌ Provider `%s` 未注册。\n\n已注册: %s",
                        targetProvider, String.join(", ", providerIds)));
            }
            ModelProvider provider = modelProviderRegistry.getProvider(targetProvider);
            StringBuilder sb = new StringBuilder();
            sb.append(String.format("🤖 *%s 可用模型*\n\n", targetProvider));
            try {
                var models = provider.listModels();
                if (models.isEmpty()) {
                    sb.append("(无法获取模型列表)");
                } else {
                    for (var model : models) {
                        sb.append(String.format("• `%s`", model.getId()));
                        if (model.getName() != null && !model.getName().equals(model.getId())) {
                            sb.append(String.format(" — %s", model.getName()));
                        }
                        sb.append("\n");
                    }
                }
            } catch (Exception e) {
                sb.append("(获取模型列表失败: ").append(e.getMessage()).append(")");
            }
            return CommandResult.text(sb.toString());
        }

        StringBuilder sb = new StringBuilder();
        sb.append("🤖 *已注册 Provider*\n\n");
        for (String providerId : providerIds) {
            ModelProvider provider = modelProviderRegistry.getProvider(providerId);
            int modelCount = 0;
            try {
                modelCount = provider.listModels().size();
            } catch (Exception ignored) {
            }
            sb.append(String.format("• `%s`", providerId));
            if (modelCount > 0) {
                sb.append(String.format(" (%d 个模型)", modelCount));
            }
            sb.append("\n");
        }

        var aliases = modelProviderRegistry.getAliases();
        if (!aliases.isEmpty()) {
            sb.append("\n📎 *模型别名*\n\n");
            for (var entry : aliases.entrySet()) {
                sb.append(String.format("• `%s` → `%s`\n", entry.getKey(), entry.getValue()));
            }
        }

        sb.append("\n💡 使用 `/models <provider>` 查看特定 provider 的模型列表。");
        return CommandResult.text(sb.toString());
    }

    public CommandResult handleModel(String args, CommandContext ctx) {
        var config = ctx.config();
        if (args.isEmpty()) {
            String modelId = config.getModel() != null ? config.getModel() : "default";
            String resolved = modelProviderRegistry.resolveModelId(modelId);
            StringBuilder sb = new StringBuilder();
            sb.append(String.format("🏷️ 当前模型: `%s`", modelId));
            if (!modelId.equals(resolved)) {
                sb.append(String.format(" → `%s`", resolved));
            }
            var overrides = ConfigRuntimeOverrides.getConfigOverrides();
            Object overrideModel = overrides.get("model");
            if (overrideModel != null) {
                sb.append(String.format("\n⚙️ 运行时覆盖: `%s`", overrideModel));
            }
            sb.append("\n\n💡 使用 `/model <model-id>` 切换模型。");
            return CommandResult.text(sb.toString());
        }

        String newModel = args.trim();
        var result = ConfigRuntimeOverrides.setConfigOverride("model", newModel);
        if (result.ok()) {
            String resolved = modelProviderRegistry.resolveModelId(newModel);
            ModelProvider provider = modelProviderRegistry.resolve(newModel);
            StringBuilder sb = new StringBuilder();
            sb.append(String.format("✅ 模型已切换: `%s`", newModel));
            if (!newModel.equals(resolved)) {
                sb.append(String.format(" → `%s`", resolved));
            }
            if (provider == null) {
                sb.append("\n⚠️ 警告: 未找到对应的 Provider，可能导致调用失败。");
            }
            return CommandResult.text(sb.toString());
        }
        return CommandResult.text("❌ 模型切换失败: " + result.error());
    }
}

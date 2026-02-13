# Changelog

## Phase 19 — Auto-Reply Batch 11: agent-runner-execution + get-reply-* pipeline (6 Java files, 1148 lines)

### Added (autoreply/reply/ 包, 6 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `AgentRunnerExecution.java` | `agent-runner-execution.ts` | 239 |
| `AgentRunner.java` | `agent-runner.ts` | 186 |
| `GetReplyDirectivesApply.java` | `get-reply-directives-apply.ts` | 157 |
| `GetReplyDirectives.java` | `get-reply-directives.ts` | 224 |
| `GetReplyInlineActions.java` | `get-reply-inline-actions.ts` | 88 |
| `GetReplyRun.java` | `get-reply-run.ts` | 187 |

Phase 19 累计 107/121 文件（~12863 行）。auto-reply/reply 目录翻译完成 ✅

## Phase 19 — Auto-Reply Batch 10: agent-runner + bash + model-selection + get-reply (10 Java files, 1591 lines)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `AudioTags.java` | `audio-tags.ts` | 16 |
| `Exec.java` | `exec.ts` | 15 |
| `AgentRunnerHelpers.java` | `agent-runner-helpers.ts` | 127 |
| `AgentRunnerPayloads.java` | `agent-runner-payloads.ts` | 150 |
| `AgentRunnerUtils.java` | `agent-runner-utils.ts` | 138 |
| `AgentRunnerMemory.java` | `agent-runner-memory.ts` | 100 |
| `FollowupRunner.java` | `followup-runner.ts` | 145 |
| `BashCommand.java` | `bash-command.ts` | 239 |
| `ModelSelection.java` | `model-selection.ts` | 314 |
| `GetReply.java` | `get-reply.ts` | 159 |

Phase 19 累计 101/121 文件（~11715 行）。

## Phase 19 — Auto-Reply Batch 9: session + history + dispatch + allowlist (10 Java files, 1756 lines)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `History.java` | `history.ts` | 161 |
| `SessionUpdates.java` | `session-updates.ts` | 148 |
| `Session.java` | `session.ts` | 139 |
| `StageSandboxMedia.java` | `stage-sandbox-media.ts` | 134 |
| `CommandsContextReport.java` | `commands-context-report.ts` | 100 |
| `CommandsSubagents.java` | `commands-subagents.ts` | 168 |
| `DirectiveHandlingImpl.java` | `directive-handling.impl.ts` | 197 |
| `DirectiveHandlingModel.java` | `directive-handling.model.ts` | 160 |
| `DispatchFromConfig.java` | `dispatch-from-config.ts` | 80 |
| `CommandsAllowlist.java` | `commands-allowlist.ts` | 289 |

Phase 19 累计 91/121 文件（~10124 行）。

## Phase 19 — Auto-Reply Batch 8: directives + inbound + streaming (10 Java files, 942 lines)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `DirectiveHandlingFastLane.java` | `directive-handling.fast-lane.ts` | 138 |
| `DirectiveHandlingModelPicker.java` | `directive-handling.model-picker.ts` | 93 |
| `DirectiveHandlingQueueValidation.java` | `directive-handling.queue-validation.ts` | 62 |
| `DirectiveHandling.java` | `directive-handling.ts` | 42 |
| `InboundContext.java` | `inbound-context.ts` | 118 |
| `InboundSenderMeta.java` | `inbound-sender-meta.ts` | 74 |
| `ProviderDispatcher.java` | `provider-dispatcher.ts` | 46 |
| `QueueFacade.java` | `queue.ts` | 26 |
| `StreamingDirectives.java` | `streaming-directives.ts` | 164 |
| `SessionResetModel.java` | `session-reset-model.ts` | 157 |

---

## Phase 19 — Auto-Reply Batch 7: commands + directives (10 Java files, 913 lines)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `CommandsBash.java` | `commands-bash.ts` | 60 |
| `CommandsContextBuilder.java` | `commands-context.ts` | 50 |
| `CommandsPlugin.java` | `commands-plugin.ts` | 46 |
| `CommandsCompact.java` | `commands-compact.ts` | 98 |
| `CommandsTts.java` | `commands-tts.ts` | 120 |
| `CommandsCore.java` | `commands-core.ts` | 95 |
| `DirectiveHandlingParse.java` | `directive-handling.parse.ts` | 108 |
| `DirectiveHandlingAuth.java` | `directive-handling.auth.ts` | 100 |
| `DirectiveHandlingPersist.java` | `directive-handling.persist.ts` | 97 |
| `CommandsModels.java` | `commands-models.ts` | 119 |

---

## Phase 19 — Auto-Reply Batch 6: commands + routing (10 Java files, 1291 lines)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `DebugCommands.java` | `debug-commands.ts` | 80 |
| `InboundDedupe.java` | `inbound-dedupe.ts` | 150 |
| `RouteReply.java` | `route-reply.ts` | 200 |
| `CommandsApprove.java` | `commands-approve.ts` | 126 |
| `CommandsPtt.java` | `commands-ptt.ts` | 209 |
| `CommandsInfo.java` | `commands-info.ts` | 205 |
| `CommandsStatus.java` | `commands-status.ts` | 250 |
| `CommandsConfig.java` | `commands-config.ts` | 100 |
| `CommandsSession.java` | `commands-session.ts` | 380 |
| `LineDirectives.java` | `line-directives.ts` | 343 |

---



## Phase 19 — Auto-Reply Batch 5: reply/ + queue/ (10 Java files, 1178 lines)

### Added (autoreply/reply/ 包 + queue/ 子包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `ReplyPayloads.java` | `reply-payloads.ts` | 168 |
| `Body.java` | `body.ts` | 39 |
| `SessionUsage.java` | `session-usage.ts` | 89 |
| `Groups.java` | `groups.ts` | 126 |
| `ReplyElevated.java` | `reply-elevated.ts` | 214 |
| `Abort.java` | `abort.ts` | 122 |
| `ReplyDirectives.java` | `reply-directives.ts` | 83 |
| `queue/QueueEnqueue.java` | `queue/enqueue.ts` | 123 |
| `queue/QueueDrain.java` | `queue/drain.ts` | 139 |
| `ConfigCommands.java` | `config-commands.ts` | 75 |

---

## Phase 19 — Auto-Reply Batch 4: reply/ + queue/ (10 Java files, 1262 lines)

### Added (autoreply/reply/ 包 + queue/ 子包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `queue/QueueTypes.java` | `queue/types.ts` | 106 |
| `queue/QueueNormalize.java` | `queue/normalize.ts` | 48 |
| `queue/QueueState.java` | `queue/state.ts` | 93 |
| `queue/QueueCleanup.java` | `queue/cleanup.ts` | 47 |
| `queue/QueueDirective.java` | `queue/directive.ts` | 182 |
| `queue/QueueSettingsResolver.java` | `queue/settings.ts` | 139 |
| `ReplyThreading.java` | `reply-threading.ts` | 90 |
| `ReplyDispatcher.java` | `reply-dispatcher.ts` | 175 |
| `NormalizeReply.java` | `normalize-reply.ts` | 116 |
| `BlockReplyPipeline.java` | `block-reply-pipeline.ts` | 266 |

---

## Phase 19 — Auto-Reply Batch 3: reply/ (10 Java files)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `Typing.java` | `typing.ts` | 192 |
| `TypingMode.java` | `typing-mode.ts` | 113 |
| `ReplyDirectiveTypes.java` | `reply-directives.ts` | 24 |
| `BlockStreamingTypes.java` | `block-streaming.ts` | 26 |
| `BlockReplyCoalescer.java` | `block-reply-coalescer.ts` | 184 |
| `GetReplyDirectivesUtils.java` | `get-reply-directives-utils.ts` | 78 |
| `Mentions.java` | `mentions.ts` | 135 |
| `MemoryFlush.java` | `memory-flush.ts` | 82 |
| `ReplyTags.java` | `reply-tags.ts` | 55 |
| `SessionUsageTypes.java` | `session-usage.ts` | 43 |

---

## Phase 19 — Auto-Reply Batch 2: reply/ (10 Java files)

### Added (autoreply/reply/ 包, 10 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `InboundText.java` | `inbound-text.ts` | 18 |
| `ConfigValue.java` | `config-value.ts` | 82 |
| `ReplyInline.java` | `reply-inline.ts` | 65 |
| `ReplyReference.java` | `reply-reference.ts` | 78 |
| `UntrustedContext.java` | `untrusted-context.ts` | 33 |
| `Directives.java` | `directives.ts` | 103 |
| `DirectiveHandlingShared.java` | `directive-handling.shared.ts` | 88 |
| `ResponsePrefixTemplate.java` | `response-prefix-template.ts` | 63 |
| `SubagentsUtils.java` | `subagents-utils.ts` | 95 |
| `CommandsTypes.java` | `commands-types.ts` | 62 |

## Phase 19 — Auto-Reply Batch 1: Top-Level (11 Java files)

### Added (autoreply/ 包, 11 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `AutoReplyTypes.java` | `types.ts` | 31 |
| `ReplyTokens.java` | `tokens.ts` | 34 |
| `ModelDirective.java` | `model.ts` | 87 |
| `ThinkingLevels.java` | `thinking.ts` | 172 |
| `GroupActivation.java` | `group-activation.ts` | 48 |
| `SendPolicy.java` | `send-policy.ts` | 52 |
| `CommandNormalizer.java` | `commands-registry.ts` (partial) | 68 |
| `CommandsArgs.java` | `commands-args.ts` | 95 |
| `MediaNote.java` | `media-note.ts` | 111 |
| `InboundDebounce.java` | `inbound-debounce.ts` | 155 |
| `ToolMeta.java` | `tool-meta.ts` | 140 |

## Phase 18 — pi-embedded-runner/ Batch 4 (12 Java files)

### Added (runner/ 包, 12 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `RunnerTypes.java` | `types.ts` | 105 |
| `RunnerUtils.java` | `utils.ts` | 40 |
| `AbortDetector.java` | `abort.ts` | 34 |
| `CacheTtl.java` | `cache-ttl.ts` | 60 |
| `HistoryLimiter.java` | `history.ts` | 98 |
| `RunnerLanes.java` | `lanes.ts` | 32 |
| `RunnerLogger.java` | `logger.ts` | 15 |
| `SandboxInfoBuilder.java` | `sandbox-info.ts` | 60 |
| `ToolSplitter.java` | `tool-split.ts` | 28 |
| `ExtraParams.java` | `extra-params.ts` | 128 |
| `RunnerModel.java` | `model.ts` | 105 |
| `SessionManagerSetup.java` | `session-manager-{cache,init}.ts` | 130 |

## Phase 18 — pi-embedded-helpers/ Batch 3 (9 Java files)

### Added (embedded/ 包, 9 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `EmbeddedTypes.java` | `types.ts` | 35 |
| `MessagingDedupe.java` | `messaging-dedupe.ts` | 68 |
| `GoogleHelpers.java` | `google.ts` | 32 |
| `ThinkingHelpers.java` | `thinking.ts` | 90 |
| `TurnValidation.java` | `turns.ts` | 115 |
| `BootstrapHelpers.java` | `bootstrap.ts` | 198 |
| `EmbeddedErrors.java` | `errors.ts` | 242 |
| `OpenAIHelpers.java` | `openai.ts` | 136 |
| `ImageSanitizer.java` | `images.ts` | 183 |

## Phase 18 — agents/ 杂项模块 Batch 2 (10 Java files)

### Added (runtime/ 包, 7 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `SandboxPaths.java` | `sandbox-paths.ts` | 155 |
| `ApplyPatchUpdate.java` | `apply-patch-update.ts` | 196 |
| `ApplyPatch.java` | `apply-patch.ts` | 316 |
| `ModelCompat.java` | `model-compat.ts` | 46 |
| `SessionFileRepair.java` | `session-file-repair.ts` | 120 |
| `ToolDisplay.java` | `tool-display.ts` | 252 |
| `WorkspaceTemplates.java` | `workspace-templates.ts` | 72 |

### Added (models/ 包, 3 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `VeniceModels.java` | `venice-models.ts` | 117 |
| `OpencodeZenModels.java` | `opencode-zen-models.ts` | 163 |
| `ModelAuth.java` | `model-auth.ts` | 168 |

## Phase 18 — agents/ 杂项模块 Batch 1 (12 Java files)

### Added (runtime/ 包, 11 new files)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `UsageUtils.java` | `usage.ts` | 90 |
| `PiSettings.java` | `pi-settings.ts` | 48 |
| `ToolCallIdSanitizer.java` | `tool-call-id.ts` | 205 |
| `SessionSlug.java` | `session-slug.ts` | 84 |
| `AgentTimeout.java` | `timeout.ts` | 63 |
| `ShellUtils.java` | `shell-utils.ts` | 130 |
| `ToolSummaryBuilder.java` | `tool-summaries.ts` | 42 |
| `AgentIdentityResolver.java` | `identity.ts` | 63 |
| `TranscriptPolicyResolver.java` | `transcript-policy.ts` | 122 |
| `ModelsConfigMerger.java` | `models-config.ts` | 160 |
| `WorkspaceManager.java` | `workspace.ts` | 170 |

### Added (models/ 包, 1 new file)

| Java 文件 | 对应 TS 源 | 行数 |
|-----------|-----------|------|
| `SyntheticModels.java` | `synthetic-models.ts` | 96 |

## Phase 17 — agents/ 剩余子模块 (pi-extensions, schema, cli-runner)

### Added (extensions/ 包, 7 new files)
- `ContextPruningSettings` — 上下文裁剪配置类型 + 有效设置解析器（TTL/比率/工具匹配）
- `ContextPruningRuntime` — WeakHashMap 会话级运行时注册表
- `ContextPruningTools` — allow/deny glob 模式编译 + 工具可裁剪谓词
- `ContextPruner` — 软裁剪(head+tail)+硬清除两阶段裁剪器
- `ContextPruningExtension` — context 事件入口（TTL 检查 → 裁剪委派）
- `CompactionSafeguardRuntime` — 压缩保障运行时注册表
- `CompactionSafeguard` — 工具失败提取 + 文件操作摘要 + 回退汇总

### Added (schema/ 包, 2 new files)
- `GeminiSchemaCleanser` — JSON Schema 清洗（移除不支持关键字/$ref 解析/anyOf 扁平化/null 剥离）
- `SchemaEnumUtils` — 安全 string-enum schema 构建器（避免 anyOf 被 provider 拒绝）

### Added (runtime/ 包, 1 new file)
- `CliRunnerHelpers` — CLI 后端管理（进程清理/JSON+JSONL 解析/会话 ID/参数构建/图片写入）

## Phase 16 — Skills 子系统

### Modified
- `SkillTypes` — 补全 6 个缺失类型 (SkillInstallSpec/CommandSpec/DispatchSpec/InstallPreferences/EligibilityContext + EXTRA source)
- `SkillFrontmatterParser` — 适配 SkillMetadata 新增 `install` 参数
- `SkillLoader` — 适配 SkillSnapshot 新增 `resolvedSkills` 参数
- `OpenClawConfig` — 新增 `SkillsConfig` / `SkillLoadConfig` 嵌套类 + `skills` / `plugins` 字段

### Added (skills/ 包, 6 new files, total 9 files, 1768 lines)
- `SkillConfigResolver` — 配置路径解析 + 资格检查 (OS/bins/env/config) + 二进制检测 + 内置白名单
- `SkillEnvOverrides` — 技能环境变量覆盖 + 可逆 Runnable + apiKey→primaryEnv 映射
- `SkillBundledDir` — 内置技能目录解析 (env→jar→classpath→上溯) + context 加载
- `SkillSerializer` — 按 key 序列化异步任务 (CompletableFuture 队列)
- `SkillRefresh` — 文件监视器 (WatchService) + 快照版本管理 + debounce + 变更通知
- `SkillPluginResolver` — 插件技能目录解析 (workspace+global manifest 扫描 + enabled 检查)

## Phase 15 — Sandbox 沙箱子系统

### Added (sandbox/ 包, 12 files, 1797 lines)
- `SandboxTypes` — 全部类型定义 (Docker/Browser/Prune/ToolPolicy/Context/WorkspaceAccess)
- `SandboxConstants` — 默认值常量 (images/prefixes/tool allow-deny lists/state 目录)
- `SandboxShared` — session key slug + scope key 解析 + agent ID 提取
- `SandboxConfigHash` — SHA-1 配置哈希（标准化 Map/List 后计算，用于变更检测）
- `SandboxConfigResolver` — 配置合并逻辑（agent→global→default）：Docker/Browser/Prune
- `SandboxToolPolicyResolver` — 工具策略模式匹配 (wildcard/exact/all) + agent→global→default 解析
- `SandboxDocker` — Docker CLI 执行 (ProcessBuilder) + 镜像管理 + 容器状态检查 + create-args 构建
- `SandboxRegistry` — JSON 文件持久化注册表 (容器 + 浏览器) CRUD
- `SandboxPruner` — 闲置/过期容器自动清理（5 分钟节流）
- `SandboxRuntimeStatus` — 运行时沙箱决策 + 被阻止工具消息格式化
- `SandboxPaths` — 路径安全：逃逸检测 + 符号链接阻止 + Unicode 空格归一化
- `SandboxManager` — 容器列表/删除 + 浏览器桥接内存缓存

## Phase 14.5 — 编译错误修复

### Fixed
- `openclaw-common/pom.xml` — 添加 Lombok (provided scope) + SLF4J API 依赖，修复 30 个编译错误（`@Data` 注解未处理导致 getter/setter 缺失）
- `ToolAbortHandler` — 修复 `executeWithAbortCheck()` 中未捕获的 checked Exception

### Result
- `mvn compile` → BUILD SUCCESS (0 errors)

## Phase 14 — P1 Tool System Extensions

### Added (tools/policy/ 包, 9 files, 1234 lines)
- `ToolPolicyTypes` — 共享类型 (ToolPolicy/PluginToolGroups/AllowlistResolution)
- `ToolPolicyUtils` — 名称归一化 + 工具分组 + profile + 插件展开
- `ToolPolicyMatcher` — 编译模式匹配 + 策略过滤
- `ToolPolicyResolver` — 有效策略链解析 (global/agent/provider)
- `ToolSchemaUtils` — JSON Schema 归一化 (anyOf/oneOf 展平 + Gemini 清理)
- `ToolParamUtils` — Claude Code 参数别名 + 必填参数校验
- `ToolAbortHandler` — abort 异常 + 执行前检查
- `ToolBeforeCallHook` — 插件 hook 接口 (stub)
- `ToolCreator` — 策略解析 + 顺序过滤编排器

## Phase 13 — P0 Core Engine Layer (Embedded Subscribe)

### Added (runtime/subscribe/ 包, 14 files, 2202 lines)
- `SubscribeTypes` — 订阅系统类型定义
- `HandlerTypes` — handler 回调类型
- `EmbeddedSubscribe` — 主入口：消息订阅 + 事件分发
- `EmbeddedAgent` — 嵌入式 agent 接口
- `EmbeddedMessaging` — 嵌入式消息传递
- `EmbeddedUtils` — 嵌入式工具函数
- `MessageHandlers` — 消息处理器（文本/工具结果/错误）
- `ToolHandlers` — 工具调用处理器
- `LifecycleHandlers` — 生命周期处理器（开始/完成/取消）
- `SubscribeHandlers` — 订阅事件处理器
- `RawStreamHandler` — 原始流处理器
- `RunManager` — 运行管理器（启动/停止/状态）
- `SubscribeToolUtils` — 订阅工具辅助函数
- `BlockChunker` — 流式内容分块器

## Phase 12 — Auth Profiles 子系统

### Added (auth/ 包, 13 files, 1667 lines)
- `AuthProfileTypes` — credential 类型 (api_key/token/oauth) + usage stats + store 数据结构
- `AuthProfileConstants` / `AuthProfilePaths` — 常量 + 路径解析
- `AuthProfileStoreManager` — store CRUD + 文件锁 + 合并 + legacy 迁移
- `AuthProfileOrder` — round-robin 排序 + cooldown + type 优先级
- `AuthProfileProfiles` — profile CRUD + provider 过滤 + order 管理
- `AuthProfileUsage` — 成功/失败追踪 + 指数退避 + billing disable
- `AuthProfileOAuth` — credential 解析 + token 刷新 + 主 agent fallback
- `AuthProfileRepair` / `AuthProfileDoctor` — 诊断 + legacy 迁移
- `AuthProfileDisplay` — 人类可读 label
- `AuthProfileSessionOverride` — session 级 auto-rotation
- `AuthProfileExternalSync` — Qwen/MiniMax CLI 同步

## Phase 11 — 缺失功能模块


### Added
- `AgentDefaults` — 默认 provider/model/context-tokens 常量
- `DateTimeUtils` — 时区解析 + 12/24h 检测 + timestamp 标准化 + 用户时间格式化
- `AgentContext` — model context window 缓存（线程安全）
- `AgentStep` — 嵌套 agent 步骤调度（gateway stub）
- `ChannelTools` — channel 动作枚举 + 工具聚合 + message hints
- `AnthropicPayloadLog` — JSONL payload/usage 调试日志
- `CloudflareAIGateway` — CF AI Gateway 常量 + model定义 + URL 构建
- `BedrockDiscovery` — AWS Bedrock 模型发现 + 缓存（stub SDK）
- `NodesUtils` — 节点列表解析 + 模糊匹配 + 默认节点选择
- `ImageToolHelpers` — data URL 解码 + 图片模型配置 + 视觉模型解析



### Added
- `RunParams` — 运行参数 record + builder（session/sender/model/tool/timeout 等 40+ 字段）
- `ToolSplit` — 工具分割为 builtIn/custom + ToolDefinition 转换
- `Lanes` — 并发 lane 管理（session/global/embedded）
- `AbortUtils` — 取消信号检测（异常链遍历）
- `ExtraParams` — 供应商参数解析（temperature/maxTokens/cacheRetention）+ OpenRouter 头
- `ThinkingFallback` — LLM 错误消息解析 + thinking 级别回退
- `MessagingDedupe` — 消息去重（emoji 剥离 + 子串匹配）
- `OpenAIHelpers` — OpenAI reasoning 块降级（orphaned signature 清理）
- `RunImages` — 图片引用检测（路径/file:// URL/媒体附件）+ 文件加载 + sandbox 限制
- `RunPayloads` — 回复 payload 构建（error/tool-meta/reasoning/answer + 静默/去重过滤）
- `ExtensionPaths` — 扩展配置解析（上下文窗口/压缩模式/缓存剪枝）



### Added
- `SessionsHelpers` — session key resolution, A2A policy engine, session kind classification, sandbox visibility, agent ID resolution
- `SessionsSendHelpers` — announce target parsing from session keys, A2A context builders (message/reply/announce), ping-pong turn config
- `SessionsListTool` — list sessions with kind/limit/activeMinutes filters (gateway stub)
- `SessionsHistoryTool` — fetch session message history with sanitization (gateway stub)
- `SessionsSendTool` — cross-session messaging with A2A policy checks, session key/label resolution (gateway stub)
- `SessionsSpawnTool` — spawn sub-agent in isolated session with model/thinking overrides (gateway stub)
- `SessionsSendA2ATool` — A2A flow orchestration: ping-pong conversation turns + announce delivery (gateway stub)
- `OpenClawConfig` extensions: `SessionConfig`, `ToolsConfig`, `AgentToAgentConfig`, `SandboxDefaults`, `SubagentDefaults`
- Registered all 4 session tools in `OpenClawToolFactory`

## 2026-02-12 — Phase 8: 核心 Agent 工具实现

### Added
- **MessageTool** — 统一消息动作工具 (send/reply/react/delete/pin 等)
- **MemoryTool** — 关键词搜索 MEMORY.md + memory/*.md
- **WebFetchTool** — URL 抓取 + SSRF 防护 + HTML→Markdown
- **ImageTool** — 图片分析 (文件/base64)，vision model stub
- **CronTool** — 定时任务管理，gateway stub
- **AgentsListTool** — Agent 列表 (从 config 读取)
- **NodesTool** — 远程节点交互，gateway stub
- **CanvasTool** — 画布控制，gateway stub
- **TtsTool** — 文本转语音 stub

### Changed
- **ToolParamUtils** — 新增 `toJsonString()` + `readIntegerParam()`
- **OpenClawToolFactory** — 注册 9 个新工具

## 2026-02-12 — Phase 7: pi-embedded-runner 运行编排核心

### Added
- **EmbeddedRunTypes** — 运行结果类型定义（AgentMeta/RunMeta/RunResult/CompactResult/SandboxInfo），对应 TS `pi-embedded-runner/types.ts`
- **ErrorClassifier** — 错误分类（rate_limit/billing/auth/timeout/overloaded/context_overflow/model_not_found）+ failover 资格判定 + 用户文本清理，对应 TS `pi-embedded-helpers/errors.ts`
- **HistoryManager** — DM 历史轮数限制 + session key 解析 + provider DM limit 查找，对应 TS `pi-embedded-runner/history.ts`
- **TurnValidator** — Gemini turn 排序修复（合并连续 assistant）+ Anthropic turn 排序修复（合并连续 user）+ provider 分发，对应 TS `pi-embedded-helpers/turns.ts`
- **SessionSanitizer** — Session 历史清理：tool call ID 规范化/去重、thinking signature 清理、空 text block 移除、Google 首条消息 user 保证，对应 TS `pi-embedded-runner/google.ts`
- **ActiveRunRegistry** — 线程安全活跃 run 管理：注册/清除/queue message/abort/wait + timeout + waiter 通知，对应 TS `pi-embedded-runner/runs.ts`
- **RunOrchestrator** — 高层运行编排：auth profile 轮换 + failover 重试循环 + Anthropic refusal scrub + usage 聚合 + 结果构建，对应 TS `pi-embedded-runner/run.ts`

### Changed
- **AgentRunner** — 新增 `provider`/`userMessage`/`maxTurns` 字段；拆分为同步 `run()` + 异步 `runAsync()`；`ErrorClassifier` 集成替换 `FailoverError`
- **AgentMethodRegistrar** / **ChatAgentBridgeImpl** — `run()` → `runAsync()` 调用适配


## 2026-02-12 — Phase 6: Agent 工具集完善

### Added
- **ToolParamUtils** — 工具参数读取工具函数（readStringParam/readNumberParam/readStringArrayParam/readBooleanParam + jsonResult），对应 TS `tools/common.ts`
- **ToolSchemaAdapter** — JSON Schema 归一化：anyOf/oneOf union 展平、Gemini 不支持关键字清理、强制 `type: "object"` 兼容 OpenAI，对应 TS `pi-tools.schema.ts`
- **ToolDefinitionAdapter** — 工具定义 → LLM provider 格式适配（Anthropic tool_use / OpenAI function calling / Gemini function declarations）+ safeExecute 错误包装，对应 TS `pi-tool-definition-adapter.ts`
- **ToolDisplay** — 工具 UI 显示解析：emoji/title/label/verb/detail 从 `tool-display.json` 读取 + action 级别匹配 + 路径缩写 + detail key 查找，对应 TS `tool-display.ts`
- **CodingToolFactory** — Coding 工具组装工厂：聚合基础工具 + 扩展工具 + ToolPolicy 过滤 + ToolRegistry 自动创建，对应 TS `pi-tools.ts`
- **OpenClawToolFactory** — OpenClaw 扩展工具注册工厂：SessionStatusTool + GatewayTool 实例化，对应 TS `openclaw-tools.ts`
- **SessionStatusTool** — Session 状态展示工具：sessionKey/model/time 状态卡 + 可选 model override，对应 TS `session-status-tool.ts`
- **GatewayTool** — Gateway 管理工具：restart/config.get/config.apply/config.patch/config.schema/update.run 六种 action，对应 TS `gateway-tool.ts`
- **tool-display.json** — 工具显示配置资源文件（从 TS 源同步）

### Changed
- **ToolRegistry** — 新增 `filterByPolicy(ToolPolicy)` 策略过滤 + `toProviderDefinitions(String)` provider 格式输出

## 2026-02-12 — Phase 5E: Subagent + AuthHealth + CacheTrace

### Added
- **SubagentRegistry** — 子 agent run 注册/完成/清理/JSON 持久化/TTL sweeper/subagent 系统 prompt/announce callback
- **AuthHealthService** — OAuth/token/API key 健康评估 + provider 聚合 + refresh token 自动刷新检测
- **CacheTrace** — JSONL 诊断 cache 跟踪 + SHA-256 digest + async writer + env/config 配置解析

## 2026-02-12 — Phase 5D: Hooks 事件系统 + Skills 技能加载

### Added
- **InternalHookRegistry** — 事件驱动 hook 注册/触发（eventKey→handler[]，type/type:action 双层匹配，错误隔离）
- **SkillTypes** — Skill/SkillEntry/SkillMetadata/SkillInvocationPolicy/SkillSnapshot 类型定义
- **SkillFrontmatterParser** — YAML frontmatter 解析 + OpenClaw metadata JSON 提取 + invocation policy
- **SkillLoader** — 多源技能发现（workspace/bundled/managed）+ OS 过滤 + prompt 构建

## 2026-02-12 — Phase 5C: Session 基础设施层

### Added
- **SessionWriteLock** — 文件锁 + PID 跟踪 + stale 检测 + 引用计数 + JVM shutdown hook
- **AgentPaths** — 状态目录/agent 目录/workspace 路径解析（env override）
- **AgentScope** — Agent 列表/默认 agent/sessionKey→agentId/config/model/fallback 解析
- **BashProcessRegistry** — 后台进程生命周期：output 缓冲 + cap 截断 + TTL sweeper
- **BootstrapResolver** — 工作区 `.openclaw/` bootstrap 文件加载 + embedded context 构建

## 2026-02-12 — Phase 5B: Agent Runtime 核心组件

### Added
- **FailoverError** — 错误分类异常 + HTTP 状态码映射 + regex 模式匹配（auth/billing/rate_limit/timeout/context_overflow）
- **CompactionService** — 会话历史压缩：token 估算 + 自适应裁剪 + LLM 摘要 + 分段合并
- **AgentIdentity** — Agent 身份解析、消息前缀、响应前缀、ack 表情

### Changed
- `SystemPromptBuilder` 重写 — +PromptMode (FULL/MINIMAL/NONE) + time/skills/memory/docs/userIdentity 段
- `AgentRunner` — 集成 CompactionService（context 超限自动压缩）+ FailoverError 错误包装 + agentId/compactionEnabled 字段

### Removed
- `ModelFallbackHandler` — 功能已被 Phase 5A 的 `ModelFallbackRunner` 完整覆盖

## 2026-02-12 — Phase 5A: Model/Provider 基础设施

### Added
- **ModelCatalogService** — 模型目录缓存 + 多源聚合（providers + config + models.json）
- **AuthProfileStore** — Auth Profile CRUD + JSON 持久化 + cooldown + round-robin ordering
- **ProviderConfigPersistence** — models.json 读写 + implicit/explicit provider merge
- **ModelFallbackRunner** — 异步 fallback 链执行 + 错误分类 + abort 检测
- `ModelSelector.ModelCatalogEntry` record — 含 vision/reasoning 能力标记
- `ModelSelector.modelSupportsVision()` / `findModelInCatalog()` / `resolveThinkingDefault()` / `buildAllowedModelSet()`
- `ModelProviderRegistry.getProvider()` / `listAllModels()` / `registerFromConfig()`

### Changed
- `ModelSelector.DEFAULT_MODEL` → `claude-opus-4-6`（与 TS `defaults.ts` 对齐）
- `ModelProviderRegistry.discoverFromEnvironment()` 现在实际创建并注册 provider 实例
- `OpenClawConfig.ProviderConfig` 新增 `enabled` 字段
- `OpenClawConfig.AgentDefaults` 新增 `modelAllowlist` 字段
- `OpenClawConfig.AgentEntry` 新增 `modelFallbacks` 字段

## 2026-02-12 — Phase 4: TS↔Java RPC 方法差距补齐

### Added
- **MiscMethodRegistrar** — 注册 17 个方法：`talk.mode`、`voicewake.get/set`、`skills.install/update`（简化）+ 12 个 stub（send/poll/tts.*/browser.request/web.login.*/update.run）
- **AgentTimestampService** — 消息时间戳注入（对应 TS agent-timestamp.ts）
- **AgentJobTracker** — Agent run 生命周期追踪 + `waitForJob` API（对应 TS agent-job.ts）

### Modified
- **GatewayBeanConfig** — 注册 AgentTimestampService、AgentJobTracker Bean

---

## 2026-02-12 — Phase 3E: Node/Device Management

### Added
- **NodeRegistry** — 在线节点会话管理、invoke/result 生命周期与超时处理
- **NodePairingService** — 节点配对 JSON 文件持久化（request/approve/reject/verify/rename）
- **DevicePairingService** — 设备配对 JSON 文件持久化 + token rotate/revoke
- **NodeDeviceMethodRegistrar** — 16 个 RPC 方法：`node.pair.*`(5)、`node.list/describe/invoke/invoke.result/event/rename`(6)、`device.pair.*`(3)、`device.token.*`(2)
- **WizardMethodRegistrar** — 4 个 `wizard.*` RPC 方法（start/next/cancel/status），内存会话状态机

### Modified
- **GatewayBeanConfig** — 注册 NodeRegistry、NodePairingService、DevicePairingService Bean

---

## 2026-02-12 — Phase 3D: Streaming LLM Subscriptions

### Added
- **ModelProvider.StreamListener** — 流式事件回调接口（onText/onToolUse/onUsage）
- **AnthropicProvider.chatStream()** — Anthropic SSE 流式解析（content_block_delta/tool_use/message_stop）
- **OpenAICompatibleProvider.chatStream()** — OpenAI SSE 流式解析（choices.delta.content/tool_calls）

### Modified
- **ModelProvider** — 新增 `chatStream()` 默认方法（fallback 非流式）
- **AgentRunner** — `executeLoop()` 改用 `chatStream()` + StreamListener，实时 delta 推送

---

## 2026-02-12 — Phase 3C: OpenAI-Compatible HTTP API

### Added
- **OpenAiTypes** — OpenAI `/v1/chat/completions` 请求/响应类型
- **ModelCatalog** — 模型目录管理

---

## 2026-02-12 — Phase 3B: Core RPC Completion & Authorization

### Added
- **SessionChannelMethodRegistrar** — `sessions.list`、`sessions.preview` 等 session 通道方法
- **MethodAuthorizer** — 方法级 scope 鉴权（admin/read/write/pairing/approvals）

---

## 2026-02-12 — Phase 3A: Agent Engine + Cron/System RPC

### Added
- **SystemMethodRegistrar** — `health`、`status`、`models.list`、`agents.list` 等系统方法
- **CronService / CronJob / CronRunLog** — 定时任务引擎 + CRUD RPC

---

## 2026-02-12 — Phase 2B: Config Write, Usage Tracking, Logs

### Added
- **ExtendedMethodRegistrar** — `config.set/patch/apply/schema`、`usage.status/cost`、`logs.tail`

---

## 2026-02-12 — Phase 2A: Models List, Enhanced Health, Tests

### Added
- 全面集成测试覆盖（WebSocket 握手、方法鉴权、config RPC）

## 2026-02-12 — Phase 1: Core Dialogue Loop Completion

### Added
- **ChatAgentBridge** — 接口 + 实现，解耦 gateway ↔ agent 模块依赖
- **Session RPC 方法** — `session.get`、`session.patch`、`session.delete`、`session.reset`
- **Agent RPC 方法** — `agent.list`、`agent.identity.get`
- **Chat RPC 方法** — `chat.inject`（注入消息到 session）
- **SystemPromptBuilder** — 自动构建系统提示词（Identity/Runtime/Workspace/Tools），集成到 AgentRunner
- **grep_search 工具** — FileTools 新增递归文件内容搜索
- **AuthService** — 认证服务
- **NetUtils** — 网络工具类
- **ProtocolTypes** — 协议类型定义
- **EventBroadcaster** — WebSocket 事件广播
- **MethodAuthorizer** — 方法鉴权
- **SessionTranscriptStore** — JSONL 对话记录持久化

### Modified
- **AgentRunner** — 集成 SystemPromptBuilder，无 systemPrompt 时自动构建
- **AgentBeanConfig** — 注册 grepSearch 工具
- **CoreMethodRegistrar** — 扩展 session/agent 方法处理
- **SessionStore** — 新增 `updateSession` 原子更新方法
- **OpenClawConfig** — AgentEntry 新增 `description` 字段
- **AcpSession** — 扩展 session 数据模型
- **ChatMethodHandler** — 重构为依赖 ChatAgentBridge 接口
- **GatewayBeanConfig / GatewayWebSocketHandler / WebSocketConfig** — 适配新组件

### Deleted
- **JsonRpcMessage** — 未被引用的遗留类

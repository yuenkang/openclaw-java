# Changelog

## Phase 10 — Runner 运行路径补全

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

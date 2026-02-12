# Changelog

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

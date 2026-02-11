# Phase 4-6 细化实施计划

> 基于已完成的 Phase 1-3（24个Java类），细化剩余工作。

---

## 已有代码盘点

| 模块 | 已有类 |
|------|--------|
| **common** | `OpenClawConfig`, `ConfigService`, `AcpSession`, `JsonRpcMessage` |
| **gateway** | `GatewayWebSocketHandler`, `GatewayConnection`, `GatewayMethodRouter`, `WebSocketConfig`, `RouteResolver`, `CoreMethodRegistrar`, `GatewayBeanConfig` |
| **agent** | `AgentTool`, `ToolRegistry`, `ModelProvider`, `ModelProviderRegistry`, `AnthropicProvider`, `OpenAICompatibleProvider`, `AgentRunner`, `ExecTool`, `FileTools`, `AgentBeanConfig` |
| **channel** | `ChannelOutboundAdapter`（接口） |
| **plugin** | `OpenClawPlugin`（接口） |
| **app** | `OpenClawApplication` |

---

## Phase 4: 渠道适配器（~6个新类）

### 4.1 渠道注册表
#### [NEW] `ChannelRegistry.java` (channel/registry)
- 渠道元信息注册（id, name, aliases, capabilities）
- 对应 TS `registry.ts`

#### [NEW] `ChannelDock.java` (channel/dock)
- 渠道运行时配置（outbound设置, streaming行为, 格式化选项）
- 对应 TS `dock.ts`

### 4.2 消息投递
#### [NEW] `MessageDeliveryService.java` (channel/delivery)
- 统一消息投递：文本分块、媒体发送、取消支持
- 对应 TS `deliver.ts`

#### [NEW] `TargetResolver.java` (channel/routing)
- 消息目标解析（用户ID / 群组ID / 目录查找 + 30分钟缓存）
- 对应 TS `target-resolver.ts`

### 4.3 Telegram适配器
#### [NEW] `TelegramOutboundAdapter.java` (channel/telegram)
- 实现 `ChannelOutboundAdapter`
- HTML格式化、reply-to、thread支持
- 使用 OkHttp 调用 Telegram Bot API

#### [NEW] `ChannelBeanConfig.java` (channel)
- Spring Bean 装配

---

## Phase 5: 插件系统 + 定时任务（~5个新类）

### 5.1 插件加载
#### [NEW] `PluginLoader.java` (plugin)
- Java SPI 发现（ServiceLoader）
- 插件配置验证 + 缓存
- 对应 TS `loader.ts`

#### [NEW] `PluginRegistry.java` (plugin)
- 管理已注册组件（tools, hooks, commands, channels, services）
- 对应 TS `registry.ts`

### 5.2 定时任务
#### [NEW] `CronService.java` (gateway/cron)
- Job CRUD + 持久化存储（JSON文件）
- 基于 `ScheduledExecutorService` 调度
- due/force 执行模式
- 对应 TS `cron/service.ts`

#### [NEW] `CronJob.java` (gateway/cron)
- Job 数据模型（id, schedule, agentId, message, enabled, lastRun, nextRun）

#### [NEW] `CronRunLog.java` (gateway/cron)
- 运行日志记录
- 对应 TS `run-log.ts`

---

## Phase 6: 测试 + 启动验证（~4个测试类）

### 6.1 单元测试
#### [NEW] `ConfigServiceTest.java` (common)
- 测试环境变量替换
- 测试默认值应用
- 测试配置缓存

#### [NEW] `SessionStoreTest.java` (gateway)
- 测试session CRUD
- 测试run跟踪和取消

#### [NEW] `ToolRegistryTest.java` (agent)
- 测试工具注册/查找
- 测试ExecTool执行

### 6.2 集成验证
#### 启动测试
```bash
# 启动应用并验证WebSocket
mvn spring-boot:run -pl openclaw-app

# 用wscat测试
wscat -c ws://127.0.0.1:3578/ws
> {"jsonrpc":"2.0","method":"status","id":"1"}
```

---

## 工作量估算

| Phase | 新增类 | 预计耗时 |
|-------|--------|---------|
| 4. 渠道 | 6 | ~2轮对话 |
| 5. 插件+Cron | 5 | ~2轮对话 |
| 6. 测试 | 3-4 | ~1轮对话 |
| **总计** | **14-15** | **~5轮对话** |

完成后项目总计：**~38-39个Java类**，覆盖 OpenClaw 核心架构的完整复刻。

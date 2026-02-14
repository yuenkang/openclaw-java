# OpenClaw Java

OpenClaw 的 Java 全栈实现 —— 基于 Spring Boot 3.3 的 AI Agent Gateway，通过 WebSocket 自定义帧协议（req/res/event）提供全功能 Agent 接口。

> **当前进度**: Phase 30 / 576 个源文件 / ~83,000 行 Java 代码

## 架构

```
┌────────────────────────────────────────────────────────────────┐
│                         openclaw-app                           │
│              (Spring Boot 入口 + 模块桥接 + OpenAI 兼容 API)    │
├────────────┬─────────────┬──────────────┬─────────────────────┤
│  gateway   │    agent    │   channel    │      plugin         │
│ WebSocket  │   Runtime   │  Adapters    │       SPI           │
│ RPC Route  │   LLM Loop │  Telegram    │     Loader          │
│ Session    │   Tools     │  Discord     │    Registry         │
│ Cron       │   Hooks     │  Outbound    │                     │
│ Outbound   │   Memory    │  Onboarding  │                     │
├────────────┴─────────────┴──────────────┴─────────────────────┤
│                          common                                │
│    Config · Models · Protocol · Sessions · Auth · Media · CLI  │
└────────────────────────────────────────────────────────────────┘
```

## 模块说明

| 模块 | 文件数 | 说明 |
|------|--------|------|
| `openclaw-common` | 32 | 配置管理 (OpenClawConfig)、数据模型、协议类型定义、认证 (Auth Profile)、CLI 参数解析 |
| `openclaw-gateway` | 109 | WebSocket 服务器、会话管理、方法路由 (MethodRouter)、Cron 调度、出站消息投递 (Outbound)、运行时重载 |
| `openclaw-agent` | 329 | Agent 执行引擎、多模型提供者 (Anthropic/OpenAI/Ollama)、内置工具 (Exec/File/Browser)、指令处理、Hooks、Memory |
| `openclaw-channel` | 96 | 渠道注册 (Registry)、消息投递 (Dock)、ACK 反应、Telegram Bot 完整层 (18 文件)、Discord 适配器、出站适配器 |
| `openclaw-plugin` | 5 | SPI 插件加载器、注册中心、清单解析 |
| `openclaw-app` | 5 | Spring Boot 入口、模块桥接、OpenAI 兼容 REST API |

**总计**: 576 个 Java 源文件，6 个测试文件

## 快速开始

### 环境要求

- Java 17+
- Maven 3.8+

### 构建

```bash
mvn clean install
```

### 运行

```bash
# 设置 API Key
export ANTHROPIC_API_KEY=your-key-here

# 可选：自定义 API 地址（代理、自建网关等）
export ANTHROPIC_BASE_URL=https://your-proxy.example.com/v1

# 启动
mvn spring-boot:run -pl openclaw-app
```

服务启动在 `ws://127.0.0.1:3578/ws`

### 环境变量

| 变量 | 说明 | 默认值 |
|------|------|--------|
| `ANTHROPIC_API_KEY` | Anthropic API 密钥 | — |
| `ANTHROPIC_BASE_URL` | Anthropic API 地址 | `https://api.anthropic.com/v1` |
| `OPENAI_API_KEY` | OpenAI API 密钥 | — |
| `OLLAMA_BASE_URL` | Ollama 本地服务地址 | `http://127.0.0.1:11434/v1` |
| `TELEGRAM_BOT_TOKEN` | Telegram Bot Token | — |

### 测试

```bash
mvn test
```

## WebSocket 协议

采用自定义帧协议 (与 TypeScript 版本对齐)，**非 JSON-RPC**：

### 帧格式

| 类型 | 格式 | 说明 |
|------|------|------|
| Request | `{"type":"req", "id":"1", "method":"...", "params":{}}` | 客户端→服务端请求 |
| Response | `{"type":"res", "id":"1", "ok":true, "payload":{}}` | 服务端→客户端响应 |
| Event | `{"type":"event", "event":"...", "payload":{}}` | 服务端→客户端事件推送 |

### 握手流程

```
服务端 → connect.challenge {nonce, ts}
客户端 → req {method:"connect", params:{client, role, auth, scopes}}
服务端 → res {payload: hello-ok {protocol, server, features, snapshot}}
```

### 方法示例

```json
// 状态检查
{"type":"req","id":"1","method":"status","params":{}}

// 获取配置
{"type":"req","id":"2","method":"config.get","params":{}}

// 重载配置
{"type":"req","id":"3","method":"config.reload","params":{}}
```

### 会话管理

```json
// 创建会话
{"type":"req","id":"4","method":"session.create","params":{"sessionKey":"my-session","cwd":"/tmp"}}

// 列出会话
{"type":"req","id":"5","method":"session.list","params":{}}

// 取消运行
{"type":"req","id":"6","method":"session.cancel","params":{"sessionId":"xxx"}}
```

### Agent 对话

```json
// 快速对话
{"type":"req","id":"7","method":"agent.message","params":{
  "message":"你好，请介绍一下你自己"
}}

// 完整控制
{"type":"req","id":"8","method":"agent.run","params":{
  "modelId":"anthropic/claude-sonnet-4-5",
  "messages":[{"role":"user","content":"帮我写一个 hello world"}],
  "systemPrompt":"You are a helpful assistant",
  "maxTokens":4096
}}
```

### Cron 调度

```json
// 列出定时任务
{"type":"req","id":"9","method":"cron.list","params":{}}

// 强制执行  
{"type":"req","id":"10","method":"cron.force","params":{"jobId":"xxx"}}
```

## 核心模块详解

### Agent 执行引擎 (`openclaw-agent`)

- **多轮对话**: 用户→LLM→工具→LLM→…→回复循环
- **模型提供者**: Anthropic Claude、OpenAI GPT、Ollama 本地、vLLM 兼容
- **内置工具**: 命令执行 (ExecTool)、文件读写 (FileTools)、浏览器控制
- **指令处理**: 快速回复、队列验证、Follow-up
- **Hooks 系统**: 内置 Hook (boot-md/command-logger/session-memory)、Workspace Hook 加载、优先级管理
- **Memory 系统**: 记忆索引、关键字搜索、后端配置

### Telegram Bot (`openclaw-channel/telegram`)

完整的 Telegram Bot 实现 (18 个类):

- Bot 生命周期管理 (轮询/Webhook)
- Update 去重 (LRU) + Media-group 合批
- Allow-list 访问控制
- 富消息上下文 (sender/chat/media/mentions)
- 原生命令 (/start /help /model 等)
- 草稿消息实时编辑 (Draft Stream)
- 连接健康监控
- HTTP 代理支持

### 出站消息投递 (`openclaw-gateway/outbound`)

- 目标解析 (peer/group/channel 路由)
- Payload 归一化
- 格式化 + 分块
- 渠道适配注册
- Agent 投递计划

### Cron 调度 (`openclaw-gateway/cron`)

- Cron 表达式解析 + 下次运行时间计算
- Job 配置规范化
- 投递目标解析 (agent/session/channel)
- 事件状态持久化

## 技术栈

- **Spring Boot 3.3** — Web + WebSocket + Scheduling
- **Jackson** — JSON 序列化
- **OkHttp** — HTTP 客户端（模型 API / 渠道 API）
- **java.net.http.HttpClient** — Telegram Bot API
- **Caffeine** — 本地缓存
- **Lombok** — 代码简化
- **docker-java** — 沙箱执行
- **JUnit 5 + Mockito** — 测试

## 配置

默认配置文件路径：`~/.openclaw/config.json`

```json
{
  "model": "anthropic/claude-sonnet-4-5",
  "modelAliases": {
    "sonnet": "anthropic/claude-sonnet-4-5",
    "gpt4": "openai/gpt-4"
  },
  "gateway": {
    "port": 3578,
    "host": "127.0.0.1"
  },
  "agents": {
    "list": [
      {
        "id": "default",
        "name": "Default Agent",
        "model": "anthropic/claude-sonnet-4-5"
      }
    ]
  },
  "channels": {
    "telegram": {
      "token": "BOT_TOKEN",
      "allowFrom": ["123456789"],
      "dmPolicy": "allowlist"
    }
  }
}
```

## 项目目录结构

```
openclaw-java/
├── pom.xml                     # 父 POM (Spring Boot 3.3, Java 17)
├── CHANGELOG.md                # 变更日志 (Phase 1–30)
├── README.md                   # 本文件
├── doc/                        # 设计文档 + 学习路线图
│   ├── notes/                  # 16 篇架构学习笔记
│   ├── openclaw_learning_roadmap.md
│   └── walkthrough.md          # 实现进度记录
├── openclaw-common/            # 公共模块
│   └── src/main/java/com/openclaw/common/
│       ├── config/             # OpenClawConfig, ConfigService
│       ├── models/             # 数据模型
│       ├── auth/               # 认证
│       └── cli/                # CLI
├── openclaw-gateway/           # Gateway 模块
│   └── src/main/java/com/openclaw/gateway/
│       ├── chat/               # 对话管理
│       ├── cron/               # Cron 调度
│       ├── outbound/           # 出站消息投递
│       ├── runtime/            # 运行时管理
│       └── session/            # 会话管理
├── openclaw-agent/             # Agent 模块
│   └── src/main/java/com/openclaw/agent/
│       ├── core/               # AgentRunner 执行引擎
│       ├── directive/          # 指令处理
│       ├── hooks/              # Hooks 系统
│       ├── memory/             # Memory 系统
│       ├── models/             # 模型提供者
│       └── tools/              # 内置工具
├── openclaw-channel/           # 渠道模块
│   └── src/main/java/com/openclaw/channel/
│       ├── telegram/           # Telegram Bot (18 文件)
│       ├── discord/            # Discord 适配器
│       ├── dock/               # 渠道运行时配置
│       ├── registry/           # 渠道注册
│       ├── adapter/            # 出站适配器
│       └── normalize/          # 消息归一化
├── openclaw-plugin/            # 插件模块
│   └── src/main/java/com/openclaw/plugin/
└── openclaw-app/               # 启动模块
    └── src/main/java/com/openclaw/app/
```

## 开发迭代历程

| Phase | 内容 | 文件数 |
|-------|------|--------|
| 1–3 | 基础框架 + Gateway + Agent Runtime | 24 |
| 4–6 | 渠道适配器 + 插件/Cron + 测试 | 18 |
| 7–12 | Agent 高级功能 (指令处理/Follow-up/模型扩展) | ~80 |
| 13–18 | Gateway 扩展 (会话/RPC/聊天/运行时) | ~90 |
| 19–23 | Agent 深度 (工具链/Auth/CLI/系统提示) | ~120 |
| 24 | 编译错误全面修复 (22 文件) | — |
| 24.5 | Media + Memory 基础设施 | 8 |
| 25 | Hooks/Plugins/Cron 模块补齐 | 9 |
| 26 | Gateway 运行时补全 | 11 |
| 27 | Infra 出站消息投递 | 15 |
| 28 | Cron + Hooks 补全 | 6 |
| 29 | Telegram Bot 完整层 | 18 |
| 30 | Channels 桥接层 | 5 |

## License

MIT

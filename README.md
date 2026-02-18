# OpenClaw Java

OpenClaw 的 Java 全栈实现 —— 基于 Spring Boot 3.3 的 AI Agent Gateway，通过 WebSocket 自定义帧协议（req/res/event）提供全功能 Agent 接口。

> **当前进度**: Phase 37 / 654 个源文件 + 22 个测试文件 / ~99,000 行 Java 代码

📊 [Java vs TypeScript 进度对比](docs/java-vs-typescript.md)

💬 [加入 Telegram 讨论群组](https://t.me/+D9DiVXI3xe43ZDNl) — 欢迎讨论关于项目的一切！

🗣️ [GitHub Issues 灌水区](https://github.com/yuenkang/openclaw-java/issues/2) — 有问题欢迎提问 👏

<img src="docs/telegram-group-qr.png" alt="Telegram 群组二维码" width="200" />

## 架构

```
┌────────────────────────────────────────────────────────────────┐
│                         openclaw-app                           │
│        (Spring Boot 入口 + 模块桥接 + OpenAI 兼容 REST API)    │
├────────────┬─────────────┬──────────────┬─────────────────────┤
│  gateway   │    agent    │   channel    │      plugin         │
│ WebSocket  │   Runtime   │  Adapters    │       SPI           │
│ RPC Route  │   LLM Loop │  Telegram    │     Loader          │
│ Session    │   Tools     │  WeChat      │    Registry         │
│ Cron       │   Hooks     │  Discord     │                     │
│ Outbound   │   Memory    │  Outbound    │                     │
│ OpenAI API │   Browser   │  Onboarding  │                     │
├────────────┴─────────────┴──────────────┴─────────────────────┤
│                          common                                │
│  Config · Models · Protocol · Sessions · Auth · Media · Security│
└────────────────────────────────────────────────────────────────┘
```

## 模块说明


| 模块               | 源文件 | 测试文件 | 说明                                                                                                                |
| ------------------ | ------ | -------- | ------------------------------------------------------------------------------------------------------------------- |
| `openclaw-common`  | 94     | 24       | 配置管理 (90+ 嵌套类型)、日志脱敏、安全审计、Markdown IR 解析/渲染、基础设施 (infra 16 模块)                       |
| `openclaw-gateway` | 117    | 4        | WebSocket 服务器、会话管理、方法路由、Cron 调度、出站消息投递、运行时重载、OpenAI 兼容 HTTP                         |
| `openclaw-agent`   | 354    | 12       | Agent 执行引擎、多模型提供者 (Anthropic/OpenAI/Ollama)、内置工具 (Exec/File/Browser/Image)、指令处理、Hooks、Memory |
| `openclaw-channel` | 104    | 9        | Telegram Bot (18+ 文件) + 微信公众号 (8 文件) + Discord 适配器、渠道注册、消息投递、出站适配器                      |
| `openclaw-plugin`  | 5      | 0        | SPI 插件加载器、注册中心、清单解析                                                                                  |
| `openclaw-app`     | 29     | 7        | Spring Boot 入口、命令系统 (15 个命令模块)、模块桥接、OpenAI 兼容 REST API、浏览器控制                              |

**总计**: 703 个 Java 源文件 (~108k 行)，56 个测试文件 (~8k 行)，612 个测试用例

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

服务启动在 `ws://127.0.0.1:18789`

### 环境变量


| 变量                 | 说明                 | 默认值                         |
| -------------------- | -------------------- | ------------------------------ |
| `ANTHROPIC_API_KEY`  | Anthropic API 密钥   | —                             |
| `ANTHROPIC_BASE_URL` | Anthropic API 地址   | `https://api.anthropic.com/v1` |
| `OPENAI_API_KEY`     | OpenAI API 密钥      | —                             |
| `OPENAI_BASE_URL`    | OpenAI API 地址      | `https://api.openai.com/v1`    |
| `OLLAMA_BASE_URL`    | Ollama 本地服务地址  | `http://127.0.0.1:11434/v1`    |
| `TELEGRAM_BOT_TOKEN` | Telegram Bot Token   | —                             |
| `WECHAT_APP_ID`      | 微信公众号 AppID     | —                             |
| `WECHAT_APP_SECRET`  | 微信公众号 AppSecret | —                             |
| `WECHAT_TOKEN`       | 微信公众号验证 Token | —                             |

### 测试

```bash
mvn test
```

## WebSocket 协议

采用自定义帧协议（与 TypeScript 版本对齐），**非 JSON-RPC**。支持三步握手、双向通信和事件推送。


| 类型     | 格式                                                    | 说明                   |
| -------- | ------------------------------------------------------- | ---------------------- |
| Request  | `{"type":"req", "id":"1", "method":"...", "params":{}}` | 客户端→服务端请求     |
| Response | `{"type":"res", "id":"1", "ok":true, "payload":{}}`     | 服务端→客户端响应     |
| Event    | `{"type":"event", "event":"...", "payload":{}}`         | 服务端→客户端事件推送 |

📖 完整协议文档：[websocket-protocol.md](docs/websocket-protocol.md)（握手流程、方法示例、会话管理、Agent 对话、Cron 调度）

📖 TUI 使用指南：[tui-guide.md](docs/tui-guide.md)（命令列表、模型切换、会话管理、快捷操作）

## 核心模块详解

### Agent 执行引擎 (`openclaw-agent`)

- **多轮对话**: 用户->LLM->工具->LLM->...->回复循环
- **多模态工具返回**: 工具可返回图片等多模态内容给 LLM（截图视觉分析等）
- **模型提供者**: Anthropic Claude、OpenAI GPT、Ollama 本地、OpenAI 兼容 (vLLM/DeepSeek 等)；认证工具: GitHub Copilot OAuth、通义千问 Portal OAuth
- **内置工具**: 命令执行 (ExecTool)、文件读写 (FileTools)、浏览器控制 (BrowserTool)、图片分析 (ImageTool)
- **Skills 系统**: 可扩展技能加载/过滤/注入、frontmatter 解析、环境变量覆盖、热重载 — 📖 [skills-guide.md](docs/skills-guide.md)
- **指令处理**: 快速回复、队列验证、Follow-up
- **Hooks 系统**: 内置 Hook (boot-md/command-logger/session-memory)、Workspace Hook 加载、优先级管理
- **Memory 系统**: 记忆索引、关键字搜索、后端配置
- **Reasoning**: 支持 `reasoning_content` 流式/非流式解析 (Claude extended thinking)

### 命令系统 (`openclaw-app/commands`)

独立的 channel-agnostic 命令系统 (15+ 个命令模块):

- **会话管理**: `/clear` 清除历史、`/usage` 用量统计
- **模型切换**: `/model` 切换模型、`/models` 分页模型列表 (inline keyboard)
- **信息查询**: `/help` `/status` `/status all` `/commands`
- **诊断工具**: `/doctor` 环境诊断 (配置/端口/二进制依赖/更新检查)
- **访问控制**: `/allowlist` 白名单管理 (addme/removeme/list/add/remove)
- **配置管理**: `/config` 运行时配置查看/修改 (支持嵌套路径)
- **高级功能**: `/bash` Shell命令、`/subagent` 子Agent管理、`/tools` 工具列表、`/tts` 语音合成、`/plugins` 插件列表
- **操作审批**: `/approve` 危险操作批准、`/restart` 重启服务

### 日志 · 安全 · Markdown (`openclaw-common`) 🆕

- **日志脱敏** (LogRedact): 16 个内置正则脱敏模式 (API key/token/PEM/Bearer 等)
- **安全审计** (SecurityAudit): 文件权限、敏感变量、root 检测、日志目录审计
- **安全扫描** (SkillScanner): 行级 (exec/eval/ProcessBuilder) + 源码级 (exfiltration) 安全扫描
- **外部内容安全** (ExternalContentSecurity): 15 个注入检测模式 + XML 边界隔离
- **Markdown IR 管道**: 解析 → 中间表示 → 边界排序标记插入 → Telegram HTML / Discord Markdown 渲染
- **安全修复**: `/fix` 命令 — chmod 修复 + 凭证文件保护

### Telegram Bot (`openclaw-channel/telegram`) ⭐ 推荐

> **📌 Telegram Bot 是 OpenClaw 最重要、功能最完整的 Channel 连接方式**，支持私聊/群聊、图片收发、流式输出、会话管理等全部特性。

📖 部署指南：[telegram-bot-setup.md](docs/telegram-bot-setup.md)

完整的 Telegram Bot 实现 (18+ 个类):

- Bot 生命周期管理 (轮询/Webhook)
- Update 去重 (LRU) + Media-group 合批
- **访问控制**: DM allowlist + 群组策略 (open/disabled/allowlist) + per-group `allowFrom` override
- **群组策略**: group/topic `enabled` 检查 → per-group override → `groupPolicy` → 群组 ID 白名单
- **配置热加载**: 每条消息获取最新配置 (含 runtime overrides)，无需重启
- 富消息上下文 (sender/chat/media/mentions)
- **命令系统**: 15+ 斜杠命令 + inline keyboard 回调 + callback query 处理
- **Markdown→HTML 管道**: IR 级 fence-aware 分块，表格自动检测转换
- 图片处理 (接收分析 + 发送图片)
- 草稿消息实时编辑 (Draft Stream)
- 连接健康监控 + HTTP 代理支持

### 微信公众号 (`openclaw-channel/wechat`) ⚠️ 待验证

📖 接入指南：[wechat-setup.md](docs/wechat-setup.md)

完整的微信公众号接入 (8 个类):

- Webhook 验证 (SHA-1 签名) + 消息接收
- access_token 管理 (Caffeine 缓存 110 分钟 TTL)
- 入站消息路由 (text/image/voice/event)
- 客服消息 API 出站发送
- Spring `RouterFunction` 条件注册

📖 渠道配置总览：[channel-configuration.md](docs/channel-configuration.md)

### 浏览器控制 (`openclaw-app/browser`)

- **独立 Netty HTTP 服务器** (端口 18791) -- 与 TypeScript 架构对齐
- 15 种浏览器操作 (status/start/stop/tabs/snapshot/act 等)
- **截图视觉分析** -- 截图自动作为图片发送给 LLM，LLM 可看到页面内容并分析
- 支持有头/无头模式切换 (`headless` 参数)
- OkHttp HTTP 客户端调用浏览器控制服务器

### 持久化层

- **会话历史**: JSONL 对话记录 (TranscriptStore) — 追加/读取/清除，Bot 重启自动恢复 50 条上下文
- **用量追踪**: JSONL 用量记录 (UsageTracker) — 多模型成本估算 (Claude/GPT/DeepSeek/Gemini)
- **会话元数据**: sessions.json 原子写入 (SessionPersistence)

### 出站消息投递 (`openclaw-gateway/outbound`)

- 目标解析 (peer/group/channel 路由)
- Payload 归一化 + 格式化 + 分块
- 渠道适配注册 + Agent 投递计划

### Cron 调度 (`openclaw-gateway/cron`)

- Cron 表达式解析 + 下次运行时间计算
- Job 配置规范化 + 投递目标解析
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
    "port": 18789,
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
    },
    "wechat": {
      "appId": "wx...",
      "appSecret": "...",
      "token": "your-verify-token"
    }
  }
}
```

## 项目目录结构

```
openclaw-java/
├── pom.xml                     # 父 POM (Spring Boot 3.3, Java 17)
├── CHANGELOG.md                # 变更日志 (Phase 1-37)
├── README.md                   # 本文件
├── doc/                        # 设计文档 + 学习路线图
│   ├── notes/                  # 16 篇架构学习笔记
│   ├── openclaw_learning_roadmap.md
│   └── walkthrough.md          # 实现进度记录
├── openclaw-common/            # 公共模块
│   └── src/main/java/com/openclaw/common/
│       ├── config/             # OpenClawConfig, ConfigService, 热加载
│       ├── logging/            # 日志脱敏, 子系统日志, 诊断
│       ├── security/           # 安全审计, 注入检测, 内容安全
│       ├── markdown/           # Markdown IR 解析/渲染引擎
│       ├── infra/              # 重试, 事件总线, 运行时守卫
│       ├── models/             # 数据模型
│       └── auth/               # 认证
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
│       ├── skills/             # Skills 系统
│       └── tools/              # 内置工具
├── openclaw-channel/           # 渠道模块
│   └── src/main/java/com/openclaw/channel/
│       ├── telegram/           # Telegram Bot (18+ 文件)
│       ├── wechat/             # 微信公众号 (8 文件)
│       ├── discord/            # Discord 适配器
│       ├── dock/               # 渠道运行时配置
│       ├── registry/           # 渠道注册
│       ├── adapter/            # 出站适配器
│       └── normalize/          # 消息归一化
├── openclaw-plugin/            # 插件模块
│   └── src/main/java/com/openclaw/plugin/
└── openclaw-app/               # 启动模块
    └── src/main/java/com/openclaw/app/
        ├── commands/           # 命令系统 (15 个命令模块)
        └── ...                 # Spring Boot 入口, 模块桥接
```

## 开发迭代历程


| Phase  | 内容                                                   | 文件数 |
| ------ | ------------------------------------------------------ | ------ |
| 1–3   | 基础框架 + Gateway + Agent Runtime                     | 24     |
| 4–6   | 渠道适配器 + 插件/Cron + 测试                          | 18     |
| 7–12  | Agent 高级功能 (指令处理/Follow-up/模型扩展)           | ~80    |
| 13–18 | Gateway 扩展 (会话/RPC/聊天/运行时)                    | ~90    |
| 19–23 | Agent 深度 (工具链/Auth/CLI/系统提示)                  | ~120   |
| 24     | 编译错误全面修复                                       | 22     |
| 24.5   | Media + Memory 基础设施                                | 8      |
| 25     | Hooks/Plugins/Cron 模块补齐                            | 9      |
| 26     | Gateway 运行时补全                                     | 11     |
| 27     | Infra 出站消息投递                                     | 15     |
| 28     | Cron + Hooks 补全                                      | 6      |
| 29     | Telegram Bot 完整层                                    | 18     |
| 30     | Channels 桥接层                                        | 5      |
| 31     | 微信公众号渠道                                         | 8      |
| 32     | 集成测试 + WebSocket 可靠性修复                        | —     |
| 33     | 浏览器控制 + 图片处理 + 持久化 + 用量追踪              | 10+    |
| 34     | infra/ 核心基础设施模块 (重试/安全/事件/运行时)        | 21     |
| 35     | Browser Control Netty 独立服务 + 截图多模态 LLM 支持   | 8      |
| 35.1   | TUI 修复 (流式/token/模型名/session) + TUI 文档        | 10     |
| 36     | 日志脱敏 · 安全审计 · Markdown IR · Providers 扩展  | 34     |
| 37     | 命令系统重构 · 访问控制对齐 · 配置热加载 · 群组策略 | 25     |
| 38–42 | Telegram 完善 · WeChat · 端到端测试 · 编译修复       | ~30    |
| 43     | infra/ 补齐 (16 模块: dotenv/binaries/restart 等)    | 16     |
| 43.5   | InfraBootstrap 接入主流程                            | 3      |
| 44     | 命令系统深化 (/status /doctor /restart 接入 infra)   | 4      |
| 45     | 测试覆盖率提升 (9 新测试文件, 612 pass)              | 9      |

## License

MIT

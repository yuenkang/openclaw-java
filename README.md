# OpenClaw Java

OpenClaw 的 Java 实现 —— 基于 Spring Boot 的 AI Agent Gateway，通过 WebSocket 提供 JSON-RPC 2.0 接口。

## 架构

```
┌─────────────────────────────────────────────────┐
│                  openclaw-app                    │
│          (Spring Boot 入口 + Bridge)             │
├──────────┬──────────┬───────────┬───────────────┤
│ gateway  │  agent   │  channel  │    plugin     │
│ WebSocket│ Runtime  │ Adapters  │    SPI        │
│ JSON-RPC │ LLM Loop │ Telegram  │   Loader      │
├──────────┴──────────┴───────────┴───────────────┤
│                  common                          │
│        Config · Models · JSON-RPC Types          │
└─────────────────────────────────────────────────┘
```

## 模块说明

| 模块 | 说明 |
|------|------|
| `openclaw-common` | 配置管理、数据模型、JSON-RPC 类型定义 |
| `openclaw-gateway` | WebSocket 服务器、会话管理、RPC 路由、Cron 调度 |
| `openclaw-agent` | Agent 执行引擎、模型提供者(Anthropic/OpenAI)、内置工具 |
| `openclaw-channel` | 渠道注册、消息投递、Telegram 适配器 |
| `openclaw-plugin` | SPI 插件加载器和注册中心 |
| `openclaw-app` | Spring Boot 入口、模块桥接 |

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

### 测试

```bash
mvn test
```

## RPC 方法

通过 WebSocket 发送 JSON-RPC 2.0 消息：

### 系统

```json
// 状态检查
{"jsonrpc":"2.0","id":1,"method":"status","params":{}}

// 获取配置
{"jsonrpc":"2.0","id":2,"method":"config.get","params":{}}
```

### 会话管理

```json
// 创建会话
{"jsonrpc":"2.0","id":3,"method":"session.create","params":{"sessionKey":"my-session","cwd":"/tmp"}}

// 列出会话
{"jsonrpc":"2.0","id":4,"method":"session.list","params":{}}

// 取消运行
{"jsonrpc":"2.0","id":5,"method":"session.cancel","params":{"sessionId":"xxx"}}
```

### Agent 对话

```json
// 快速对话
{"jsonrpc":"2.0","id":6,"method":"agent.message","params":{
  "message":"你好，请介绍一下你自己"
}}

// 完整控制
{"jsonrpc":"2.0","id":7,"method":"agent.run","params":{
  "modelId":"anthropic/claude-sonnet-4-5",
  "messages":[{"role":"user","content":"帮我写一个 hello world"}],
  "systemPrompt":"You are a helpful assistant",
  "maxTokens":4096
}}
```

## 技术栈

- **Spring Boot 3.3** — Web + WebSocket + Scheduling
- **Jackson** — JSON 序列化
- **OkHttp** — HTTP 客户端（模型 API / 渠道 API）
- **Caffeine** — 本地缓存
- **Lombok** — 代码简化
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
  }
}
```

## License

MIT

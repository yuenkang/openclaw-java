# 自定义 API 部署指南

本文档说明如何将 OpenClaw 部署到 Telegram，并配置自定义 API 中转站（第三方 OpenAI 兼容端点）。

---

## 前置条件

- Java 17+
- Maven 3.8+
- Telegram Bot Token（从 [@BotFather](https://t.me/BotFather) 获取）
- OpenAI 兼容 API 的 Key 和 Base URL

## 快速部署

### 1. 创建配置文件

创建 `~/.openclaw/config.json`：

```json
{
  "model": "openai/gpt-4o",
  "models": {
    "providers": {
      "openai": {
        "apiKey": "你的 API Key",
        "baseUrl": "https://你的中转站地址/v1",
        "enabled": true
      }
    }
  },
  "channels": {
    "telegram": {
      "botToken": "你的 Telegram Bot Token",
      "allowFrom": ["你的 Telegram 用户 ID"],
      "connectTimeoutMs": 30000,
      "readTimeoutMs": 60000
    }
  }
}
```

### 2. 构建与启动

```bash
# 构建
mvn clean install -DskipTests

# 启动
mvn spring-boot:run -pl openclaw-app
```

启动成功后，你会看到以下日志：

```
Registered model provider: openai
Registered provider from config: openai (baseUrl=https://你的中转站/v1)
Telegram bot identity: @your_bot (id=123456789)
Telegram bot polling started for account: default
Started OpenClawApplication in 3.x seconds
```

### 3. 测试

在 Telegram 中搜索你的 Bot，发送 `/start`，然后发送任意消息。

---

## 自定义 API 中转站配置

### 工作原理

OpenClaw 通过 `OpenAICompatibleProvider` 支持所有兼容 OpenAI Chat Completions API 的服务。配置 `baseUrl` 后，所有请求会发送到 `{baseUrl}/chat/completions`。

### Provider 注册优先级

```
config.json models.providers → 环境变量 → Ollama (默认)
```

环境变量可以覆盖 config.json 中同名的 provider。

### config.json 方式（推荐）

```json
{
  "models": {
    "providers": {
      "my-relay": {
        "apiKey": "sk-xxx",
        "baseUrl": "https://relay.example.com/v1",
        "enabled": true
      }
    }
  },
  "model": "my-relay/gpt-4o"
}
```

Provider ID（如 `my-relay`）用作模型前缀：`my-relay/gpt-4o`。

### 环境变量方式

```bash
export OPENAI_API_KEY="sk-xxx"
export OPENAI_BASE_URL="https://relay.example.com/v1"
```

或者创建项目根目录下的 `.env` 文件：

```env
OPENAI_API_KEY=sk-xxx
OPENAI_BASE_URL=https://relay.example.com/v1
```

### 支持的 Provider 类型

| Provider ID | 类型              | 说明                                       |
| ----------- | ----------------- | ------------------------------------------ |
| `openai`    | OpenAI Compatible | 官方 API 或任意兼容中转站                  |
| `anthropic` | Anthropic Native  | Claude API（使用独立的 AnthropicProvider） |
| `ollama`    | OpenAI Compatible | 本地 Ollama（无需 API Key，默认注册）      |
| 自定义 ID   | OpenAI Compatible | 任意 OpenAI 兼容服务                       |

### 多 Provider 配置

```json
{
  "models": {
    "providers": {
      "openai-relay": {
        "apiKey": "sk-relay-key",
        "baseUrl": "https://relay1.example.com/v1",
        "enabled": true
      },
      "anthropic": {
        "apiKey": "sk-ant-xxx",
        "enabled": true
      },
      "deepseek": {
        "apiKey": "sk-ds-xxx",
        "baseUrl": "https://api.deepseek.com/v1",
        "enabled": true
      }
    }
  },
  "model": "openai-relay/gpt-4o",
  "modelAliases": {
    "gpt4": "openai-relay/gpt-4o",
    "claude": "anthropic/claude-sonnet-4-5",
    "ds": "deepseek/deepseek-chat"
  }
}
```

配置 `modelAliases` 后，可以在 Telegram 中通过 `/model gpt4` 快速切换模型。

---

## Telegram 配置详解

### Bot Token 获取

1. 在 Telegram 中搜索 `@BotFather`
2. 发送 `/newbot`，按提示设置名称和用户名
3. 获取 Token（格式：`123456789:ABCDefGHI...`）

### 白名单配置

```json
{
  "channels": {
    "telegram": {
      "allowFrom": ["123456789", "@your_username"],
      "groupAllowFrom": ["group_chat_id"]
    }
  }
}
```

| 格式    | 示例                  | 说明                         |
| ------- | --------------------- | ---------------------------- |
| 用户 ID | `"123456789"`         | 精确匹配（推荐）             |
| 用户名  | `"@john"` 或 `"john"` | 按用户名匹配                 |
| 通配符  | `"*"`                 | 允许所有人（不推荐生产使用） |

获取用户 ID：给 `@userinfobot` 发送 `/start`。

### 网络代理

中国大陆用户访问 Telegram API 需要代理：

```bash
export HTTPS_PROXY="http://127.0.0.1:7890"
```

OpenClaw 会自动检测系统代理设置并应用到 Telegram 长轮询连接。

---

## ProviderConfig 字段参考

| 字段         | 类型    | 必填 | 默认值                      | 说明                |
| ------------ | ------- | ---- | --------------------------- | ------------------- |
| `apiKey`     | string  | 是   | —                           | API 密钥            |
| `baseUrl`    | string  | 否   | `https://api.openai.com/v1` | API 端点基础 URL    |
| `apiBaseUrl` | string  | 否   | —                           | `baseUrl` 的别名    |
| `enabled`    | boolean | 否   | `true`                      | 是否启用此 Provider |
| `headers`    | object  | 否   | —                           | 自定义 HTTP 请求头  |
| `auth`       | string  | 否   | `"api-key"`                 | 认证方式            |

---

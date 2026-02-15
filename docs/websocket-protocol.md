# WebSocket 协议

OpenClaw 采用自定义帧协议（与 TypeScript 版本对齐），**非 JSON-RPC**。

---

## 帧格式

| 类型 | 格式 | 说明 |
|------|------|------|
| Request | `{"type":"req", "id":"1", "method":"...", "params":{}}` | 客户端→服务端请求 |
| Response | `{"type":"res", "id":"1", "ok":true, "payload":{}}` | 服务端→客户端响应 |
| Event | `{"type":"event", "event":"...", "payload":{}}` | 服务端→客户端事件推送 |

---

## 握手流程

WebSocket 连接建立后，三步握手：

```
客户端                                    服务端
  │── WebSocket 连接 ───────────────────→│
  │←── connect.challenge {nonce, ts} ────│  (立即)
  │── connect {client, role, auth} ─────→│  (10s 内)
  │←── hello-ok {protocol, features} ───│  (成功)
  │  或 error + close ──────────────────│  (失败)
```

**Step 1 — 服务端质询：**

```json
{"type":"event", "event":"connect.challenge", "payload":{"nonce":"uuid-xxx","ts":1739520000000}}
```

**Step 2 — 客户端连接（必须是第一条请求）：**

```json
{
  "type": "req", "id": "1", "method": "connect",
  "params": {
    "minProtocol": 3, "maxProtocol": 3,
    "client": {"id":"cli", "version":"1.2.3", "platform":"macos", "mode":"operator"},
    "role": "operator",
    "scopes": ["operator.admin"],
    "auth": {"token": "your-gateway-token"},
    "device": {"id":"fingerprint", "publicKey":"...", "signature":"...", "signedAt":1739520000000, "nonce":"uuid-xxx"}
  }
}
```

> Node 角色额外携带 `caps`/`commands`/`permissions` 声明能力；`device.nonce` 签署 Step 1 的 nonce（本地连接可省略）。

**Step 3 — 服务端响应：**

```json
{
  "type": "res", "id": "1", "ok": true,
  "payload": {
    "type": "hello-ok", "protocol": 3,
    "server": {"version":"1.2.3", "host":"hostname", "connId":"uuid"},
    "features": {"methods":["status","config.get","..."], "events":["agent.message","..."]},
    "auth": {"deviceToken":"eyJ...", "role":"operator", "scopes":["operator.admin"]},
    "policy": {"tickIntervalMs":15000}
  }
}
```

> `auth.deviceToken` 仅在首次配对时返回，客户端应持久化供后续连接使用。

---

## 方法示例

```json
// 状态检查
{"type":"req","id":"1","method":"status","params":{}}

// 获取配置
{"type":"req","id":"2","method":"config.get","params":{}}

// 重载配置
{"type":"req","id":"3","method":"config.reload","params":{}}
```

---

## 会话管理

```json
// 创建会话
{"type":"req","id":"4","method":"session.create","params":{"sessionKey":"my-session","cwd":"/tmp"}}

// 列出会话
{"type":"req","id":"5","method":"session.list","params":{}}

// 取消运行
{"type":"req","id":"6","method":"session.cancel","params":{"sessionId":"xxx"}}
```

---

## Agent 对话

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

---

## Cron 调度

```json
// 列出定时任务
{"type":"req","id":"9","method":"cron.list","params":{}}

// 强制执行  
{"type":"req","id":"10","method":"cron.force","params":{"jobId":"xxx"}}
```

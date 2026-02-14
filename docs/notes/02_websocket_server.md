# WebSocket 服务器实现学习笔记

> 第一阶段第二课:WebSocket通信机制深度解析

## 📁 核心文件结构

```
src/gateway/
├── server/
│   ├── ws-connection.ts       # 连接处理器 (267行)
│   ├── ws-connection/
│   │   └── message-handler.ts # 消息处理器
│   ├── ws-types.ts           # WebSocket类型定义
│   └── health-state.ts       # 健康状态管理
├── server-ws-runtime.ts       # WS运行时入口 (50行)
├── server-methods.ts          # 方法分发核心 (217行)
├── server-methods/            # 方法处理器目录 (36个文件)
│   ├── types.ts              # 类型定义 (120行)
│   ├── chat.ts               # 聊天处理 (701行)
│   ├── sessions.ts           # 会话处理
│   ├── nodes.ts              # 节点处理
│   └── ...
└── protocol/
    └── index.ts               # 协议验证 (568行)
```

---

## 🔌 WebSocket连接流程

### 1. 连接建立序列图

```
Client                          Server
   │                              │
   │──── WebSocket Connect ──────>│
   │                              │ 创建connId (UUID)
   │                              │ 设置握手计时器
   │<── connect.challenge ────────│ {nonce, ts}
   │                              │
   │──── connect (请求帧) ────────>│ 验证token/password
   │                              │ 验证nonce签名
   │                              │
   │<── HelloOk ──────────────────│ {methods, events, caps}
   │                              │
   │──── request(method) ─────────>│
   │<── response ─────────────────│
   │                              │
   │<── event (广播) ─────────────│
```

### 2. 连接处理关键代码

```typescript
// ws-connection.ts L61-126
wss.on("connection", (socket, upgradeReq) => {
  const connId = randomUUID();
  const openedAt = Date.now();
  let handshakeState: "pending" | "connected" | "failed" = "pending";
  
  // 发送认证挑战
  const connectNonce = randomUUID();
  send({
    type: "event",
    event: "connect.challenge",
    payload: { nonce: connectNonce, ts: Date.now() },
  });
  
  // 握手超时处理
  const handshakeTimer = setTimeout(() => {
    if (!client) {
      handshakeState = "failed";
      close();
    }
  }, handshakeTimeoutMs);
  
  // 关闭清理
  socket.once("close", (code, reason) => {
    if (client?.presenceKey) {
      upsertPresence(client.presenceKey, { reason: "disconnect" });
      broadcast("presence", { presence: listSystemPresence() });
    }
    // 节点注销
    if (client?.connect?.role === "node") {
      nodeRegistry.unregister(connId);
    }
  });
});
```

---

## ⚙️ 方法分发机制

### 1. Handler模式架构

```
┌─────────────────────────────────────────────────────────────┐
│                    GatewayRequestHandlers                    │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────────────┐   │
│  │ chatHandlers│ │sessionHandlers│ │   nodeHandlers    │   │
│  └──────┬──────┘ └──────┬──────┘ └──────────┬──────────┘   │
│         │               │                    │              │
│  chat.send       sessions.list         node.invoke         │
│  chat.history    sessions.patch        node.list           │
│  chat.abort      sessions.reset        node.describe       │
│  chat.inject     sessions.delete       node.event          │
└─────────────────────────────────────────────────────────────┘
```

### 2. 核心分发代码

```typescript
// server-methods.ts L190-216
export async function handleGatewayRequest(
  opts: GatewayRequestOptions & { extraHandlers?: GatewayRequestHandlers },
): Promise<void> {
  const { req, respond, client, context } = opts;
  
  // 1. 权限检查
  const authError = authorizeGatewayMethod(req.method, client);
  if (authError) {
    respond(false, undefined, authError);
    return;
  }
  
  // 2. 查找handler
  const handler = opts.extraHandlers?.[req.method] 
    ?? coreGatewayHandlers[req.method];
  if (!handler) {
    respond(false, undefined, 
      errorShape(ErrorCodes.INVALID_REQUEST, `unknown method: ${req.method}`));
    return;
  }
  
  // 3. 执行handler
  await handler({
    req,
    params: (req.params ?? {}) as Record<string, unknown>,
    client,
    respond,
    context,
  });
}
```

### 3. Handler模块列表

| 模块 | 方法前缀 | 功能 |
|------|---------|------|
| `chat.ts` | `chat.*` | 消息发送/历史/中止 |
| `sessions.ts` | `sessions.*` | 会话管理 |
| `nodes.ts` | `node.*` | 节点控制 |
| `channels.ts` | `channels.*` | 渠道状态 |
| `config.ts` | `config.*` | 配置管理 |
| `cron.ts` | `cron.*` | 定时任务 |
| `agent.ts` | `agent.*` | Agent调用 |
| `browser.ts` | `browser.*` | 浏览器控制 |
| `skills.ts` | `skills.*` | 技能管理 |
| `wizard.ts` | `wizard.*` | 向导流程 |

---

## 🔐 权限系统

### 1. 角色 (Role)

| 角色 | 说明 | 允许方法 |
|------|------|---------|
| `operator` | 操作员 | 大部分方法 |
| `node` | 设备节点 | `node.invoke.result`, `node.event`, `skills.bins` |

### 2. 权限范围 (Scope)

```typescript
const ADMIN_SCOPE   = "operator.admin";     // 管理员
const READ_SCOPE    = "operator.read";      // 只读
const WRITE_SCOPE   = "operator.write";     // 读写
const APPROVALS_SCOPE = "operator.approvals"; // 审批
const PAIRING_SCOPE = "operator.pairing";   // 配对
```

### 3. 方法权限映射

```typescript
// 只读方法
const READ_METHODS = new Set([
  "health", "logs.tail", "channels.status",
  "sessions.list", "sessions.preview",
  "chat.history", "node.list", ...
]);

// 写入方法
const WRITE_METHODS = new Set([
  "send", "chat.send", "chat.abort",
  "node.invoke", "browser.request", ...
]);

// 管理员方法 (前缀匹配)
const ADMIN_METHOD_PREFIXES = ["exec.approvals."];
```

---

## 💬 Chat处理器详解

### 1. chat.send 流程

```
┌─────────────┐
│  chat.send  │
└──────┬──────┘
       │
       ▼
┌──────────────────┐
│ 参数验证         │ validateChatSendParams
└──────┬───────────┘
       │
       ▼
┌──────────────────┐
│ 检查停止命令     │ isChatStopCommandText
└──────┬───────────┘
       │
       ▼
┌──────────────────┐
│ 幂等性检查       │ dedupe.get / chatAbortControllers
└──────┬───────────┘
       │
       ▼
┌──────────────────┐
│ 创建AbortController│
└──────┬───────────┘
       │
       ▼
┌──────────────────┐
│ 分发消息到Agent  │ dispatchInboundMessage
└──────┬───────────┘
       │
       ▼
┌──────────────────┐
│ 广播聊天事件     │ broadcast("chat", payload)
└──────────────────┘
```

### 2. 关键代码片段

```typescript
// chat.ts L302-443 (简化版)
"chat.send": async ({ params, respond, context, client }) => {
  // 1. 参数验证
  if (!validateChatSendParams(params)) {
    respond(false, undefined, errorShape(...));
    return;
  }
  
  // 2. 停止命令处理
  if (isChatStopCommandText(p.message)) {
    const res = abortChatRunsForSessionKey(...);
    respond(true, { ok: true, aborted: res.aborted });
    return;
  }
  
  // 3. 幂等性缓存检查
  const cached = context.dedupe.get(`chat:${clientRunId}`);
  if (cached) {
    respond(cached.ok, cached.payload, cached.error, { cached: true });
    return;
  }
  
  // 4. 创建中止控制器
  const abortController = new AbortController();
  context.chatAbortControllers.set(clientRunId, {
    controller: abortController,
    sessionKey: p.sessionKey,
    startedAtMs: now,
    expiresAtMs: resolveChatRunExpiresAtMs({ now, timeoutMs }),
  });
  
  // 5. 立即响应确认
  respond(true, { runId: clientRunId, status: "started" });
  
  // 6. 异步分发消息
  void dispatchInboundMessage({
    ctx,
    cfg,
    dispatcher,
    replyOptions: {
      runId: clientRunId,
      abortSignal: abortController.signal,
      images: parsedImages,
    },
  }).then(() => {
    broadcastChatFinal({ context, runId, sessionKey, message });
  }).catch((err) => {
    broadcastChatError({ context, runId, sessionKey, errorMessage: String(err) });
  });
}
```

---

## 📢 广播机制

### 1. 事件类型

| 事件 | 说明 | 触发时机 |
|------|------|---------|
| `chat` | 聊天消息 | 消息发送/完成/错误 |
| `presence` | 在线状态 | 客户端连接/断开 |
| `tick` | 心跳 | 定时器触发 |
| `voicewake.changed` | 语音唤醒变更 | 配置变更 |
| `shutdown` | 关闭通知 | Gateway关闭 |
| `heartbeat` | 心跳事件 | 心跳运行器 |

### 2. 广播函数签名

```typescript
broadcast: (
  event: string,          // 事件名
  payload: unknown,       // 事件数据
  opts?: {
    dropIfSlow?: boolean; // 慢客户端丢弃
    stateVersion?: {      // 状态版本
      presence?: number;
      health?: number;
    };
  },
) => void;
```

---

## ☕ Java实现对照

### 1. WebSocket配置

```java
@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {
    
    @Autowired
    private GatewayWebSocketHandler handler;
    
    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(handler, "/gateway")
                .setAllowedOrigins("*")
                .addInterceptors(new HandshakeInterceptor());
    }
}

// 握手拦截器
public class HandshakeInterceptor implements HandshakeInterceptor {
    @Override
    public boolean beforeHandshake(ServerHttpRequest request, 
                                   ServerHttpResponse response,
                                   WebSocketHandler wsHandler, 
                                   Map<String, Object> attributes) {
        String connId = UUID.randomUUID().toString();
        attributes.put("connId", connId);
        attributes.put("openedAt", System.currentTimeMillis());
        return true;
    }
}
```

### 2. 连接处理器

```java
@Component
@Slf4j
public class GatewayWebSocketHandler extends TextWebSocketHandler {
    
    private final Map<String, WebSocketSession> clients = new ConcurrentHashMap<>();
    private final Map<String, ScheduledFuture<?>> handshakeTimers = new ConcurrentHashMap<>();
    private final MethodDispatcher dispatcher;
    private final ScheduledExecutorService scheduler;
    
    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        String connId = (String) session.getAttributes().get("connId");
        
        // 发送挑战
        String nonce = UUID.randomUUID().toString();
        session.getAttributes().put("nonce", nonce);
        sendEvent(session, "connect.challenge", Map.of(
            "nonce", nonce,
            "ts", System.currentTimeMillis()
        ));
        
        // 设置握手超时
        ScheduledFuture<?> timer = scheduler.schedule(() -> {
            if (!clients.containsKey(connId)) {
                log.warn("Handshake timeout: {}", connId);
                closeSession(session);
            }
        }, 30, TimeUnit.SECONDS);
        handshakeTimers.put(connId, timer);
    }
    
    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        try {
            JsonNode frame = objectMapper.readTree(message.getPayload());
            handleFrame(session, frame);
        } catch (Exception e) {
            log.error("Failed to handle message", e);
        }
    }
    
    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        String connId = (String) session.getAttributes().get("connId");
        clients.remove(connId);
        
        // 清理握手计时器
        ScheduledFuture<?> timer = handshakeTimers.remove(connId);
        if (timer != null) timer.cancel(false);
        
        // 广播presence变更
        broadcastPresence();
        
        log.info("Connection closed: {} status={}", connId, status);
    }
}
```

### 3. 方法分发器

```java
@Component
public class MethodDispatcher {
    
    private final Map<String, MethodHandler> handlers = new HashMap<>();
    private final AuthorizationService authService;
    
    @Autowired
    public MethodDispatcher(List<MethodHandler> handlerList, 
                           AuthorizationService authService) {
        this.authService = authService;
        handlerList.forEach(h -> 
            Arrays.stream(h.getMethods())
                .forEach(m -> handlers.put(m, h))
        );
    }
    
    public void dispatch(RequestFrame request, 
                        GatewayClient client,
                        ResponseCallback respond) {
        // 权限检查
        Optional<ErrorShape> authError = 
            authService.authorize(request.getMethod(), client);
        if (authError.isPresent()) {
            respond.error(authError.get());
            return;
        }
        
        // 查找handler
        MethodHandler handler = handlers.get(request.getMethod());
        if (handler == null) {
            respond.error(ErrorCodes.INVALID_REQUEST, 
                "unknown method: " + request.getMethod());
            return;
        }
        
        // 执行
        handler.handle(request, client, respond);
    }
}
```

### 4. Chat Handler

```java
@Component
public class ChatHandler implements MethodHandler {
    
    private final SessionService sessionService;
    private final MessageDispatcher messageDispatcher;
    private final Map<String, AbortController> abortControllers;
    private final Broadcaster broadcaster;
    
    @Override
    public String[] getMethods() {
        return new String[]{"chat.send", "chat.history", "chat.abort", "chat.inject"};
    }
    
    @Override
    public void handle(RequestFrame request, 
                      GatewayClient client,
                      ResponseCallback respond) {
        switch (request.getMethod()) {
            case "chat.send" -> handleSend(request, client, respond);
            case "chat.history" -> handleHistory(request, respond);
            case "chat.abort" -> handleAbort(request, respond);
            case "chat.inject" -> handleInject(request, respond);
        }
    }
    
    private void handleSend(RequestFrame request, 
                           GatewayClient client,
                           ResponseCallback respond) {
        ChatSendParams params = parseParams(request, ChatSendParams.class);
        
        String runId = params.getIdempotencyKey();
        
        // 创建中止控制器
        AbortController controller = new AbortController();
        abortControllers.put(runId, controller);
        
        // 立即响应
        respond.ok(new ChatSendResponse(runId, "started"));
        
        // 异步处理
        CompletableFuture.runAsync(() -> {
            try {
                messageDispatcher.dispatch(params, controller.getSignal());
                broadcaster.broadcast("chat", new ChatFinalEvent(runId, sessionKey));
            } catch (Exception e) {
                broadcaster.broadcast("chat", new ChatErrorEvent(runId, e.getMessage()));
            } finally {
                abortControllers.remove(runId);
            }
        });
    }
}
```

### 5. 权限服务

```java
@Service
public class AuthorizationService {
    
    private static final Set<String> READ_METHODS = Set.of(
        "health", "sessions.list", "chat.history", "node.list"
    );
    
    private static final Set<String> WRITE_METHODS = Set.of(
        "chat.send", "chat.abort", "node.invoke"
    );
    
    private static final Set<String> NODE_ROLE_METHODS = Set.of(
        "node.invoke.result", "node.event", "skills.bins"
    );
    
    public Optional<ErrorShape> authorize(String method, GatewayClient client) {
        if (client == null || client.getConnect() == null) {
            return Optional.empty();
        }
        
        String role = client.getConnect().getRole();
        Set<String> scopes = client.getConnect().getScopes();
        
        // 节点专属方法
        if (NODE_ROLE_METHODS.contains(method)) {
            if (!"node".equals(role)) {
                return Optional.of(new ErrorShape(
                    ErrorCodes.INVALID_REQUEST, 
                    "unauthorized role: " + role));
            }
            return Optional.empty();
        }
        
        // 管理员可以执行所有操作
        if (scopes.contains("operator.admin")) {
            return Optional.empty();
        }
        
        // 权限范围检查
        if (READ_METHODS.contains(method) && 
            !scopes.contains("operator.read") && 
            !scopes.contains("operator.write")) {
            return Optional.of(new ErrorShape(
                ErrorCodes.INVALID_REQUEST, 
                "missing scope: operator.read"));
        }
        
        if (WRITE_METHODS.contains(method) && 
            !scopes.contains("operator.write")) {
            return Optional.of(new ErrorShape(
                ErrorCodes.INVALID_REQUEST, 
                "missing scope: operator.write"));
        }
        
        return Optional.empty();
    }
}
```

---

## ✅ 学习检查点

完成这一节后,你应该能够:

- [x] 理解WebSocket连接建立流程
- [x] 理解握手挑战认证机制
- [x] 理解Handler模式的方法分发
- [x] 理解权限系统设计
- [x] 理解Chat消息处理流程
- [x] 理解广播机制
- [ ] 能够用Java实现基础WebSocket服务器

---

## 📚 下一步

完成WebSocket服务器理解后,我们将学习:

1. **会话管理器** (`session-utils.ts`)
2. **配置管理** (`config-reload.ts`)
3. **事件路由** (Agent事件系统)

---

## 🔗 关键源文件链接

- [ws-connection.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server/ws-connection.ts) - 连接处理器
- [server-methods.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-methods.ts) - 方法分发
- [chat.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-methods/chat.ts) - 聊天处理
- [types.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-methods/types.ts) - 类型定义

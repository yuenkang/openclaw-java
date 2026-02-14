# 事件路由学习笔记

> 第一阶段第五课:事件广播与节点订阅机制

## 📁 核心文件结构

```
src/gateway/
├── server-chat.ts              # Agent事件处理器 (414行)
├── server-node-events.ts       # 节点事件处理 (249行)
├── server-node-events-types.ts # 类型定义 (37行)
└── server/
    └── ws-connection.ts        # 广播实现
```

---

## 🌐 事件分发架构

```
┌─────────────────────────────────────────────────────────────┐
│                     Event Distribution                       │
└─────────────────────────────────────────────────────────────┘

                  ┌─────────────┐
                  │   Gateway   │
                  │   Server    │
                  └──────┬──────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ▼               ▼               ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│  broadcast  │  │nodeSendTo   │  │broadcastTo  │
│  (全局)     │  │Session      │  │ConnIds      │
└──────┬──────┘  └──────┬──────┘  └──────┬──────┘
       │                │                │
       ▼                ▼                ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│ All WebChat │  │ Subscribed  │  │  Specific   │
│  Clients    │  │   Nodes     │  │ Connections │
└─────────────┘  └─────────────┘  └─────────────┘
```

---

## 📡 三种分发方法

### 1. broadcast - 全局广播

```typescript
// server-methods/types.ts L38-44
broadcast: (
  event: string,
  payload: unknown,
  opts?: {
    dropIfSlow?: boolean;
    stateVersion?: { presence?: number; health?: number };
  },
) => void;
```

**用途**: 发送事件到所有连接的WebChat客户端

**示例事件**:
- `chat` - 聊天消息
- `agent` - Agent生命周期事件
- `presence` - 在线状态
- `health` - 健康检查
- `tick` - 心跳

### 2. nodeSendToSession - 会话定向发送

```typescript
// server-methods/types.ts L55
nodeSendToSession: (sessionKey: string, event: string, payload: unknown) => void;
```

**用途**: 发送事件到订阅了特定会话的所有节点

**实现**:
```typescript
// server.impl.ts L362-363
const nodeSendToSession = (sessionKey: string, event: string, payload: unknown) =>
  nodeConnManager.sendToSubscribedNodes(sessionKey, event, payload);
```

### 3. broadcastToConnIds - 连接定向发送

```typescript
// server-methods/types.ts L46-53
broadcastToConnIds: (
  event: string,
  payload: unknown,
  connIds: ReadonlySet<string>,
  opts?: { dropIfSlow?: boolean; stateVersion?: { presence?: number; health?: number } },
) => void;
```

**用途**: 发送事件到指定的连接ID集合 (用于工具事件)

---

## 🎯 事件类型

| 事件名 | 说明 | 分发方式 |
|--------|------|----------|
| `chat` | 聊天消息/状态 | broadcast + nodeSendToSession |
| `agent` | Agent生命周期 | broadcast/broadcastToConnIds |
| `presence` | 在线状态 | broadcast |
| `health` | 健康检查 | broadcast |
| `tick` | 心跳 | broadcast |
| `node.pair.requested` | 节点配对请求 | broadcast |
| `node.pair.approved` | 节点配对批准 | broadcast |
| `exec.approval.requested` | 执行审批请求 | broadcast |
| `exec.approval.resolved` | 执行审批结果 | broadcast |
| `talk.mode` | 语音模式 | broadcast |

---

## 💬 Chat事件处理

### ChatRunRegistry - 运行注册表

```typescript
// server-chat.ts L33-39
type ChatRunRegistry = {
  add: (sessionId: string, entry: ChatRunEntry) => void;
  peek: (sessionId: string) => ChatRunEntry | undefined;
  shift: (sessionId: string) => ChatRunEntry | undefined;
  remove: (sessionId: string, clientRunId: string) => ChatRunEntry | undefined;
  clear: () => void;
};
```

### ChatRunState - 运行状态

```typescript
// server-chat.ts L93-99
type ChatRunState = {
  registry: ChatRunRegistry;     // 运行注册
  buffers: Map<string, string>;  // 流式文本缓冲
  deltaSentAt: Map<string, number>; // Delta发送时间戳
  abortedRuns: Map<string, number>; // 中止的运行
  clear: () => void;
};
```

### Delta节流 (150ms)

```typescript
// server-chat.ts L230-254
const emitChatDelta = (sessionKey, clientRunId, seq, text) => {
  chatRunState.buffers.set(clientRunId, text);
  const now = Date.now();
  const last = chatRunState.deltaSentAt.get(clientRunId) ?? 0;
  if (now - last < 150) {  // 150ms节流
    return;
  }
  chatRunState.deltaSentAt.set(clientRunId, now);
  broadcast("chat", payload, { dropIfSlow: true });
  nodeSendToSession(sessionKey, "chat", payload);
};
```

---

## 🔌 节点事件处理

### 支持的节点事件

| 事件 | 说明 |
|------|------|
| `voice.transcript` | 语音转文字 |
| `agent.request` | Agent请求 |
| `chat.subscribe` | 订阅会话 |
| `chat.unsubscribe` | 取消订阅 |
| `exec.started` | 执行开始 |
| `exec.finished` | 执行完成 |
| `exec.denied` | 执行拒绝 |

### 订阅机制

```typescript
// server-node-events.ts L155-172
case "chat.subscribe": {
  const sessionKey = obj.sessionKey.trim();
  ctx.nodeSubscribe(nodeId, sessionKey);  // 订阅
  return;
}

case "chat.unsubscribe": {
  const sessionKey = obj.sessionKey.trim();
  ctx.nodeUnsubscribe(nodeId, sessionKey);  // 取消订阅
  return;
}
```

---

## 🛠 工具事件接收者注册

```typescript
// server-chat.ts L123-127
type ToolEventRecipientRegistry = {
  add: (runId: string, connId: string) => void;
  get: (runId: string) => ReadonlySet<string> | undefined;
  markFinal: (runId: string) => void;
};

// TTL配置
const TOOL_EVENT_RECIPIENT_TTL_MS = 10 * 60 * 1000;  // 10分钟
const TOOL_EVENT_RECIPIENT_FINAL_GRACE_MS = 30 * 1000;  // 30秒宽限
```

---

## ☕ Java实现对照

### 1. 事件类型定义

```java
public enum GatewayEventType {
    CHAT("chat"),
    AGENT("agent"),
    PRESENCE("presence"),
    HEALTH("health"),
    TICK("tick"),
    NODE_PAIR_REQUESTED("node.pair.requested"),
    NODE_PAIR_APPROVED("node.pair.approved"),
    EXEC_APPROVAL_REQUESTED("exec.approval.requested"),
    EXEC_APPROVAL_RESOLVED("exec.approval.resolved");
    
    private final String eventName;
}
```

### 2. 事件广播服务

```java
@Service
@Slf4j
public class EventBroadcastService {
    
    private final WebSocketSessionManager sessionManager;
    private final NodeSubscriptionManager nodeSubscriptions;
    
    /**
     * 全局广播到所有WebChat客户端
     */
    public void broadcast(String event, Object payload) {
        broadcast(event, payload, false);
    }
    
    public void broadcast(String event, Object payload, boolean dropIfSlow) {
        EventFrame frame = new EventFrame(event, payload);
        String json = objectMapper.writeValueAsString(frame);
        
        for (WebSocketSession session : sessionManager.getAllSessions()) {
            try {
                if (dropIfSlow && !session.isWritable()) {
                    continue;  // 跳过慢连接
                }
                session.sendMessage(new TextMessage(json));
            } catch (IOException e) {
                log.warn("Failed to broadcast to session {}", session.getId(), e);
            }
        }
    }
    
    /**
     * 发送到订阅了特定会话的节点
     */
    public void nodeSendToSession(String sessionKey, String event, Object payload) {
        Set<String> subscribedNodes = nodeSubscriptions.getSubscribers(sessionKey);
        if (subscribedNodes == null || subscribedNodes.isEmpty()) {
            return;
        }
        
        EventFrame frame = new EventFrame(event, payload);
        String json = objectMapper.writeValueAsString(frame);
        
        for (String nodeId : subscribedNodes) {
            WebSocketSession nodeSession = sessionManager.getNodeSession(nodeId);
            if (nodeSession != null && nodeSession.isOpen()) {
                try {
                    nodeSession.sendMessage(new TextMessage(json));
                } catch (IOException e) {
                    log.warn("Failed to send to node {}", nodeId, e);
                }
            }
        }
    }
    
    /**
     * 发送到指定连接ID集合
     */
    public void broadcastToConnIds(String event, Object payload, Set<String> connIds) {
        EventFrame frame = new EventFrame(event, payload);
        String json = objectMapper.writeValueAsString(frame);
        
        for (String connId : connIds) {
            WebSocketSession session = sessionManager.getSession(connId);
            if (session != null && session.isOpen()) {
                try {
                    session.sendMessage(new TextMessage(json));
                } catch (IOException e) {
                    log.warn("Failed to send to connection {}", connId, e);
                }
            }
        }
    }
}
```

### 3. 节点订阅管理

```java
@Component
public class NodeSubscriptionManager {
    
    // sessionKey -> Set<nodeId>
    private final ConcurrentHashMap<String, Set<String>> subscriptions = new ConcurrentHashMap<>();
    
    // nodeId -> Set<sessionKey>
    private final ConcurrentHashMap<String, Set<String>> nodeToSessions = new ConcurrentHashMap<>();
    
    public void subscribe(String nodeId, String sessionKey) {
        subscriptions.computeIfAbsent(sessionKey, k -> ConcurrentHashMap.newKeySet())
            .add(nodeId);
        nodeToSessions.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet())
            .add(sessionKey);
    }
    
    public void unsubscribe(String nodeId, String sessionKey) {
        Set<String> nodes = subscriptions.get(sessionKey);
        if (nodes != null) {
            nodes.remove(nodeId);
            if (nodes.isEmpty()) {
                subscriptions.remove(sessionKey);
            }
        }
        Set<String> sessions = nodeToSessions.get(nodeId);
        if (sessions != null) {
            sessions.remove(sessionKey);
        }
    }
    
    public void unsubscribeAll(String nodeId) {
        Set<String> sessions = nodeToSessions.remove(nodeId);
        if (sessions != null) {
            for (String sessionKey : sessions) {
                Set<String> nodes = subscriptions.get(sessionKey);
                if (nodes != null) {
                    nodes.remove(nodeId);
                }
            }
        }
    }
    
    public Set<String> getSubscribers(String sessionKey) {
        return subscriptions.get(sessionKey);
    }
}
```

### 4. Chat事件处理器

```java
@Component
@Slf4j
public class ChatEventHandler {
    
    private final EventBroadcastService broadcastService;
    private final ConcurrentHashMap<String, List<ChatRunEntry>> chatRunSessions = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, String> buffers = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Long> deltaSentAt = new ConcurrentHashMap<>();
    
    private static final long DELTA_THROTTLE_MS = 150;
    
    public void emitChatDelta(String sessionKey, String clientRunId, int seq, String text) {
        buffers.put(clientRunId, text);
        
        long now = System.currentTimeMillis();
        Long last = deltaSentAt.get(clientRunId);
        if (last != null && now - last < DELTA_THROTTLE_MS) {
            return;  // 节流
        }
        
        deltaSentAt.put(clientRunId, now);
        
        ChatEventPayload payload = ChatEventPayload.builder()
            .runId(clientRunId)
            .sessionKey(sessionKey)
            .seq(seq)
            .state("delta")
            .message(ChatMessage.builder()
                .role("assistant")
                .content(List.of(new TextContent("text", text)))
                .timestamp(now)
                .build())
            .build();
        
        broadcastService.broadcast("chat", payload, true);
        broadcastService.nodeSendToSession(sessionKey, "chat", payload);
    }
    
    public void emitChatFinal(String sessionKey, String clientRunId, int seq, 
                               String jobState, String error) {
        String text = buffers.remove(clientRunId);
        deltaSentAt.remove(clientRunId);
        
        ChatEventPayload payload = ChatEventPayload.builder()
            .runId(clientRunId)
            .sessionKey(sessionKey)
            .seq(seq)
            .state(jobState)
            .message(text != null ? ChatMessage.builder()
                .role("assistant")
                .content(List.of(new TextContent("text", text.trim())))
                .timestamp(System.currentTimeMillis())
                .build() : null)
            .errorMessage(error)
            .build();
        
        broadcastService.broadcast("chat", payload);
        broadcastService.nodeSendToSession(sessionKey, "chat", payload);
    }
}
```

---

## ✅ 学习检查点

- [x] 理解三种事件分发方式
- [x] 理解Chat事件流式处理(Delta节流)
- [x] 理解节点订阅机制
- [x] 理解工具事件接收者管理
- [x] 能够用Java实现事件广播服务

---

## 🎉 Gateway阶段完成!

已完成Gateway所有五个子模块的学习:
1. ✅ 整体架构
2. ✅ WebSocket服务器
3. ✅ 会话管理器
4. ✅ 配置管理
5. ✅ 事件路由

---

## 📚 下一阶段

进入 **Agent Runtime (代理运行时)** 学习:
1. 消息处理流程
2. 上下文构建
3. 工具调用机制
4. 模型交互

---

## 🔗 关键源文件链接

- [server-chat.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-chat.ts) - Agent事件处理
- [server-node-events.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-node-events.ts) - 节点事件
- [types.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-methods/types.ts) - 核心类型

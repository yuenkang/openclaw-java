# Gateway 架构学习笔记

> 第一阶段第一课:Gateway控制平面深度解析

## 📁 核心文件结构

```
src/gateway/
├── server.impl.ts          # Gateway主入口 (639行)
├── client.ts               # Gateway客户端 (442行)
├── protocol/               # 协议定义
│   ├── index.ts           # 协议验证器 (568行)
│   └── schema/            # JSON Schema定义
├── server-*.ts            # 服务器相关模块
│   ├── server-channels.ts # 渠道管理
│   ├── server-chat.ts     # 聊天事件处理
│   ├── server-cron.ts     # 定时任务
│   ├── server-http.ts     # HTTP服务器
│   └── ...
├── session-utils.ts        # 会话工具
└── config-reload.ts        # 配置热重载
```

---

## 🔑 核心概念

### 1. Gateway是什么?

Gateway是OpenClaw的**控制平面(Control Plane)**,职责包括:
- 提供WebSocket服务器供客户端连接
- 管理多个消息渠道(Telegram/Discord/Slack等)
- 处理会话生命周期
- 路由消息到Agent
- 提供HTTP API (OpenAI兼容)
- 管理节点(macOS/iOS/Android设备)

### 2. 关键组件

```
┌─────────────────────────────────────────────────────────────┐
│                       Gateway Server                        │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │  WebSocket  │  │   HTTP/S    │  │   Canvas Host       │ │
│  │   Server    │  │   Server    │  │   (A2UI渲染)        │ │
│  └──────┬──────┘  └──────┬──────┘  └──────────┬──────────┘ │
│         │                │                     │            │
│  ┌──────┴────────────────┴─────────────────────┴──────────┐│
│  │                   Router (方法分发)                     ││
│  └─────────────────────────┬──────────────────────────────┘│
│                            │                                │
│  ┌─────────┐ ┌─────────┐ ┌┴──────────┐ ┌─────────────────┐ │
│  │ Session │ │ Channel │ │   Node    │ │     Config      │ │
│  │ Manager │ │ Manager │ │ Registry  │ │     Loader      │ │
│  └─────────┘ └─────────┘ └───────────┘ └─────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

---

## 📖 代码结构解读

### 1. Gateway启动流程 (`server.impl.ts`)

```typescript
// 关键函数签名
export async function startGatewayServer(
  port = 18789,
  opts: GatewayServerOptions = {},
): Promise<GatewayServer>
```

**启动流程概览:**

1. **配置加载与验证** (L170-220)
   ```typescript
   let configSnapshot = await readConfigFileSnapshot();
   // 处理legacy配置迁移
   // 验证配置有效性
   // 自动启用插件
   ```

2. **初始化核心服务** (L220-245)
   ```typescript
   const cfgAtStart = loadConfig();
   initSubagentRegistry();
   const defaultAgentId = resolveDefaultAgentId(cfgAtStart);
   const { pluginRegistry, gatewayMethods } = loadGatewayPlugins({...});
   ```

3. **创建运行时状态** (L331-353)
   ```typescript
   const {
     httpServer,    // HTTP服务器
     wss,           // WebSocket服务器
     clients,       // 客户端连接Map
     broadcast,     // 广播函数
     nodeRegistry,  // 节点注册表
     cron,          // 定时任务服务
   } = await createGatewayRuntimeState({...});
   ```

4. **启动渠道管理** (L382-388)
   ```typescript
   const channelManager = createChannelManager({
     loadConfig,
     channelLogs,
     channelRuntimeEnvs,
   });
   ```

5. **WebSocket事件处理** (L472-529)
   ```typescript
   attachGatewayWsHandlers({
     wss,
     clients,
     gatewayMethods,
     context: { deps, cron, nodeRegistry, ... },
   });
   ```

### 2. Gateway客户端 (`client.ts`)

**GatewayClient类结构:**

```typescript
class GatewayClient {
  private ws: WebSocket | null = null;
  private pending = new Map<string, Pending>();  // 待处理请求
  private lastSeq: number | null = null;         // 消息序列号
  
  constructor(opts: GatewayClientOptions) { ... }
  
  start(): void { ... }           // 启动连接
  stop(): void { ... }            // 停止连接
  sendConnect(): void { ... }     // 发送Connect握手
  handleMessage(raw: string) { }  // 处理接收消息
  request<T>(method, params): Promise<T> { }  // 发送RPC请求
}
```

**连接流程:**

```
Client                    Server
  │                          │
  │─── WebSocket Connect ───>│
  │                          │
  │<── HelloOk Response ────│
  │                          │
  │─── Request(method) ────>│
  │                          │
  │<── Response/Event ──────│
```

### 3. 协议定义 (`protocol/index.ts`)

**消息帧类型:**

| 帧类型 | 用途 | 方向 |
|--------|------|------|
| `RequestFrame` | RPC请求 | Client → Server |
| `ResponseFrame` | RPC响应 | Server → Client |
| `EventFrame` | 服务器推送事件 | Server → Client |

**核心协议Schema:**

```typescript
// 连接参数
interface ConnectParams {
  clientName: string;
  clientVersion?: string;
  platform?: string;
  token?: string;
}

// 请求帧
interface RequestFrame {
  id: string;      // 请求ID (用于匹配响应)
  method: string;  // 方法名
  params?: object; // 参数
}

// 响应帧
interface ResponseFrame {
  id: string;      // 对应请求ID
  result?: object; // 成功结果
  error?: ErrorShape; // 错误信息
}

// 事件帧
interface EventFrame {
  event: string;   // 事件名
  data?: object;   // 事件数据
}
```

**核心方法列表 (部分):**

| 方法 | 功能 |
|------|------|
| `sessions.list` | 列出所有会话 |
| `sessions.patch` | 更新会话配置 |
| `sessions.reset` | 重置会话 |
| `chat.send` | 发送聊天消息 |
| `chat.history` | 获取聊天历史 |
| `config.get` | 获取配置 |
| `config.patch` | 更新配置 |
| `channels.status` | 渠道状态 |
| `nodes.list` | 列出节点 |
| `nodes.invoke` | 调用节点方法 |
| `cron.list` | 列出定时任务 |

---

## 🔄 关键流程

### 1. 消息处理流程

```
用户发送消息 (WhatsApp/Telegram/Slack)
         │
         ▼
┌─────────────────┐
│ Channel Adapter │  ← 各渠道的适配器
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Message Router  │  ← 消息路由
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Session Manager │  ← 确定/创建会话
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Agent RPC     │  ← 调用AI代理
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Response Router │  ← 响应路由回渠道
└─────────────────┘
```

### 2. 配置热重载流程

```typescript
// config-reload.ts
const configReloader = startGatewayConfigReloader({
  initialConfig: cfgAtStart,
  readSnapshot: readConfigFileSnapshot,
  onHotReload: applyHotReload,    // 热重载回调
  onRestart: requestGatewayRestart, // 需要重启的配置
  watchPath: CONFIG_PATH,
});
```

支持热重载的配置:
- Hooks配置
- Heartbeat配置
- Cron配置
- 渠道开关

需要重启的配置:
- 端口变更
- TLS配置
- 认证配置

---

## ☕ Java实现对照

### 1. 核心类结构

| TypeScript | Java等价 |
|------------|----------|
| `GatewayServer` | `GatewayServer` (Spring Bean) |
| `GatewayClient` | `GatewayClient` (WebSocket Client) |
| `NodeRegistry` | `NodeRegistry` (Registry Pattern) |
| `ExecApprovalManager` | `ExecApprovalService` |

### 2. Gateway Server Java实现

```java
@Component
@Slf4j
public class GatewayServer {
    
    @Value("${gateway.port:18789}")
    private int port;
    
    private final SessionManager sessionManager;
    private final ChannelManager channelManager;
    private final NodeRegistry nodeRegistry;
    private final ConfigLoader configLoader;
    
    @Autowired
    public GatewayServer(
        SessionManager sessionManager,
        ChannelManager channelManager,
        NodeRegistry nodeRegistry,
        ConfigLoader configLoader
    ) {
        this.sessionManager = sessionManager;
        this.channelManager = channelManager;
        this.nodeRegistry = nodeRegistry;
        this.configLoader = configLoader;
    }
    
    @PostConstruct
    public void start() {
        log.info("Starting Gateway on port {}", port);
        // 初始化各组件
        initializeWebSocket();
        initializeChannels();
        initializeHealthCheck();
    }
    
    @PreDestroy
    public void shutdown() {
        log.info("Shutting down Gateway");
        // 优雅关闭
    }
}
```

### 3. WebSocket Handler

```java
@Component
public class GatewayWebSocketHandler extends TextWebSocketHandler {
    
    private final Map<String, WebSocketSession> clients = new ConcurrentHashMap<>();
    private final GatewayMethodDispatcher dispatcher;
    
    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        clients.put(session.getId(), session);
        log.info("Client connected: {}", session.getId());
    }
    
    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        try {
            RequestFrame request = parseRequest(message.getPayload());
            Object result = dispatcher.dispatch(request.getMethod(), request.getParams());
            sendResponse(session, request.getId(), result);
        } catch (Exception e) {
            sendError(session, request.getId(), e);
        }
    }
    
    public void broadcast(String event, Object data) {
        EventFrame frame = new EventFrame(event, data);
        String json = toJson(frame);
        clients.values().forEach(session -> {
            try {
                session.sendMessage(new TextMessage(json));
            } catch (IOException e) {
                log.error("Broadcast failed", e);
            }
        });
    }
}
```

### 4. 协议定义

```java
// 请求帧
@Data
public class RequestFrame {
    private String id;
    private String method;
    private JsonNode params;
}

// 响应帧
@Data
public class ResponseFrame {
    private String id;
    private JsonNode result;
    private ErrorShape error;
}

// 事件帧
@Data
public class EventFrame {
    private String event;
    private JsonNode data;
}

// 错误信息
@Data
public class ErrorShape {
    private int code;
    private String message;
    private JsonNode data;
}
```

### 5. 方法分发器

```java
@Component
public class GatewayMethodDispatcher {
    
    private final Map<String, GatewayMethodHandler> handlers = new HashMap<>();
    
    @Autowired
    public GatewayMethodDispatcher(List<GatewayMethodHandler> handlerList) {
        handlerList.forEach(h -> handlers.put(h.getMethod(), h));
    }
    
    public Object dispatch(String method, JsonNode params) {
        GatewayMethodHandler handler = handlers.get(method);
        if (handler == null) {
            throw new MethodNotFoundException(method);
        }
        return handler.handle(params);
    }
}

// 方法处理器接口
public interface GatewayMethodHandler {
    String getMethod();
    Object handle(JsonNode params);
}

// 具体实现
@Component
public class SessionsListHandler implements GatewayMethodHandler {
    
    @Override
    public String getMethod() {
        return "sessions.list";
    }
    
    @Override
    public Object handle(JsonNode params) {
        // 实现逻辑
    }
}
```

---

## ✅ 学习检查点

完成这一节后,你应该能够:

- [ ] 理解Gateway在整体架构中的位置
- [ ] 了解WebSocket服务器的工作原理
- [ ] 理解请求/响应/事件帧的结构
- [ ] 知道Gateway的主要职责
- [ ] 能够描述消息处理流程
- [ ] 理解Java实现的基本框架

---

## 📚 下一步

完成Gateway基础理解后,我们将深入:

1. **会话管理** (`session-utils.ts`)
2. **渠道适配器** (`src/telegram/`, `src/discord/`)
3. **节点注册** (`node-registry.ts`)
4. **配置热重载** (`config-reload.ts`)

---

## 🔗 关键源文件链接

- [server.impl.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server.impl.ts) - Gateway主入口
- [client.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/client.ts) - 客户端实现
- [protocol/index.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/protocol/index.ts) - 协议定义
- [session-utils.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/session-utils.ts) - 会话工具

# 消息路由学习笔记

> 第二阶段:渠道路由实现和目标解析

## 📁 核心文件结构

```
src/routing/
└── resolve-route.ts     # Agent路由解析 (261行)

src/infra/outbound/
├── deliver.ts           # 消息投递 (376行)
├── channel-selection.ts # 渠道选择 (93行)
├── target-resolver.ts   # 目标解析 (498行)
└── message-action-runner.ts # 消息动作执行
```

---

## 🔄 路由解析流程

```
┌──────────────────────────────────────────────────────────┐
│           resolveAgentRoute(input)                        │
├──────────────────────────────────────────────────────────┤
│  Input:                                                   │
│  - channel: "whatsapp"                                    │
│  - accountId: "default"                                   │
│  - peer: { kind: "dm", id: "+15551234" }                  │
│  - guildId / teamId (可选)                                │
├──────────────────────────────────────────────────────────┤
│  匹配优先级:                                              │
│  1. binding.peer       (精确peer匹配)                     │
│  2. binding.peer.parent (线程父peer匹配)                  │
│  3. binding.guild      (Discord guild匹配)               │
│  4. binding.team       (Slack team匹配)                  │
│  5. binding.account    (账户级匹配)                       │
│  6. binding.channel    (渠道级通配)                       │
│  7. default            (默认agent)                        │
├──────────────────────────────────────────────────────────┤
│  Output: ResolvedAgentRoute                              │
│  - agentId: "main"                                        │
│  - sessionKey: "agent:main:whatsapp:+15551234"           │
│  - matchedBy: "binding.peer"                             │
└──────────────────────────────────────────────────────────┘
```

---

## 📤 消息投递流程

```typescript
async function deliverOutboundPayloads(params: {
  cfg: OpenClawConfig;
  channel: "whatsapp" | "telegram" | ...;
  to: string;
  payloads: ReplyPayload[];
  accountId?: string;
  replyToId?: string;
  threadId?: string;
  abortSignal?: AbortSignal;
}): Promise<OutboundDeliveryResult[]>
```

### 投递步骤

```
1. 创建ChannelHandler (createChannelHandler)
   ↓
2. 规范化Payloads (normalizeReplyPayloadsForDelivery)
   ↓
3. 遍历每个Payload:
   ├── 文本 → sendTextChunks (分块发送)
   ├── 媒体 → sendMedia
   └── 投票 → sendPoll
   ↓
4. 检查abortSignal (支持取消)
   ↓
5. 返回投递结果列表
```

### ChannelHandler

```typescript
type ChannelHandler = {
  chunker: (text: string, limit: number) => string[] | null;
  chunkerMode?: "text" | "markdown";
  textChunkLimit?: number;
  sendPayload?: (payload) => Promise<Result>;
  sendText: (text) => Promise<Result>;
  sendMedia: (caption, mediaUrl) => Promise<Result>;
};
```

---

## 🎯 目标解析

```typescript
async function resolveMessagingTarget(params: {
  cfg: OpenClawConfig;
  channel: ChannelId;
  input: string;        // 用户输入如 "@john" 或 "+15551234"
  accountId?: string;
  preferredKind?: "user" | "group";
  resolveAmbiguous?: "first" | "highest-rank" | "none";
}): Promise<ResolveMessagingTargetResult>
```

### 解析策略

```
1. looksLikeTargetId() 检测
   ├── WhatsApp: E.164格式 / @s.whatsapp.net
   ├── Telegram: 纯数字
   └── Discord/Slack: 特定ID格式
   
2. 如果像ID → 直接规范化返回

3. 否则查目录 (directory):
   ├── 先查缓存 (30分钟TTL)
   └── 缓存未命中 → 实时查询
   
4. 模糊匹配策略:
   ├── "first" → 第一个匹配
   ├── "highest-rank" → 最高排名
   └── "none" → 返回多个候选
```

---

## 📋 渠道选择

```typescript
async function resolveMessageChannelSelection(params: {
  cfg: OpenClawConfig;
  channel?: string;
}): Promise<{ channel: MessageChannelId; configured: MessageChannelId[] }>
```

### 选择逻辑

```
指定channel:
  → 验证是已知渠道 → 返回

未指定channel:
  → 枚举所有已配置渠道
  → 单个 → 自动选择
  → 多个 → 报错要求指定
  → 无   → 报错无可用渠道
```

---

## ☕ Java实现对照

### 1. 路由解析

```java
@Service
public class AgentRouteResolver {
    
    private final BindingService bindingService;
    
    public ResolvedAgentRoute resolve(ResolveAgentRouteInput input) {
        String channel = input.getChannel().toLowerCase();
        String accountId = normalizeAccountId(input.getAccountId());
        RoutePeer peer = input.getPeer();
        
        List<Binding> bindings = bindingService.listBindings(input.getCfg())
            .stream()
            .filter(b -> matchesChannel(b, channel))
            .filter(b -> matchesAccountId(b, accountId))
            .collect(Collectors.toList());
        
        // 1. Peer匹配
        if (peer != null) {
            Optional<Binding> peerMatch = bindings.stream()
                .filter(b -> matchesPeer(b, peer))
                .findFirst();
            if (peerMatch.isPresent()) {
                return buildRoute(peerMatch.get(), "binding.peer", input);
            }
        }
        
        // 2. Guild匹配 (Discord)
        // 3. Team匹配 (Slack)
        // 4. Account匹配
        // 5. Channel通配
        // 6. 默认
        
        return buildDefaultRoute(input);
    }
    
    private ResolvedAgentRoute buildRoute(Binding binding, String matchedBy, Input input) {
        String agentId = pickFirstExistingAgentId(input.getCfg(), binding.getAgentId());
        String sessionKey = buildSessionKey(agentId, input);
        return ResolvedAgentRoute.builder()
            .agentId(agentId)
            .channel(input.getChannel())
            .accountId(input.getAccountId())
            .sessionKey(sessionKey)
            .matchedBy(matchedBy)
            .build();
    }
}
```

### 2. 消息投递服务

```java
@Service
@Slf4j
public class OutboundDeliveryService {
    
    private final Map<String, ChannelOutboundAdapter> adapters;
    
    public List<DeliveryResult> deliver(DeliverParams params) {
        ChannelOutboundAdapter adapter = adapters.get(params.getChannel());
        if (adapter == null) {
            throw new UnsupportedChannelException(params.getChannel());
        }
        
        List<ReplyPayload> payloads = normalizePayloads(params.getPayloads());
        List<DeliveryResult> results = new ArrayList<>();
        
        for (ReplyPayload payload : payloads) {
            checkAborted(params.getAbortSignal());
            
            if (payload.hasMedia()) {
                results.add(adapter.sendMedia(OutboundContext.builder()
                    .to(params.getTo())
                    .text(payload.getText())
                    .mediaUrl(payload.getMediaUrl())
                    .build()).join());
            } else if (payload.hasText()) {
                for (String chunk : chunkText(payload.getText(), adapter)) {
                    results.add(adapter.sendText(OutboundContext.builder()
                        .to(params.getTo())
                        .text(chunk)
                        .build()).join());
                }
            }
        }
        
        return results;
    }
    
    private List<String> chunkText(String text, ChannelOutboundAdapter adapter) {
        int limit = adapter.getTextChunkLimit();
        return adapter.chunk(text, limit);
    }
}
```

### 3. 目标解析服务

```java
@Service
public class TargetResolver {
    
    // 30分钟缓存
    private final Cache<String, List<DirectoryEntry>> cache = Caffeine.newBuilder()
        .expireAfterWrite(Duration.ofMinutes(30))
        .build();
    
    public ResolveResult resolve(ResolveParams params) {
        String input = params.getInput().trim();
        String channel = params.getChannel();
        
        // 1. 检查是否像ID
        if (looksLikeTargetId(channel, input)) {
            String normalized = normalizeTarget(channel, input);
            return ResolveResult.ok(ResolvedTarget.builder()
                .to(normalized)
                .kind(detectKind(channel, input))
                .source("normalized")
                .build());
        }
        
        // 2. 查目录
        List<DirectoryEntry> entries = getDirectoryEntries(params);
        List<DirectoryEntry> matches = entries.stream()
            .filter(e -> matchesEntry(e, input))
            .collect(Collectors.toList());
        
        if (matches.isEmpty()) {
            return ResolveResult.error(unknownTargetError(input));
        }
        
        if (matches.size() == 1) {
            return ResolveResult.ok(toResolvedTarget(matches.get(0)));
        }
        
        // 3. 模糊匹配
        return pickAmbiguousMatch(matches, params.getResolveAmbiguous());
    }
    
    private boolean looksLikeTargetId(String channel, String input) {
        return switch (channel) {
            case "whatsapp" -> input.matches("^\\+?\\d{10,15}$");
            case "telegram" -> input.matches("^\\d+$");
            default -> false;
        };
    }
}
```

---

## ✅ 学习检查点

- [x] 理解路由匹配优先级
- [x] 理解SessionKey构建规则
- [x] 理解消息投递流程和分块
- [x] 理解目标解析(ID检测/目录查询)
- [x] 能够用Java实现路由和投递服务

---

## 🔗 关键源文件链接

- [resolve-route.ts](https://github.com/openclaw/openclaw/blob/main/src/routing/resolve-route.ts) - Agent路由
- [deliver.ts](https://github.com/openclaw/openclaw/blob/main/src/infra/outbound/deliver.ts) - 消息投递
- [target-resolver.ts](https://github.com/openclaw/openclaw/blob/main/src/infra/outbound/target-resolver.ts) - 目标解析

# 渠道适配器模式学习笔记

> 第二阶段:渠道适配器架构和实现模式

## 📁 核心文件结构

```
src/channels/plugins/
├── types.adapters.ts     # 适配器类型定义 (313行)
├── outbound/             # 出站适配器实现
│   ├── telegram.ts       # Telegram (106行)
│   ├── whatsapp.ts       # WhatsApp (87行)
│   ├── discord.ts        # Discord
│   ├── slack.ts          # Slack
│   ├── signal.ts         # Signal
│   ├── imessage.ts       # iMessage
│   └── load.ts           # 加载器
└── ...
```

---

## 🔌 12种适配器接口

```
┌───────────────────────────────────────────────────────────┐
│                  ChannelPlugin                             │
│  (完整渠道插件 = 多个适配器组合)                             │
├───────────────────────────────────────────────────────────┤
│ ChannelSetupAdapter     │ 账户设置和配置应用                │
│ ChannelConfigAdapter    │ 账户列表和解析                    │
│ ChannelOutboundAdapter  │ 消息发送 ★核心★                  │
│ ChannelStatusAdapter    │ 状态探测和快照                    │
│ ChannelGatewayAdapter   │ 网关连接管理                      │
│ ChannelAuthAdapter      │ 登录认证                          │
│ ChannelHeartbeatAdapter │ 心跳检查                          │
│ ChannelDirectoryAdapter │ 联系人目录                        │
│ ChannelResolverAdapter  │ 目标解析                          │
│ ChannelGroupAdapter     │ 群组策略                          │
│ ChannelPairingAdapter   │ 配对审批                          │
│ ChannelSecurityAdapter  │ DM安全策略                        │
└───────────────────────────────────────────────────────────┘
```

---

## 📤 ChannelOutboundAdapter (出站适配器)

### 核心接口

```typescript
type ChannelOutboundAdapter = {
  // 投递模式
  deliveryMode: "direct" | "gateway" | "hybrid";
  
  // 文本分块
  chunker?: (text: string, limit: number) => string[];
  chunkerMode?: "text" | "markdown";
  textChunkLimit?: number;
  
  // 投票限制
  pollMaxOptions?: number;
  
  // 目标解析
  resolveTarget?: (params) => { ok: true; to: string } | { ok: false; error: Error };
  
  // 发送方法
  sendText?: (ctx: ChannelOutboundContext) => Promise<OutboundDeliveryResult>;
  sendMedia?: (ctx: ChannelOutboundContext) => Promise<OutboundDeliveryResult>;
  sendPayload?: (ctx: ChannelOutboundPayloadContext) => Promise<OutboundDeliveryResult>;
  sendPoll?: (ctx: ChannelPollContext) => Promise<ChannelPollResult>;
};
```

### 投递模式对比

| 模式 | 说明 | 使用渠道 |
|------|------|----------|
| **direct** | 直接调用渠道API | Telegram |
| **gateway** | 通过Gateway WS发送 | WhatsApp |
| **hybrid** | 支持两种方式 | Discord, Slack |

---

## 📱 Telegram Outbound

```typescript
const telegramOutbound: ChannelOutboundAdapter = {
  deliveryMode: "direct",
  chunker: markdownToTelegramHtmlChunks,  // Markdown → HTML
  chunkerMode: "markdown",
  textChunkLimit: 4000,
  
  sendText: async ({ to, text, accountId, replyToId, threadId }) => {
    return await sendMessageTelegram(to, text, {
      textMode: "html",
      messageThreadId: parseThreadId(threadId),
      replyToMessageId: parseReplyToMessageId(replyToId),
      accountId,
    });
  },
  
  sendPayload: async ({ to, payload, ... }) => {
    // 支持buttons、quoteText
    const telegramData = payload.channelData?.telegram;
    return await send(to, text, {
      buttons: telegramData?.buttons,
      quoteText: telegramData?.quoteText,
    });
  },
};
```

---

## 💬 WhatsApp Outbound

```typescript
const whatsappOutbound: ChannelOutboundAdapter = {
  deliveryMode: "gateway",  // 通过Gateway发送
  chunker: chunkText,       // 纯文本分块
  chunkerMode: "text",
  textChunkLimit: 4000,
  pollMaxOptions: 12,
  
  // 目标解析(allowFrom验证)
  resolveTarget: ({ to, allowFrom, mode }) => {
    const normalized = normalizeWhatsAppTarget(to);
    
    // 群组JID直接通过
    if (isWhatsAppGroupJid(normalized)) {
      return { ok: true, to: normalized };
    }
    
    // 检查allowFrom列表
    if (mode === "implicit" || mode === "heartbeat") {
      if (allowList.includes(normalized)) {
        return { ok: true, to: normalized };
      }
      return { ok: true, to: allowList[0] };  // 默认第一个
    }
    
    return { ok: true, to: normalized };
  },
  
  sendPoll: async ({ to, poll, accountId }) => 
    await sendPollWhatsApp(to, poll, { accountId }),
};
```

---

## ☕ Java实现对照

### 1. 适配器接口定义

```java
public interface ChannelOutboundAdapter {
    
    enum DeliveryMode { DIRECT, GATEWAY, HYBRID }
    
    DeliveryMode getDeliveryMode();
    
    int getTextChunkLimit();
    
    List<String> chunk(String text, int limit);
    
    Optional<String> resolveTarget(ResolveTargetParams params);
    
    CompletableFuture<OutboundResult> sendText(OutboundContext ctx);
    
    CompletableFuture<OutboundResult> sendMedia(OutboundContext ctx);
    
    CompletableFuture<OutboundResult> sendPayload(OutboundPayloadContext ctx);
    
    default CompletableFuture<PollResult> sendPoll(PollContext ctx) {
        throw new UnsupportedOperationException("Poll not supported");
    }
}

@Data
@Builder
public class OutboundContext {
    private String to;
    private String text;
    private String mediaUrl;
    private String replyToId;
    private String threadId;
    private String accountId;
}
```

### 2. Telegram实现

```java
@Component("telegram")
public class TelegramOutboundAdapter implements ChannelOutboundAdapter {
    
    private final TelegramApiClient client;
    
    @Override
    public DeliveryMode getDeliveryMode() {
        return DeliveryMode.DIRECT;
    }
    
    @Override
    public int getTextChunkLimit() {
        return 4000;
    }
    
    @Override
    public List<String> chunk(String text, int limit) {
        return MarkdownToHtmlChunker.chunk(text, limit);
    }
    
    @Override
    public CompletableFuture<OutboundResult> sendText(OutboundContext ctx) {
        return client.sendMessage(SendMessageRequest.builder()
            .chatId(ctx.getTo())
            .text(ctx.getText())
            .parseMode("HTML")
            .replyToMessageId(parseMessageId(ctx.getReplyToId()))
            .messageThreadId(parseThreadId(ctx.getThreadId()))
            .build())
            .thenApply(r -> OutboundResult.builder()
                .channel("telegram")
                .messageId(r.getMessageId())
                .chatId(ctx.getTo())
                .build());
    }
    
    @Override
    public CompletableFuture<OutboundResult> sendPayload(OutboundPayloadContext ctx) {
        Map<String, Object> telegramData = ctx.getPayload().getChannelData("telegram");
        List<List<InlineKeyboardButton>> buttons = extractButtons(telegramData);
        
        return client.sendMessage(SendMessageRequest.builder()
            .chatId(ctx.getTo())
            .text(ctx.getText())
            .parseMode("HTML")
            .replyMarkup(buttons.isEmpty() ? null : new InlineKeyboardMarkup(buttons))
            .build());
    }
}
```

### 3. WhatsApp实现

```java
@Component("whatsapp")
public class WhatsAppOutboundAdapter implements ChannelOutboundAdapter {
    
    private final GatewayClient gateway;
    
    @Override
    public DeliveryMode getDeliveryMode() {
        return DeliveryMode.GATEWAY;
    }
    
    @Override
    public Optional<String> resolveTarget(ResolveTargetParams params) {
        String to = params.getTo();
        List<String> allowList = normalizeAllowList(params.getAllowFrom());
        
        if (to == null || to.isBlank()) {
            return allowList.isEmpty() ? Optional.empty() : Optional.of(allowList.get(0));
        }
        
        String normalized = normalizeWhatsAppTarget(to);
        
        // 群组JID直接通过
        if (isGroupJid(normalized)) {
            return Optional.of(normalized);
        }
        
        // implicit模式下检查allowFrom
        if (params.getMode() == TargetMode.IMPLICIT) {
            if (allowList.contains(normalized)) {
                return Optional.of(normalized);
            }
            return allowList.isEmpty() ? Optional.of(normalized) : Optional.of(allowList.get(0));
        }
        
        return Optional.of(normalized);
    }
    
    @Override
    public CompletableFuture<OutboundResult> sendText(OutboundContext ctx) {
        return gateway.send(GatewayMessage.builder()
            .channel("whatsapp")
            .to(ctx.getTo())
            .text(ctx.getText())
            .accountId(ctx.getAccountId())
            .build());
    }
    
    @Override
    public CompletableFuture<PollResult> sendPoll(PollContext ctx) {
        return gateway.sendPoll(ctx.getTo(), ctx.getPoll(), ctx.getAccountId());
    }
}
```

### 4. 适配器注册表

```java
@Component
public class ChannelAdapterRegistry {
    
    private final Map<String, ChannelOutboundAdapter> adapters;
    
    public ChannelAdapterRegistry(List<ChannelOutboundAdapter> adapterList) {
        this.adapters = adapterList.stream()
            .collect(Collectors.toMap(
                a -> a.getClass().getAnnotation(Component.class).value(),
                Function.identity()
            ));
    }
    
    public ChannelOutboundAdapter getAdapter(String channelId) {
        return adapters.get(channelId);
    }
    
    public CompletableFuture<OutboundResult> send(String channelId, OutboundContext ctx) {
        ChannelOutboundAdapter adapter = getAdapter(channelId);
        if (adapter == null) {
            return CompletableFuture.failedFuture(
                new UnsupportedChannelException(channelId));
        }
        return adapter.sendText(ctx);
    }
}
```

---

## ✅ 学习检查点

- [x] 理解12种适配器接口职责
- [x] 理解出站适配器三种投递模式
- [x] 理解Telegram direct模式实现
- [x] 理解WhatsApp gateway模式和目标解析
- [x] 能够用Java实现适配器接口和注册表

---

## 🔗 关键源文件链接

- [types.adapters.ts](https://github.com/openclaw/openclaw/blob/main/src/channels/plugins/types.adapters.ts) - 适配器类型
- [telegram.ts](https://github.com/openclaw/openclaw/blob/main/src/channels/plugins/outbound/telegram.ts) - Telegram出站
- [whatsapp.ts](https://github.com/openclaw/openclaw/blob/main/src/channels/plugins/outbound/whatsapp.ts) - WhatsApp出站

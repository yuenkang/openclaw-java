# Channels 渠道系统学习笔记

> 第二阶段:多渠道消息路由和插件系统

## 📁 核心文件结构

```
src/channels/
├── registry.ts          # 渠道注册表 (180行)
├── dock.ts              # 渠道Dock配置 (457行)
├── channel-config.ts    # 配置匹配逻辑 (183行)
├── session.ts           # 会话管理
├── plugins/             # 插件系统 (70+文件)
│   ├── types.core.ts    # 核心类型定义 (332行)
│   ├── types.plugin.ts  # 插件类型
│   ├── catalog.ts       # 插件目录 (312行)
│   └── ...
└── ...
```

---

## 🌐 支持的渠道

```typescript
const CHAT_CHANNEL_ORDER = [
  "telegram",   // Telegram Bot API
  "whatsapp",   // WhatsApp Web (QR link)
  "discord",    // Discord Bot API
  "googlechat", // Google Chat API
  "slack",      // Slack Socket Mode
  "signal",     // signal-cli linked device
  "imessage",   // iMessage (imsg)
] as const;

// 别名映射
const CHAT_CHANNEL_ALIASES = {
  imsg: "imessage",
  "google-chat": "googlechat",
  gchat: "googlechat",
};
```

---

## 🔧 ChannelDock 配置

每个渠道都有独立的Dock配置:

```typescript
type ChannelDock = {
  id: ChannelId;
  
  // 渠道能力
  capabilities: {
    chatTypes: ["direct" | "group" | "channel" | "thread"];
    polls?: boolean;         // 投票支持
    reactions?: boolean;     // 反应支持
    media?: boolean;         // 媒体支持
    nativeCommands?: boolean;// 原生命令
    blockStreaming?: boolean;// 块流式传输
    threads?: boolean;       // 线程支持
  };
  
  // 出站配置
  outbound?: {
    textChunkLimit?: number; // 文本分块限制
  };
  
  // 流式传输配置
  streaming?: {
    blockStreamingCoalesceDefaults?: {
      minChars?: number;
      idleMs?: number;
    };
  };
  
  // 命令适配器
  commands?: ChannelCommandAdapter;
  
  // 群组适配器
  groups?: ChannelGroupAdapter;
  
  // 提及适配器
  mentions?: ChannelMentionAdapter;
  
  // 线程适配器
  threading?: ChannelThreadingAdapter;
};
```

### 各渠道特性对比

| 渠道 | chatTypes | 投票 | 反应 | 媒体 | 线程 | 分块限制 |
|------|-----------|------|------|------|------|----------|
| Telegram | direct/group/channel/thread | ❌ | ❌ | ❌ | ❌ | 4000 |
| WhatsApp | direct/group | ✅ | ✅ | ✅ | ❌ | 4000 |
| Discord | direct/channel/thread | ✅ | ✅ | ✅ | ✅ | 2000 |
| Slack | direct/channel/thread | ✅ | ✅ | ✅ | ✅ | 4000 |
| GoogleChat | direct/space | ❌ | ❌ | ❌ | ✅ | 4096 |
| Signal | direct/group | ❌ | ✅ | ✅ | ❌ | 4000 |
| iMessage | direct/group | ❌ | ✅ | ✅ | ❌ | 4000 |

---

## 🔌 插件系统核心类型

### ChannelMeta (渠道元数据)

```typescript
type ChannelMeta = {
  id: ChannelId;
  label: string;           // 显示名称
  selectionLabel: string;  // 选择界面标签
  docsPath: string;        // 文档路径
  blurb: string;           // 简介
  systemImage?: string;    // 系统图标
  aliases?: string[];      // 别名列表
  forceAccountBinding?: boolean;
};
```

### ChannelCapabilities (渠道能力)

```typescript
type ChannelCapabilities = {
  chatTypes: Array<"direct" | "group" | "channel" | "thread">;
  polls?: boolean;
  reactions?: boolean;
  edit?: boolean;
  unsend?: boolean;
  reply?: boolean;
  effects?: boolean;
  groupManagement?: boolean;
  threads?: boolean;
  media?: boolean;
  nativeCommands?: boolean;
  blockStreaming?: boolean;
};
```

### ChannelAccountSnapshot (账户快照)

```typescript
type ChannelAccountSnapshot = {
  accountId: string;
  name?: string;
  enabled?: boolean;
  configured?: boolean;
  linked?: boolean;
  running?: boolean;
  connected?: boolean;
  lastConnectedAt?: number | null;
  lastMessageAt?: number | null;
  lastError?: string | null;
  // ... 更多状态字段
};
```

---

## 🔄 配置匹配逻辑

支持三种匹配源:

```typescript
type ChannelMatchSource = "direct" | "parent" | "wildcard";

// 匹配优先级
// 1. direct  - 直接匹配 (channelId)
// 2. parent  - 父级匹配 (groupId)
// 3. wildcard - 通配符匹配 (*)
```

---

## ☕ Java实现对照

### 1. 渠道注册表

```java
@Component
public class ChannelRegistry {
    
    public static final List<String> CHANNEL_ORDER = List.of(
        "telegram", "whatsapp", "discord", 
        "googlechat", "slack", "signal", "imessage"
    );
    
    private static final Map<String, String> ALIASES = Map.of(
        "imsg", "imessage",
        "google-chat", "googlechat",
        "gchat", "googlechat"
    );
    
    private static final Map<String, ChannelMeta> CHANNEL_META = Map.of(
        "telegram", ChannelMeta.builder()
            .id("telegram")
            .label("Telegram")
            .selectionLabel("Telegram (Bot API)")
            .docsPath("/channels/telegram")
            .blurb("simplest way to get started")
            .build(),
        // ... 其他渠道
    );
    
    public String normalizeChannelId(String raw) {
        if (raw == null) return null;
        String normalized = raw.trim().toLowerCase();
        String resolved = ALIASES.getOrDefault(normalized, normalized);
        return CHANNEL_ORDER.contains(resolved) ? resolved : null;
    }
    
    public ChannelMeta getChannelMeta(String id) {
        return CHANNEL_META.get(id);
    }
    
    public List<ChannelMeta> listChannels() {
        return CHANNEL_ORDER.stream()
            .map(CHANNEL_META::get)
            .collect(Collectors.toList());
    }
}
```

### 2. 渠道Dock

```java
@Data
@Builder
public class ChannelDock {
    private String id;
    private ChannelCapabilities capabilities;
    private OutboundConfig outbound;
    private StreamingConfig streaming;
    private ChannelGroupAdapter groups;
    private ChannelMentionAdapter mentions;
    private ChannelThreadingAdapter threading;
    
    @Data
    @Builder
    public static class ChannelCapabilities {
        private List<String> chatTypes;
        private boolean polls;
        private boolean reactions;
        private boolean media;
        private boolean nativeCommands;
        private boolean blockStreaming;
        private boolean threads;
    }
    
    @Data
    public static class OutboundConfig {
        private int textChunkLimit = 4000;
    }
}

@Component
public class ChannelDockRegistry {
    
    private final Map<String, ChannelDock> docks = new ConcurrentHashMap<>();
    
    @PostConstruct
    public void init() {
        // Telegram
        docks.put("telegram", ChannelDock.builder()
            .id("telegram")
            .capabilities(ChannelCapabilities.builder()
                .chatTypes(List.of("direct", "group", "channel", "thread"))
                .nativeCommands(true)
                .blockStreaming(true)
                .build())
            .outbound(new OutboundConfig().setTextChunkLimit(4000))
            .build());
        
        // Discord
        docks.put("discord", ChannelDock.builder()
            .id("discord")
            .capabilities(ChannelCapabilities.builder()
                .chatTypes(List.of("direct", "channel", "thread"))
                .polls(true).reactions(true).media(true)
                .nativeCommands(true).threads(true)
                .build())
            .outbound(new OutboundConfig().setTextChunkLimit(2000))
            .streaming(StreamingConfig.builder()
                .minChars(1500).idleMs(1000)
                .build())
            .build());
        
        // ... 其他渠道
    }
    
    public ChannelDock getDock(String channelId) {
        return docks.get(channelId);
    }
}
```

### 3. 配置匹配服务

```java
@Service
public class ChannelConfigMatcher {
    
    public enum MatchSource {
        DIRECT, PARENT, WILDCARD
    }
    
    @Data
    @Builder
    public static class ChannelMatch<T> {
        private T entry;
        private String key;
        private T wildcardEntry;
        private String wildcardKey;
        private T parentEntry;
        private String parentKey;
        private String matchKey;
        private MatchSource matchSource;
    }
    
    public <T> ChannelMatch<T> resolveMatch(
        Map<String, T> entries,
        List<String> keys,
        List<String> parentKeys,
        String wildcardKey
    ) {
        ChannelMatch.ChannelMatchBuilder<T> builder = ChannelMatch.builder();
        
        // 1. 直接匹配
        for (String key : keys) {
            if (entries.containsKey(key)) {
                return builder
                    .entry(entries.get(key))
                    .key(key)
                    .matchKey(key)
                    .matchSource(MatchSource.DIRECT)
                    .build();
            }
        }
        
        // 2. 父级匹配
        if (parentKeys != null) {
            for (String key : parentKeys) {
                if (entries.containsKey(key)) {
                    T entry = entries.get(key);
                    return builder
                        .entry(entry)
                        .parentEntry(entry)
                        .parentKey(key)
                        .matchKey(key)
                        .matchSource(MatchSource.PARENT)
                        .build();
                }
            }
        }
        
        // 3. 通配符匹配
        if (wildcardKey != null && entries.containsKey(wildcardKey)) {
            T entry = entries.get(wildcardKey);
            return builder
                .entry(entry)
                .wildcardEntry(entry)
                .wildcardKey(wildcardKey)
                .matchKey(wildcardKey)
                .matchSource(MatchSource.WILDCARD)
                .build();
        }
        
        return builder.build();
    }
}
```

---

## ✅ 学习检查点

- [x] 理解7种核心渠道及别名
- [x] 理解ChannelDock配置结构
- [x] 理解渠道能力(capabilities)差异
- [x] 理解配置匹配优先级(direct/parent/wildcard)
- [x] 理解插件目录系统(catalog)
- [x] 能够用Java实现渠道注册表和Dock

---

## 🔗 关键源文件链接

- [registry.ts](https://github.com/openclaw/openclaw/blob/main/src/channels/registry.ts) - 渠道注册
- [dock.ts](https://github.com/openclaw/openclaw/blob/main/src/channels/dock.ts) - Dock配置
- [types.core.ts](https://github.com/openclaw/openclaw/blob/main/src/channels/plugins/types.core.ts) - 核心类型

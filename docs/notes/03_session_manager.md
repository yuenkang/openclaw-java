# 会话管理器学习笔记

> 第一阶段第三课:Session生命周期与存储机制

## 📁 核心文件结构

```
src/
├── gateway/
│   ├── session-utils.ts          # 会话工具函数 (725行)
│   ├── session-utils.types.ts    # 类型定义 (92行)
│   ├── session-utils.fs.ts       # 文件系统操作
│   ├── sessions-patch.ts         # 会话补丁
│   ├── sessions-resolve.ts       # 会话解析
│   └── server-methods/
│       └── sessions.ts           # Gateway方法 (483行)
├── config/
│   └── sessions/
│       └── types.ts              # SessionEntry类型 (169行)
└── sessions/
    ├── session-key-utils.ts      # 密钥工具
    └── send-policy.ts            # 发送策略
```

---

## 🔑 核心数据结构

### 1. SessionEntry (会话条目)

```typescript
// config/sessions/types.ts L25-96
export type SessionEntry = {
  // 核心标识
  sessionId: string;               // 会话UUID
  updatedAt: number;               // 最后更新时间戳(ms)
  sessionFile?: string;            // 会话文件路径
  
  // 关系
  spawnedBy?: string;              // 父会话Key
  
  // 状态
  systemSent?: boolean;            // 是否已发送系统消息
  abortedLastRun?: boolean;        // 上次运行是否中止
  
  // 会话类型
  chatType?: "direct" | "group" | "channel";
  
  // AI配置
  thinkingLevel?: string;          // 思考深度 (low/medium/high)
  verboseLevel?: string;           // 详细程度
  reasoningLevel?: string;         // 推理深度
  elevatedLevel?: string;          // 提升级别
  providerOverride?: string;       // 模型提供者覆盖
  modelOverride?: string;          // 模型覆盖
  contextTokens?: number;          // 上下文Token数
  
  // Token统计
  inputTokens?: number;
  outputTokens?: number;
  totalTokens?: number;
  
  // 队列配置
  queueMode?: "steer" | "followup" | "collect" | "queue" | "interrupt";
  queueDebounceMs?: number;
  queueCap?: number;
  
  // 发送策略
  sendPolicy?: "allow" | "deny";
  
  // 渠道信息
  channel?: string;                // 来源渠道
  groupId?: string;                // 群组ID
  subject?: string;                // 主题
  displayName?: string;            // 显示名称
  label?: string;                  // 标签
  origin?: SessionOrigin;          // 来源信息
  
  // 最近投递
  lastChannel?: ChannelId;         // 最近渠道
  lastTo?: string;                 // 最近接收者
  lastAccountId?: string;          // 最近账户
};
```

### 2. GatewaySessionRow (Gateway会话行)

```typescript
// gateway/session-utils.types.ts L11-44
export type GatewaySessionRow = {
  key: string;                     // 会话Key
  kind: "direct" | "group" | "global" | "unknown";
  label?: string;
  displayName?: string;
  derivedTitle?: string;           // 推导标题
  lastMessagePreview?: string;     // 最后消息预览
  channel?: string;
  updatedAt: number | null;
  sessionId?: string;
  // ... 其他字段
};
```

### 3. Session Key 格式

| 格式 | 说明 | 示例 |
|------|------|------|
| `main` | 主会话别名 | `main` |
| `global` | 全局会话 | `global` |
| `agent:{agentId}:{key}` | 代理会话 | `agent:default:main` |
| `{channel}:group:{id}` | 群组会话 | `telegram:group:12345` |
| `{channel}:channel:{id}` | 频道会话 | `slack:channel:C01234` |

---

## 💾 存储机制

### 1. 存储路径

```
~/.openclaw/
└── agents/
    └── {agentId}/
        └── sessions/
            ├── store.json     # 会话存储 (Key -> Entry)
            └── {sessionId}.jsonl  # 会话记录
```

### 2. store.json 结构

```json
{
  "agent:default:main": {
    "sessionId": "abc123-...",
    "updatedAt": 1706252400000,
    "thinkingLevel": "medium",
    "inputTokens": 1500,
    "outputTokens": 800
  },
  "telegram:group:12345": {
    "sessionId": "def456-...",
    "channel": "telegram",
    "chatType": "group"
  }
}
```

### 3. {sessionId}.jsonl 结构 (JSONL)

```jsonl
{"type":"session","version":"1.0","id":"abc123","timestamp":"..."}
{"type":"message","id":"msg1","message":{"role":"user","content":[{"type":"text","text":"Hello"}]}}
{"type":"message","id":"msg2","message":{"role":"assistant","content":[{"type":"text","text":"Hi!"}]}}
```

---

## ⚙️ 核心操作流程

### 1. 会话加载 (loadSessionEntry)

```typescript
// session-utils.ts L183-192
export function loadSessionEntry(sessionKey: string) {
  const cfg = loadConfig();
  const canonicalKey = resolveSessionStoreKey({ cfg, sessionKey });
  const agentId = resolveSessionStoreAgentId(cfg, canonicalKey);
  const storePath = resolveStorePath(sessionCfg?.store, { agentId });
  const store = loadSessionStore(storePath);
  const entry = store[canonicalKey];
  return { cfg, storePath, store, entry, canonicalKey };
}
```

### 2. 会话Key规范化

```
输入: "main"
  ↓
parseAgentSessionKey() → null
  ↓
检查是否是main别名
  ↓
resolveMainSessionKey()
  ↓
输出: "agent:default:main"
```

### 3. 多Agent会话合并

```typescript
// session-utils.ts L465-506
export function loadCombinedSessionStoreForGateway(cfg: OpenClawConfig) {
  const storeConfig = cfg.session?.store;
  
  // 如果不是模板路径,直接加载
  if (!isStorePathTemplate(storeConfig)) {
    const store = loadSessionStore(storePath);
    // 合并到combined
    return { storePath, store: combined };
  }
  
  // 遍历所有agent
  const agentIds = listConfiguredAgentIds(cfg);
  const combined: Record<string, SessionEntry> = {};
  
  for (const agentId of agentIds) {
    const storePath = resolveStorePath(storeConfig, { agentId });
    const store = loadSessionStore(storePath);
    // 合并每个entry
    for (const [key, entry] of Object.entries(store)) {
      const canonicalKey = canonicalizeSessionKeyForAgent(agentId, key);
      mergeSessionEntryIntoCombined({ combined, entry, agentId, canonicalKey });
    }
  }
  
  return { storePath: "(multiple)", store: combined };
}
```

---

## 🔧 Gateway方法

### 1. sessions.list

**参数:**
```typescript
{
  includeGlobal?: boolean;       // 包含全局会话
  includeUnknown?: boolean;      // 包含未知会话
  includeDerivedTitles?: boolean;// 包含推导标题
  includeLastMessage?: boolean;  // 包含最后消息
  spawnedBy?: string;            // 按父会话过滤
  label?: string;                // 按标签过滤
  agentId?: string;              // 按Agent过滤
  search?: string;               // 搜索
  activeMinutes?: number;        // 活跃时间窗口
  limit?: number;                // 数量限制
}
```

**返回:**
```typescript
{
  ts: number;
  path: string;
  count: number;
  defaults: GatewaySessionsDefaults;
  sessions: GatewaySessionRow[];
}
```

### 2. sessions.patch

**参数:**
```typescript
{
  key: string;                   // 会话Key
  patch: Partial<SessionEntry>;  // 补丁数据
}
```

### 3. sessions.reset

重置会话,生成新的sessionId。

### 4. sessions.delete

删除会话,归档转录文件。

---

## ☕ Java实现对照

### 1. SessionEntry实体

```java
@Entity
@Table(name = "sessions")
@Data
public class SessionEntry {
    
    @Id
    private String sessionKey;
    
    @Column(nullable = false)
    private String sessionId;
    
    @Column(nullable = false)
    private Long updatedAt;
    
    // 关系
    private String spawnedBy;
    
    // 状态
    private Boolean systemSent;
    private Boolean abortedLastRun;
    
    // 类型
    @Enumerated(EnumType.STRING)
    private ChatType chatType;
    
    // AI配置
    private String thinkingLevel;
    private String verboseLevel;
    private String modelOverride;
    private String providerOverride;
    private Integer contextTokens;
    
    // Token统计
    private Long inputTokens;
    private Long outputTokens;
    private Long totalTokens;
    
    // 渠道信息
    private String channel;
    private String groupId;
    private String displayName;
    private String label;
    
    // 发送策略
    @Enumerated(EnumType.STRING)
    private SendPolicy sendPolicy;
    
    // 队列配置
    @Enumerated(EnumType.STRING)
    private QueueMode queueMode;
    private Integer queueDebounceMs;
    private Integer queueCap;
    
    // 最近投递
    private String lastChannel;
    private String lastTo;
    private String lastAccountId;
}
```

### 2. SessionRepository

```java
public interface SessionRepository extends JpaRepository<SessionEntry, String> {
    
    List<SessionEntry> findBySpawnedBy(String spawnedBy);
    
    List<SessionEntry> findByLabel(String label);
    
    List<SessionEntry> findByUpdatedAtGreaterThan(Long cutoff);
    
    @Query("SELECT s FROM SessionEntry s WHERE " +
           "LOWER(s.displayName) LIKE LOWER(CONCAT('%', :search, '%')) OR " +
           "LOWER(s.label) LIKE LOWER(CONCAT('%', :search, '%')) OR " +
           "LOWER(s.sessionId) LIKE LOWER(CONCAT('%', :search, '%'))")
    List<SessionEntry> search(@Param("search") String search);
    
    @Query("SELECT s FROM SessionEntry s WHERE s.sessionKey LIKE :agentPrefix")
    List<SessionEntry> findByAgentId(@Param("agentPrefix") String agentPrefix);
}
```

### 3. SessionService

```java
@Service
@Slf4j
public class SessionService {
    
    private final SessionRepository sessionRepository;
    private final TranscriptService transcriptService;
    private final ConfigService configService;
    
    public SessionEntry loadSession(String sessionKey) {
        String canonicalKey = resolveCanonicalKey(sessionKey);
        return sessionRepository.findById(canonicalKey)
            .orElse(null);
    }
    
    public SessionEntry getOrCreateSession(String sessionKey) {
        String canonicalKey = resolveCanonicalKey(sessionKey);
        return sessionRepository.findById(canonicalKey)
            .orElseGet(() -> createNewSession(canonicalKey));
    }
    
    private SessionEntry createNewSession(String canonicalKey) {
        SessionEntry entry = new SessionEntry();
        entry.setSessionKey(canonicalKey);
        entry.setSessionId(UUID.randomUUID().toString());
        entry.setUpdatedAt(System.currentTimeMillis());
        return sessionRepository.save(entry);
    }
    
    public SessionsListResult listSessions(SessionsListParams params) {
        List<SessionEntry> sessions;
        
        // 基础查询
        if (params.getSpawnedBy() != null) {
            sessions = sessionRepository.findBySpawnedBy(params.getSpawnedBy());
        } else if (params.getLabel() != null) {
            sessions = sessionRepository.findByLabel(params.getLabel());
        } else if (params.getSearch() != null) {
            sessions = sessionRepository.search(params.getSearch());
        } else {
            sessions = sessionRepository.findAll();
        }
        
        // 过滤
        if (params.getActiveMinutes() != null) {
            long cutoff = System.currentTimeMillis() 
                - params.getActiveMinutes() * 60_000;
            sessions = sessions.stream()
                .filter(s -> s.getUpdatedAt() >= cutoff)
                .collect(Collectors.toList());
        }
        
        // 排序
        sessions.sort(Comparator.comparing(
            SessionEntry::getUpdatedAt, 
            Comparator.nullsLast(Comparator.reverseOrder())));
        
        // 限制
        if (params.getLimit() != null) {
            sessions = sessions.stream()
                .limit(params.getLimit())
                .collect(Collectors.toList());
        }
        
        return new SessionsListResult(sessions);
    }
    
    @Transactional
    public SessionEntry patchSession(String key, Map<String, Object> patch) {
        SessionEntry entry = getOrCreateSession(key);
        
        // 应用补丁
        if (patch.containsKey("thinkingLevel")) {
            entry.setThinkingLevel((String) patch.get("thinkingLevel"));
        }
        if (patch.containsKey("modelOverride")) {
            entry.setModelOverride((String) patch.get("modelOverride"));
        }
        // ... 其他字段
        
        entry.setUpdatedAt(System.currentTimeMillis());
        return sessionRepository.save(entry);
    }
    
    @Transactional
    public SessionEntry resetSession(String key) {
        SessionEntry entry = loadSession(key);
        if (entry == null) {
            return createNewSession(resolveCanonicalKey(key));
        }
        
        // 归档旧转录
        transcriptService.archive(entry.getSessionId());
        
        // 重置
        entry.setSessionId(UUID.randomUUID().toString());
        entry.setUpdatedAt(System.currentTimeMillis());
        entry.setInputTokens(0L);
        entry.setOutputTokens(0L);
        entry.setTotalTokens(0L);
        
        return sessionRepository.save(entry);
    }
    
    @Transactional
    public boolean deleteSession(String key) {
        String canonicalKey = resolveCanonicalKey(key);
        SessionEntry entry = sessionRepository.findById(canonicalKey).orElse(null);
        
        if (entry != null) {
            // 归档转录
            transcriptService.archive(entry.getSessionId());
            // 删除
            sessionRepository.delete(entry);
            return true;
        }
        
        return false;
    }
    
    private String resolveCanonicalKey(String sessionKey) {
        if ("main".equals(sessionKey)) {
            return "agent:default:main";
        }
        if (sessionKey.startsWith("agent:")) {
            return sessionKey;
        }
        String defaultAgentId = configService.getDefaultAgentId();
        return "agent:" + defaultAgentId + ":" + sessionKey;
    }
}
```

### 4. 会话Key分类

```java
public enum SessionKind {
    DIRECT, GROUP, GLOBAL, UNKNOWN
}

public class SessionKeyUtils {
    
    public static SessionKind classifyKey(String key, SessionEntry entry) {
        if ("global".equals(key)) {
            return SessionKind.GLOBAL;
        }
        if ("unknown".equals(key)) {
            return SessionKind.UNKNOWN;
        }
        if (entry != null) {
            ChatType chatType = entry.getChatType();
            if (chatType == ChatType.GROUP || chatType == ChatType.CHANNEL) {
                return SessionKind.GROUP;
            }
        }
        if (key.contains(":group:") || key.contains(":channel:")) {
            return SessionKind.GROUP;
        }
        return SessionKind.DIRECT;
    }
    
    public static Optional<GroupKey> parseGroupKey(String key) {
        // agent:xxx:telegram:group:12345
        String[] parts = key.split(":");
        for (int i = 0; i < parts.length - 2; i++) {
            if ("group".equals(parts[i]) || "channel".equals(parts[i])) {
                String channel = parts[i - 1];
                String kind = parts[i];
                String id = String.join(":", 
                    Arrays.copyOfRange(parts, i + 1, parts.length));
                return Optional.of(new GroupKey(channel, kind, id));
            }
        }
        return Optional.empty();
    }
}
```

---

## ✅ 学习检查点

- [x] 理解SessionEntry数据结构
- [x] 理解会话Key规范化流程
- [x] 理解会话存储机制(store.json + .jsonl)
- [x] 理解多Agent会话合并
- [x] 理解Gateway会话方法
- [x] 能够用Java实现SessionService

---

## 📚 下一步

1. **配置管理** - 配置热重载机制
2. **事件路由** - Agent事件系统

---

## 🔗 关键源文件链接

- [session-utils.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/session-utils.ts) - 会话工具
- [sessions/types.ts](https://github.com/openclaw/openclaw/blob/main/src/config/sessions/types.ts) - SessionEntry定义
- [sessions.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-methods/sessions.ts) - Gateway方法

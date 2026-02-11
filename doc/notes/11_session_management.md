# 会话管理深入学习笔记

> 第二阶段:Session实体设计和SessionManager实现

## 📁 核心文件结构

```
src/acp/
├── session.ts           # AcpSessionStore (95行)
├── types.ts             # AcpSession类型 (30行)
└── session-mapper.ts    # 会话映射

src/agents/pi-embedded-runner/
├── session-manager-cache.ts  # 缓存机制 (70行)
└── session-manager-init.ts   # 初始化 (54行)

src/sessions/
└── session-key-utils.ts      # 会话键工具 (76行)
```

---

## 📦 AcpSession 实体

```typescript
type AcpSession = {
  sessionId: SessionId;        // UUID标识符
  sessionKey: string;          // 格式化键 (如 "agent:main:whatsapp:+123")
  cwd: string;                 // 工作目录
  createdAt: number;           // 创建时间戳
  abortController: AbortController | null;  // 取消控制器
  activeRunId: string | null;  // 当前活跃运行ID
};
```

---

## 🔑 SessionKey 格式

```
┌─────────────────────────────────────────────────────────────┐
│                     SessionKey 格式                          │
├─────────────────────────────────────────────────────────────┤
│ agent:<agentId>:<channel>:<target>                          │
│ agent:<agentId>:subagent:<subagentId>                       │
│ agent:<agentId>:acp:<sessionId>                             │
│ agent:<agentId>:<channel>:<target>:thread:<threadId>        │
└─────────────────────────────────────────────────────────────┘

示例:
  agent:main:whatsapp:+15551234567
  agent:main:telegram:123456789
  agent:main:discord:guild:channel:123
  agent:main:acp:a1b2c3d4-e5f6-7890
  agent:main:whatsapp:+15551234567:thread:abc123
```

### 解析函数

```typescript
type ParsedAgentSessionKey = {
  agentId: string;
  rest: string;
};

function parseAgentSessionKey(sessionKey: string): ParsedAgentSessionKey | null {
  // "agent:main:whatsapp:+123" → { agentId: "main", rest: "whatsapp:+123" }
  const parts = sessionKey.split(":").filter(Boolean);
  if (parts[0] !== "agent" || parts.length < 3) return null;
  return { agentId: parts[1], rest: parts.slice(2).join(":") };
}

function isSubagentSessionKey(sessionKey: string): boolean;
function isAcpSessionKey(sessionKey: string): boolean;
function resolveThreadParentSessionKey(sessionKey: string): string | null;
```

---

## 🗄️ AcpSessionStore

### 接口定义

```typescript
type AcpSessionStore = {
  createSession: (params: { 
    sessionKey: string; 
    cwd: string; 
    sessionId?: string 
  }) => AcpSession;
  
  getSession: (sessionId: string) => AcpSession | undefined;
  
  getSessionByRunId: (runId: string) => AcpSession | undefined;
  
  setActiveRun: (
    sessionId: string, 
    runId: string, 
    abortController: AbortController
  ) => void;
  
  clearActiveRun: (sessionId: string) => void;
  
  cancelActiveRun: (sessionId: string) => boolean;
  
  clearAllSessionsForTest: () => void;
};
```

### 内存实现

```typescript
function createInMemorySessionStore(): AcpSessionStore {
  const sessions = new Map<string, AcpSession>();
  const runIdToSessionId = new Map<string, string>();
  
  // 双向映射: sessionId → session, runId → sessionId
  
  return {
    createSession: (params) => {
      const session: AcpSession = {
        sessionId: params.sessionId ?? randomUUID(),
        sessionKey: params.sessionKey,
        cwd: params.cwd,
        createdAt: Date.now(),
        abortController: null,
        activeRunId: null,
      };
      sessions.set(session.sessionId, session);
      return session;
    },
    
    cancelActiveRun: (sessionId) => {
      const session = sessions.get(sessionId);
      if (!session?.abortController) return false;
      session.abortController.abort();  // 发送取消信号
      return true;
    },
    // ...
  };
}
```

---

## 💾 Session缓存机制

```typescript
// 默认TTL: 45秒
const DEFAULT_SESSION_MANAGER_TTL_MS = 45_000;

const SESSION_MANAGER_CACHE = new Map<string, {
  sessionFile: string;
  loadedAt: number;
}>();

// 跟踪访问
function trackSessionManagerAccess(sessionFile: string): void;

// 检查是否缓存有效
function isSessionManagerCached(sessionFile: string): boolean;

// 预热文件(读取4KB到页面缓存)
async function prewarmSessionFile(sessionFile: string): Promise<void>;
```

---

## ☕ Java实现对照

### 1. Session实体

```java
@Data
@Builder
public class AgentSession {
    private String sessionId;
    private String sessionKey;
    private String cwd;
    private Instant createdAt;
    private String activeRunId;
    
    // Java中使用CompletableFuture取消
    @Builder.Default
    private AtomicReference<CompletableFuture<?>> activeFuture = new AtomicReference<>();
    
    public boolean cancel() {
        CompletableFuture<?> future = activeFuture.get();
        if (future != null) {
            return future.cancel(true);
        }
        return false;
    }
}
```

### 2. SessionStore

```java
@Component
public class InMemorySessionStore implements SessionStore {
    
    private final ConcurrentMap<String, AgentSession> sessions = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, String> runIdToSessionId = new ConcurrentHashMap<>();
    
    @Override
    public AgentSession createSession(CreateSessionParams params) {
        String sessionId = params.getSessionId() != null 
            ? params.getSessionId() 
            : UUID.randomUUID().toString();
            
        AgentSession session = AgentSession.builder()
            .sessionId(sessionId)
            .sessionKey(params.getSessionKey())
            .cwd(params.getCwd())
            .createdAt(Instant.now())
            .build();
            
        sessions.put(sessionId, session);
        return session;
    }
    
    @Override
    public Optional<AgentSession> getSession(String sessionId) {
        return Optional.ofNullable(sessions.get(sessionId));
    }
    
    @Override
    public Optional<AgentSession> getSessionByRunId(String runId) {
        String sessionId = runIdToSessionId.get(runId);
        return sessionId != null ? getSession(sessionId) : Optional.empty();
    }
    
    @Override
    public void setActiveRun(String sessionId, String runId, CompletableFuture<?> future) {
        getSession(sessionId).ifPresent(session -> {
            session.setActiveRunId(runId);
            session.getActiveFuture().set(future);
            runIdToSessionId.put(runId, sessionId);
        });
    }
    
    @Override
    public boolean cancelActiveRun(String sessionId) {
        return getSession(sessionId)
            .map(AgentSession::cancel)
            .orElse(false);
    }
}
```

### 3. SessionKey解析

```java
@UtilityClass
public class SessionKeyUtils {
    
    public record ParsedSessionKey(String agentId, String rest) {}
    
    public static Optional<ParsedSessionKey> parseAgentSessionKey(String sessionKey) {
        if (sessionKey == null || sessionKey.isBlank()) {
            return Optional.empty();
        }
        
        String[] parts = sessionKey.trim().split(":");
        if (parts.length < 3 || !"agent".equals(parts[0])) {
            return Optional.empty();
        }
        
        String agentId = parts[1].trim();
        String rest = String.join(":", Arrays.copyOfRange(parts, 2, parts.length));
        
        return Optional.of(new ParsedSessionKey(agentId, rest));
    }
    
    public static boolean isSubagentSessionKey(String sessionKey) {
        return parseAgentSessionKey(sessionKey)
            .map(parsed -> parsed.rest().toLowerCase().startsWith("subagent:"))
            .orElse(false);
    }
    
    public static boolean isAcpSessionKey(String sessionKey) {
        return parseAgentSessionKey(sessionKey)
            .map(parsed -> parsed.rest().toLowerCase().startsWith("acp:"))
            .orElse(false);
    }
}
```

### 4. Session缓存

```java
@Component
public class SessionCache {
    
    private static final Duration DEFAULT_TTL = Duration.ofSeconds(45);
    
    private final Cache<String, CacheEntry> cache = Caffeine.newBuilder()
        .expireAfterWrite(DEFAULT_TTL)
        .maximumSize(1000)
        .build();
    
    @Value
    private static class CacheEntry {
        String sessionFile;
        Instant loadedAt;
    }
    
    public void trackAccess(String sessionFile) {
        cache.put(sessionFile, new CacheEntry(sessionFile, Instant.now()));
    }
    
    public boolean isCached(String sessionFile) {
        return cache.getIfPresent(sessionFile) != null;
    }
    
    public CompletableFuture<Void> prewarm(String sessionFile) {
        if (isCached(sessionFile)) {
            return CompletableFuture.completedFuture(null);
        }
        
        return CompletableFuture.runAsync(() -> {
            try {
                // 读取4KB预热页面缓存
                byte[] buffer = new byte[4096];
                try (FileInputStream fis = new FileInputStream(sessionFile)) {
                    fis.read(buffer);
                }
                trackAccess(sessionFile);
            } catch (IOException ignored) {
                // 文件不存在,SessionManager会创建
            }
        });
    }
}
```

---

## ✅ 学习检查点

- [x] 理解AcpSession实体结构
- [x] 理解SessionKey格式和解析
- [x] 理解AcpSessionStore接口和内存实现
- [x] 理解会话缓存和预热机制
- [x] 能够用Java实现Session实体和Store

---

## 🔗 关键源文件链接

- [session.ts](https://github.com/openclaw/openclaw/blob/main/src/acp/session.ts) - SessionStore
- [types.ts](https://github.com/openclaw/openclaw/blob/main/src/acp/types.ts) - AcpSession类型
- [session-key-utils.ts](https://github.com/openclaw/openclaw/blob/main/src/sessions/session-key-utils.ts) - 键解析
- [session-manager-cache.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-embedded-runner/session-manager-cache.ts) - 缓存

# RPC模式通信学习笔记

> 第一阶段Agent Runtime部分:消息调用链和事件发布

## 📁 核心文件结构

```
src/
├── commands/
│   └── agent.ts              # agentCommand主入口 (531行)
├── infra/
│   └── agent-events.ts       # 事件发布订阅 (84行)
└── agents/
    └── model-fallback.ts     # 模型故障转移 (395行)
```

---

## 🔄 agentCommand 调用链

```
┌─────────────────────────────────────────────────────────────┐
│                     调用入口                                 │
│  Gateway WS → server-methods/agent.ts → agentCommand        │
│  OpenAI HTTP → openai-http.ts → agentCommand                │
│  Node事件 → server-node-events.ts → agentCommand            │
│  启动钩子 → boot.ts → agentCommand                          │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│           agentCommand(opts, runtime, deps)                 │
│                      commands/agent.ts                      │
└─────────────────────────────────────────────────────────────┘
       │
       ├── 1. resolveSession() → 会话解析
       ├── 2. resolveConfiguredModelRef() → 模型解析
       ├── 3. buildWorkspaceSkillSnapshot() → 技能快照
       ├── 4. registerAgentRunContext() → 注册运行上下文
       │
       └── 5. runWithModelFallback() ─────────────────────┐
                            │                              │
                            ▼                              │
              ┌─────────────────────────┐                 │
              │ runEmbeddedPiAgent()    │ ◄───────────────┘
              │ 或 runCliAgent()        │   故障转移重试
              └───────────┬─────────────┘
                          │
       ├── 6. emitAgentEvent(lifecycle:end) → 发布结束事件
       ├── 7. updateSessionStoreAfterAgentRun() → 更新会话
       └── 8. deliverAgentCommandResult() → 投递结果
```

---

## 📡 事件发布订阅 (agent-events.ts)

### 核心类型

```typescript
type AgentEventStream = "lifecycle" | "tool" | "assistant" | "error";

type AgentEventPayload = {
  runId: string;        // 运行ID
  seq: number;          // 序列号(自增)
  stream: AgentEventStream;
  ts: number;           // 时间戳
  data: Record<string, unknown>;
  sessionKey?: string;
};

type AgentRunContext = {
  sessionKey?: string;
  verboseLevel?: VerboseLevel;
  isHeartbeat?: boolean;
};
```

### 发布/订阅机制

```typescript
// 内部状态
const seqByRun = new Map<string, number>();          // runId → 序列号
const listeners = new Set<(evt) => void>();          // 监听器集合
const runContextById = new Map<string, AgentRunContext>(); // 运行上下文

// 发布事件
function emitAgentEvent(event) {
  const nextSeq = (seqByRun.get(event.runId) ?? 0) + 1;
  seqByRun.set(event.runId, nextSeq);
  
  const enriched = { ...event, seq: nextSeq, ts: Date.now() };
  
  for (const listener of listeners) {
    listener(enriched);
  }
}

// 订阅事件
function onAgentEvent(listener) {
  listeners.add(listener);
  return () => listeners.delete(listener);  // 返回取消订阅函数
}
```

---

## 🔁 模型故障转移 (model-fallback.ts)

### runWithModelFallback

```typescript
async function runWithModelFallback<T>(params: {
  cfg: OpenClawConfig;
  provider: string;
  model: string;
  fallbacksOverride?: string[];
  run: (provider: string, model: string) => Promise<T>;
  onError?: (attempt) => void;
}): Promise<{ result: T; provider: string; model: string; attempts: FallbackAttempt[] }>
```

### 故障转移流程

```
┌─────────────────────────────────────────────────────────────┐
│           resolveFallbackCandidates()                       │
│                                                             │
│  1. 主模型: provider/model                                   │
│  2. 配置故障转移: agents.defaults.model.fallbacks           │
│  3. 默认模型: primary配置                                    │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              for (candidate of candidates)                  │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ 检查认证Profile冷却状态                                │  │
│  │ if (所有Profile冷却中) → 跳过此候选                    │  │
│  └──────────────────────────────────────────────────────┘  │
│                          │                                  │
│                          ▼                                  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ try { result = await run(provider, model) }          │  │
│  │ → 成功返回                                            │  │
│  └──────────────────────────────────────────────────────┘  │
│                          │                                  │
│                          ▼ (失败)                          │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ if (AbortError) → 直接抛出                            │  │
│  │ if (!FailoverError) → 直接抛出                        │  │
│  │ else → 记录attempt, 尝试下一个候选                    │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

---

## ☕ Java实现对照

### 1. 事件发布服务

```java
@Service
@Slf4j
public class AgentEventService {
    
    private final Map<String, Integer> seqByRun = new ConcurrentHashMap<>();
    private final Set<Consumer<AgentEventPayload>> listeners = ConcurrentHashMap.newKeySet();
    private final Map<String, AgentRunContext> runContextById = new ConcurrentHashMap<>();
    
    public void registerRunContext(String runId, AgentRunContext context) {
        runContextById.merge(runId, context, (old, neu) -> {
            if (neu.getSessionKey() != null) old.setSessionKey(neu.getSessionKey());
            if (neu.getVerboseLevel() != null) old.setVerboseLevel(neu.getVerboseLevel());
            return old;
        });
    }
    
    public void emit(AgentEventPayload event) {
        int seq = seqByRun.merge(event.getRunId(), 1, Integer::sum);
        
        AgentRunContext ctx = runContextById.get(event.getRunId());
        String sessionKey = event.getSessionKey() != null 
            ? event.getSessionKey() 
            : (ctx != null ? ctx.getSessionKey() : null);
        
        AgentEventPayload enriched = event.toBuilder()
            .seq(seq)
            .ts(System.currentTimeMillis())
            .sessionKey(sessionKey)
            .build();
        
        for (Consumer<AgentEventPayload> listener : listeners) {
            try {
                listener.accept(enriched);
            } catch (Exception e) {
                log.warn("Event listener error", e);
            }
        }
    }
    
    public Runnable subscribe(Consumer<AgentEventPayload> listener) {
        listeners.add(listener);
        return () -> listeners.remove(listener);
    }
    
    public void clearRunContext(String runId) {
        runContextById.remove(runId);
        seqByRun.remove(runId);
    }
}
```

### 2. 模型故障转移服务

```java
@Service
@Slf4j
public class ModelFallbackService {
    
    private final AuthProfileStore authStore;
    
    public <T> FallbackResult<T> runWithFallback(
        OpenClawConfig cfg,
        String provider,
        String model,
        List<String> fallbacksOverride,
        BiFunction<String, String, T> runner
    ) {
        List<ModelCandidate> candidates = resolveCandidates(cfg, provider, model, fallbacksOverride);
        List<FallbackAttempt> attempts = new ArrayList<>();
        Exception lastError = null;
        
        for (int i = 0; i < candidates.size(); i++) {
            ModelCandidate candidate = candidates.get(i);
            
            // 检查Profile冷却
            List<String> profileIds = authStore.getOrderedProfiles(candidate.getProvider());
            boolean anyAvailable = profileIds.stream()
                .anyMatch(id -> !authStore.isInCooldown(id));
            
            if (!profileIds.isEmpty() && !anyAvailable) {
                attempts.add(FallbackAttempt.builder()
                    .provider(candidate.getProvider())
                    .model(candidate.getModel())
                    .error("Provider in cooldown")
                    .reason("rate_limit")
                    .build());
                continue;
            }
            
            try {
                T result = runner.apply(candidate.getProvider(), candidate.getModel());
                return FallbackResult.<T>builder()
                    .result(result)
                    .provider(candidate.getProvider())
                    .model(candidate.getModel())
                    .attempts(attempts)
                    .build();
            } catch (AbortException e) {
                throw e;  // 直接抛出中止错误
            } catch (FailoverException e) {
                lastError = e;
                attempts.add(FallbackAttempt.builder()
                    .provider(candidate.getProvider())
                    .model(candidate.getModel())
                    .error(e.getMessage())
                    .reason(e.getReason())
                    .status(e.getStatus())
                    .build());
            } catch (Exception e) {
                throw e;  // 非故障转移错误直接抛出
            }
        }
        
        // 所有候选都失败
        String summary = attempts.stream()
            .map(a -> String.format("%s/%s: %s", a.getProvider(), a.getModel(), a.getError()))
            .collect(Collectors.joining(" | "));
        throw new AllModelsFailedException("All models failed: " + summary, lastError);
    }
}
```

### 3. AgentCommand服务

```java
@Service
@Slf4j
public class AgentCommandService {
    
    private final SessionResolver sessionResolver;
    private final ModelFallbackService fallbackService;
    private final AgentEventService eventService;
    private final AgentRunner agentRunner;
    
    public AgentRunResult execute(AgentCommandOpts opts) {
        // 1. 会话解析
        SessionResolution session = sessionResolver.resolve(opts);
        String runId = opts.getRunId() != null ? opts.getRunId() : session.getSessionId();
        
        // 2. 注册运行上下文
        eventService.registerRunContext(runId, AgentRunContext.builder()
            .sessionKey(session.getSessionKey())
            .verboseLevel(opts.getVerboseLevel())
            .build());
        
        long startedAt = System.currentTimeMillis();
        
        try {
            // 3. 带故障转移的执行
            FallbackResult<AgentRunResult> fallbackResult = fallbackService.runWithFallback(
                cfg, provider, model, null,
                (p, m) -> agentRunner.run(AgentRunParams.builder()
                    .sessionId(session.getSessionId())
                    .sessionKey(session.getSessionKey())
                    .prompt(opts.getMessage())
                    .provider(p)
                    .model(m)
                    .build())
            );
            
            // 4. 发布结束事件
            eventService.emit(AgentEventPayload.builder()
                .runId(runId)
                .stream("lifecycle")
                .data(Map.of(
                    "phase", "end",
                    "startedAt", startedAt,
                    "endedAt", System.currentTimeMillis()
                ))
                .build());
            
            return fallbackResult.getResult();
            
        } catch (Exception e) {
            // 发布错误事件
            eventService.emit(AgentEventPayload.builder()
                .runId(runId)
                .stream("lifecycle")
                .data(Map.of(
                    "phase", "error",
                    "error", e.getMessage()
                ))
                .build());
            throw e;
        } finally {
            eventService.clearRunContext(runId);
        }
    }
}
```

---

## ✅ 学习检查点

- [x] 理解agentCommand调用链
- [x] 理解事件发布订阅机制(emitAgentEvent/onAgentEvent)
- [x] 理解模型故障转移(runWithModelFallback)
- [x] 理解Profile冷却跳过机制
- [x] 能够用Java实现事件服务和故障转移

---

## 🔗 关键源文件链接

- [agent.ts](https://github.com/openclaw/openclaw/blob/main/src/commands/agent.ts) - agentCommand入口
- [agent-events.ts](https://github.com/openclaw/openclaw/blob/main/src/infra/agent-events.ts) - 事件发布
- [model-fallback.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/model-fallback.ts) - 故障转移

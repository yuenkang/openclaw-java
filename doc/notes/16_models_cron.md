# 模型集成与定时任务学习笔记

> 第三阶段:多模型Provider和Cron系统

---

## 🤖 模型提供者架构

### 支持的Provider列表

| Provider | API Base URL | 认证方式 |
|----------|-------------|---------|
| Anthropic | api.anthropic.com | ANTHROPIC_API_KEY |
| OpenAI | api.openai.com | OPENAI_API_KEY |
| Google/Gemini | generativelanguage.googleapis.com | GEMINI_API_KEY |
| Ollama | 127.0.0.1:11434 | 无需认证 |
| MiniMax | api.minimax.chat/v1 | 平台API Key |
| Moonshot | api.moonshot.cn/v1 | 平台API Key |
| Qwen | 平台Portal | OAuth |
| Venice | venice.ai | VENICE_API_KEY |
| AWS Bedrock | AWS SDK | AWS凭证 |
| GitHub Copilot | api.githubcopilot.com | Copilot Token |

### Provider加载流程

```
1. 显式配置 (config.models.providers)
   ↓
2. normalizeProviders() → 标准化API Key和URL
   ↓
3. resolveImplicitProviders() → 自动发现:
   ├── 检测环境变量中的API Key → 启用对应Provider
   ├── Ollama → 本地HTTP探测
   └── Copilot → Token文件检测
   ↓
4. 合并显式+隐式Provider列表
```

### 模型定义

```typescript
type ModelDefinitionConfig = {
  id: string;           // "anthropic/claude-sonnet-4-5"
  name?: string;
  provider: string;     // "anthropic"
  contextWindow: number;
  maxTokens: number;
  cost: { input, output, cacheRead, cacheWrite };
  input: ("text"|"image")[];
};

// 默认别名
const MODEL_ALIASES = {
  opus: "anthropic/claude-opus-4-6",
  sonnet: "anthropic/claude-sonnet-4-5",
  gpt: "openai/gpt-5.2",
};
```

---

## ⏰ Cron定时任务系统

### CronService API

```typescript
class CronService {
  start(): void          // 启动调度器
  stop(): void           // 停止调度器
  status(): CronStatus   // 获取状态
  list(): CronJob[]      // 列出任务
  add(input): CronJob    // 添加任务
  update(id, patch): void // 更新任务
  remove(id): void       // 删除任务
  run(id, mode): void    // 手动触发 (due/force)
  wake(opts): void       // 唤醒心跳
}
```

### Gateway Cron集成

```
buildGatewayCronService()
├── cronEnabled: 配置 + 环境变量控制
├── enqueueSystemEvent: 发送系统事件到agent session
├── runHeartbeatOnce: 执行心跳检查
├── runIsolatedAgentJob: 在独立session中执行agent任务
│   └── sessionKey: "cron:<jobId>"
└── onEvent: 广播cron事件 + 写运行日志
```

### Job执行模式

| 模式 | 说明 |
|------|------|
| `due` | 仅当到期时执行 |
| `force` | 强制立即执行 |
| `now` | 唤醒心跳立即处理 |
| `next-heartbeat` | 下次心跳时处理 |

---

## ☕ Java实现对照

### 1. 模型Provider注册

```java
@Service
public class ModelProviderRegistry {
    
    private final Map<String, ModelProvider> providers = new ConcurrentHashMap<>();
    private final Map<String, String> modelAliases = Map.of(
        "opus", "anthropic/claude-opus-4-6",
        "sonnet", "anthropic/claude-sonnet-4-5",
        "gpt", "openai/gpt-5.2"
    );
    
    @PostConstruct
    public void init() {
        // 显式配置
        config.getModels().getProviders().forEach(this::registerProvider);
        // 隐式发现
        discoverImplicitProviders();
    }
    
    public ModelProvider resolve(String modelId) {
        String resolved = modelAliases.getOrDefault(modelId, modelId);
        String provider = resolved.split("/")[0];
        return providers.get(provider);
    }
}

public interface ModelProvider {
    String getId();
    String getApiBaseUrl();
    CompletableFuture<ChatResponse> chat(ChatRequest request);
    List<ModelDefinition> listModels();
}
```

### 2. Cron调度服务

```java
@Service
public class CronService {
    
    private final ScheduledExecutorService scheduler = 
        Executors.newScheduledThreadPool(2);
    private final ConcurrentMap<String, CronJob> jobs = new ConcurrentHashMap<>();
    
    public CronJob add(CronJobCreate input) {
        CronJob job = CronJob.builder()
            .id(UUID.randomUUID().toString())
            .schedule(input.getSchedule())
            .agentId(input.getAgentId())
            .message(input.getMessage())
            .enabled(true)
            .build();
        
        jobs.put(job.getId(), job);
        scheduleJob(job);
        return job;
    }
    
    private void scheduleJob(CronJob job) {
        CronExpression cron = CronExpression.parse(job.getSchedule());
        Instant next = cron.next(Instant.now());
        long delay = Duration.between(Instant.now(), next).toMillis();
        
        scheduler.schedule(() -> executeJob(job), delay, TimeUnit.MILLISECONDS);
    }
    
    private void executeJob(CronJob job) {
        String sessionKey = "cron:" + job.getId();
        agentRunner.runIsolated(job.getAgentId(), sessionKey, job.getMessage());
        scheduleJob(job); // 重新调度
    }
}
```

---

## ✅ 学习检查点

- [x] 理解多Provider架构(10+模型提供者)
- [x] 理解Provider自动发现机制(环境变量+本地探测)
- [x] 理解CronService API和Gateway集成
- [x] 能够用Java实现Provider注册和Cron调度

---

## 🔗 关键源文件链接

- [models-config.providers.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/models-config.providers.ts) - 模型Provider
- [server-cron.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/server-cron.ts) - Gateway Cron
- [service.ts](https://github.com/openclaw/openclaw/blob/main/src/cron/service.ts) - CronService

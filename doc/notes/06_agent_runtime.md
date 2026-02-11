# Agent Runtime 核心架构学习笔记

> 第一阶段Agent Runtime部分:Pi Agent执行机制

## 📁 核心文件结构

```
src/agents/
├── pi-embedded-runner/
│   ├── run.ts              # Agent运行主入口 (711行)
│   ├── run/
│   │   ├── attempt.ts      # 单次尝试执行
│   │   └── payloads.ts     # 响应构建
│   ├── types.ts            # 类型定义
│   ├── compact.ts          # 上下文压缩
│   └── model.ts            # 模型解析
├── pi-embedded-subscribe.ts # 流式响应订阅 (567行)
├── pi-tools.ts              # 工具集创建 (454行)
├── system-prompt.ts         # 系统提示构建 (646行)
├── model-auth.ts            # 认证配置
└── model-fallback.ts        # 故障转移
```

---

## 🔄 Agent运行主流程

```
┌─────────────────────────────────────────────────────────────┐
│                runEmbeddedPiAgent (run.ts)                  │
└─────────────────────────────────────────────────────────────┘
                            │
        ┌───────────────────┼───────────────────┐
        ▼                   ▼                   ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ 1.模型解析   │    │ 2.认证配置  │    │ 3.上下文检查│
│ resolveModel│    │ ensureAuth  │    │ ctxGuard    │
└──────┬──────┘    └──────┬──────┘    └──────┬──────┘
       │                  │                  │
       └──────────────────┼──────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                 while (true) 重试循环                        │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  runEmbeddedAttempt() - 单次执行尝试                   │  │
│  └──────────────────────────────────────────────────────┘  │
│                          │                                  │
│           ┌──────────────┼──────────────┐                  │
│           ▼              ▼              ▼                  │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐          │
│  │ Context     │ │ Auth        │ │ Thinking    │          │
│  │ Overflow?   │ │ Failure?    │ │ Level降级?  │          │
│  │ →Compaction │ │ →轮转Profile│ │ →重试       │          │
│  └─────────────┘ └─────────────┘ └─────────────┘          │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│              buildEmbeddedRunPayloads()                     │
│                     构建响应                                 │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔑 核心类型

### EmbeddedPiRunResult

```typescript
type EmbeddedPiRunResult = {
  payloads?: Array<{
    text?: string;           // 响应文本
    mediaUrl?: string;       // 媒体URL
    isError?: boolean;       // 是否错误
  }>;
  meta: {
    durationMs: number;      // 执行时长
    agentMeta?: {
      sessionId: string;
      provider: string;
      model: string;
      usage?: { input, output, total };
    };
    aborted?: boolean;
    stopReason?: string;     // completed/tool_calls
    pendingToolCalls?: Array<{ id, name, arguments }>;
  };
  didSendViaMessagingTool?: boolean;
};
```

---

## 🛠 故障处理机制

### 1. 认证Profile轮转

```typescript
// 预加载认证候选列表
const profileOrder = resolveAuthProfileOrder({
  cfg, store, provider, preferredProfile
});

// 失败时轮转
const advanceAuthProfile = async () => {
  while (nextIndex < profileCandidates.length) {
    if (isProfileInCooldown(store, candidate)) continue;
    await applyApiKeyInfo(candidate);
    profileIndex = nextIndex;
    return true;
  }
  return false;
};
```

### 2. 上下文溢出自动压缩

```typescript
const MAX_OVERFLOW_COMPACTION_ATTEMPTS = 3;

if (isContextOverflowError(errorText)) {
  if (overflowCompactionAttempts < MAX_ATTEMPTS) {
    const compactResult = await compactEmbeddedPiSessionDirect({...});
    if (compactResult.compacted) {
      continue;  // 重试
    }
  }
}
```

### 3. Thinking Level降级

```typescript
const fallbackThinking = pickFallbackThinkingLevel({
  message: errorText,
  attempted: attemptedThinking,  // Set<ThinkLevel>
});
if (fallbackThinking) {
  thinkLevel = fallbackThinking;
  continue;  // 重试
}
```

---

## 🧰 工具系统

### createOpenClawCodingTools

```typescript
function createOpenClawCodingTools(options?: {
  exec?: ExecToolDefaults;              // 执行配置
  sandbox?: SandboxContext;             // 沙箱配置
  modelProvider?: string;               // 模型提供者
  availableChannels?: ChannelPattern[]; // 可用渠道
  toolProfilePolicy?: ToolPolicy;       // 工具策略
  senderIsOwner?: boolean;              // 是否Owner
}): AnyAgentTool[]
```

**工具类别:**
- **Coding Tools**: read_file, edit_file, write_file
- **Exec Tools**: bash, process
- **OpenClaw Tools**: sessions, channels, memory_search
- **Messaging Tools**: telegram, whatsapp, discord, slack

---

## 📝 系统提示构建

### buildAgentSystemPrompt

```typescript
function buildAgentSystemPrompt(params: {
  workspaceDir: string;
  defaultThinkLevel?: ThinkLevel;
  extraSystemPrompt?: string;
  toolNames?: string[];
  skillsPrompt?: string;
  sandbox?: EmbeddedSandboxInfo;
  mode?: PromptMode;  // full | minimal | none
  ...
}): string
```

**提示模块:**

| 模块 | 函数 | 说明 |
|------|------|------|
| Skills | buildSkillsSection | 技能系统 |
| Memory | buildMemorySection | 记忆搜索 |
| Messaging | buildMessagingSection | 消息发送 |
| Voice | buildVoiceSection | TTS支持 |
| Docs | buildDocsSection | 文档路径 |
| ReplyTags | buildReplyTagsSection | 回复标记 |
| UserIdentity | buildUserIdentitySection | 用户身份 |
| Time | buildTimeSection | 时间信息 |
| Runtime | buildRuntimeLine | 运行时信息 |

---

## ☕ Java实现对照

### 1. 运行结果类型

```java
@Data
@Builder
public class AgentRunResult {
    private List<ResponsePayload> payloads;
    private AgentRunMeta meta;
    private boolean didSendViaMessagingTool;
    
    @Data
    public static class ResponsePayload {
        private String text;
        private String mediaUrl;
        private boolean isError;
    }
    
    @Data
    public static class AgentRunMeta {
        private long durationMs;
        private AgentMeta agentMeta;
        private boolean aborted;
        private String stopReason;
        private List<PendingToolCall> pendingToolCalls;
    }
}
```

### 2. Agent Runner

```java
@Service
@Slf4j
public class AgentRunner {
    
    private final ModelResolver modelResolver;
    private final AuthProfileStore authStore;
    private final CompactionService compactionService;
    private final SystemPromptBuilder promptBuilder;
    
    private static final int MAX_COMPACTION_ATTEMPTS = 3;
    
    public AgentRunResult runAgent(AgentRunParams params) {
        long started = System.currentTimeMillis();
        
        // 1. 解析模型
        Model model = modelResolver.resolve(params.getProvider(), params.getModelId());
        
        // 2. 获取认证候选列表
        List<String> profileCandidates = authStore.getOrderedProfiles(
            params.getProvider(), params.getPreferredProfile());
        
        int profileIndex = 0;
        String thinkLevel = params.getThinkLevel();
        Set<String> attemptedThinking = new HashSet<>();
        int compactionAttempts = 0;
        
        while (profileIndex < profileCandidates.size()) {
            String profileId = profileCandidates.get(profileIndex);
            
            // 跳过冷却中的profile
            if (authStore.isInCooldown(profileId)) {
                profileIndex++;
                continue;
            }
            
            try {
                attemptedThinking.add(thinkLevel);
                
                // 执行单次尝试
                AttemptResult attempt = runAttempt(params, model, profileId, thinkLevel);
                
                if (attempt.getPromptError() != null) {
                    String errorText = attempt.getPromptError().getMessage();
                    
                    // 上下文溢出 -> 自动压缩
                    if (isContextOverflowError(errorText)) {
                        if (compactionAttempts < MAX_COMPACTION_ATTEMPTS) {
                            compactionAttempts++;
                            boolean compacted = compactionService.compact(params.getSessionId());
                            if (compacted) continue;
                        }
                        return buildOverflowResult(started);
                    }
                    
                    // 认证失败 -> 轮转profile
                    if (isAuthError(errorText)) {
                        authStore.markFailure(profileId, "auth");
                        profileIndex++;
                        continue;
                    }
                    
                    throw attempt.getPromptError();
                }
                
                // 检查thinking level降级
                String fallback = pickFallbackThinking(
                    attempt.getLastAssistant().getErrorMessage(), attemptedThinking);
                if (fallback != null) {
                    thinkLevel = fallback;
                    continue;
                }
                
                // 成功
                authStore.markGood(profileId);
                return buildSuccessResult(attempt, started);
                
            } catch (Exception e) {
                if (canAdvanceProfile(profileCandidates, profileIndex)) {
                    profileIndex++;
                    continue;
                }
                throw e;
            }
        }
        
        throw new NoAvailableProfileException("All profiles exhausted");
    }
}
```

### 3. 系统提示构建器

```java
@Component
public class SystemPromptBuilder {
    
    public String build(SystemPromptParams params) {
        StringBuilder sb = new StringBuilder();
        
        // 基础身份
        sb.append("You are ").append(params.getAgentName()).append(".\n\n");
        
        // 技能模块
        if (params.getSkillsPrompt() != null) {
            sb.append(buildSkillsSection(params));
        }
        
        // 记忆模块
        if (params.hasMemoryTool()) {
            sb.append(buildMemorySection(params));
        }
        
        // 消息模块
        if (params.hasMessagingTools()) {
            sb.append(buildMessagingSection(params));
        }
        
        // 运行时信息
        sb.append(buildRuntimeSection(params));
        
        // 额外提示
        if (params.getExtraPrompt() != null) {
            sb.append("\n").append(params.getExtraPrompt());
        }
        
        return sb.toString();
    }
    
    private String buildSkillsSection(SystemPromptParams params) {
        return """
            ## Skills
            You have access to the following skills:
            %s
            
            Use the read_file tool to read skill files.
            """.formatted(params.getSkillsPrompt());
    }
}
```

---

## ✅ 学习检查点

- [x] 理解Agent运行主流程(run.ts)
- [x] 理解认证Profile轮转机制
- [x] 理解上下文溢出自动压缩
- [x] 理解Thinking Level降级重试
- [x] 理解工具系统(pi-tools.ts)
- [x] 理解系统提示构建(system-prompt.ts)
- [x] 能够用Java实现AgentRunner

---

## 📚 下一步

继续深入Agent Runtime的其他模块:
1. **流式响应处理** - pi-embedded-subscribe.ts
2. **工具执行** - bash-tools.exec.ts
3. **模型故障转移** - model-fallback.ts

---

## 🔗 关键源文件链接

- [run.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-embedded-runner/run.ts) - 运行主入口
- [pi-tools.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-tools.ts) - 工具创建
- [system-prompt.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/system-prompt.ts) - 系统提示

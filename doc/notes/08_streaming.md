# 工具和块流式传输学习笔记

> 第一阶段Agent Runtime部分:流式响应处理机制

## 📁 核心文件结构

```
src/agents/
├── pi-embedded-subscribe.ts                    # 主订阅入口
├── pi-embedded-subscribe.handlers.types.ts    # 类型定义 (109行)
├── pi-embedded-subscribe.handlers.tools.ts    # 工具事件处理 (230行)
├── pi-embedded-subscribe.handlers.messages.ts # 消息事件处理 (372行)
└── pi-embedded-block-chunker.ts               # 块分割器 (353行)
```

---

## 🔧 工具事件处理

### 三阶段生命周期

```
┌─────────────────────────────────────────────────────────────┐
│       handleToolExecutionStart (开始)                        │
│                                                             │
│  • 刷新blockReplyBuffer                                      │
│  • 规范化工具名称                                             │
│  • 提取工具元信息(exec flags等)                               │
│  • 发布 tool:start 事件                                      │
│  • 跟踪消息工具发送(pendingMessagingTargets)                  │
└─────────────────────────────────────────────────────────────┘
                            ▼
┌─────────────────────────────────────────────────────────────┐
│       handleToolExecutionUpdate (更新)                       │
│                                                             │
│  • 发布 tool:update 事件                                     │
│  • 传递 partialResult                                        │
└─────────────────────────────────────────────────────────────┘
                            ▼
┌─────────────────────────────────────────────────────────────┐
│       handleToolExecutionEnd (结束)                          │
│                                                             │
│  • 提取最终结果                                               │
│  • 记录工具错误(lastToolError)                               │
│  • 提交消息工具发送记录                                        │
│  • 发布 tool:result 事件                                     │
│  • 输出工具结果文本                                           │
└─────────────────────────────────────────────────────────────┘
```

### 工具事件数据

```typescript
// tool:start
{
  phase: "start",
  name: string,       // 规范化工具名称
  toolCallId: string,
  args: object
}

// tool:update
{
  phase: "update",
  name: string,
  toolCallId: string,
  partialResult: unknown
}

// tool:result
{
  phase: "result",
  name: string,
  toolCallId: string,
  meta?: string,      // 工具元信息
  isError: boolean,
  result: unknown
}
```

---

## 💬 消息事件处理

### handleMessageUpdate (核心流式)

```typescript
// 处理 text_delta / text_start / text_end
handleMessageUpdate(ctx, evt) {
  // 1. 增量追加到buffer
  ctx.state.deltaBuffer += chunk;
  ctx.blockChunker.append(chunk);
  
  // 2. 流式推理(如果启用)
  ctx.emitReasoningStream(extractThinkingFromTaggedStream(deltaBuffer));
  
  // 3. 发布 assistant:update 事件
  emitAgentEvent({
    stream: "assistant",
    data: { text: cleanedText, delta: deltaText }
  });
  
  // 4. 块分割排水
  ctx.blockChunker.drain({ force: false, emit: ctx.emitBlockChunk });
}
```

### handleMessageEnd (消息结束)

```typescript
handleMessageEnd(ctx, evt) {
  // 1. 提取最终文本和推理
  const text = extractAssistantText(msg);
  const thinking = extractAssistantThinking(msg);
  
  // 2. 最终化assistantTexts
  ctx.finalizeAssistantTexts({ text, addedDuringMessage, chunkerHasBuffered });
  
  // 3. 发送推理消息(如果启用)
  if (includeReasoning) onBlockReply({ text: formattedReasoning });
  
  // 4. 强制排水剩余块
  ctx.blockChunker.drain({ force: true, emit: ctx.emitBlockChunk });
  
  // 5. 重置状态
  ctx.state.deltaBuffer = "";
  ctx.blockChunker.reset();
}
```

---

## 📦 块分割器 (EmbeddedBlockChunker)

### 配置选项

```typescript
type BlockReplyChunking = {
  minChars: number;     // 最小字符数才分割
  maxChars: number;     // 强制分割阈值
  breakPreference?: "paragraph" | "newline" | "sentence";
  flushOnParagraph?: boolean;  // 段落边界立即刷新
};
```

### 分割优先级

```
1. 段落边界 (\n\n) ← breakPreference="paragraph"
2. 换行边界 (\n)   ← breakPreference="newline"
3. 句尾边界 (.!?) ← breakPreference="sentence"
4. 空白符回退
5. 硬分割 (maxChars到达)
```

### 代码块安全机制

```typescript
// 检测代码围栏范围
const fenceSpans = parseFenceSpans(buffer);

// 只在安全位置分割(代码块外)
if (isSafeFenceBreak(fenceSpans, candidate)) {
  return { index: candidate };
}

// 如果必须在代码块内分割 → 关闭+重开围栏
return {
  index: maxChars,
  fenceSplit: {
    closeFenceLine: "```",
    reopenFenceLine: "```typescript"
  }
};
```

---

## ☕ Java实现对照

### 1. 工具事件处理器

```java
@Component
@Slf4j
public class ToolEventHandler {
    
    private final AgentEventService eventService;
    private final Map<String, String> toolMetaById = new ConcurrentHashMap<>();
    
    public void handleStart(ToolExecutionEvent evt) {
        String toolName = normalizeToolName(evt.getToolName());
        String toolCallId = evt.getToolCallId();
        
        // 提取工具元信息
        String meta = inferToolMeta(toolName, evt.getArgs());
        toolMetaById.put(toolCallId, meta);
        
        eventService.emit(AgentEventPayload.builder()
            .runId(evt.getRunId())
            .stream("tool")
            .data(Map.of(
                "phase", "start",
                "name", toolName,
                "toolCallId", toolCallId,
                "args", evt.getArgs()
            ))
            .build());
    }
    
    public void handleUpdate(ToolExecutionEvent evt) {
        eventService.emit(AgentEventPayload.builder()
            .runId(evt.getRunId())
            .stream("tool")
            .data(Map.of(
                "phase", "update",
                "name", normalizeToolName(evt.getToolName()),
                "toolCallId", evt.getToolCallId(),
                "partialResult", evt.getPartialResult()
            ))
            .build());
    }
    
    public void handleEnd(ToolExecutionEvent evt) {
        String toolCallId = evt.getToolCallId();
        String meta = toolMetaById.remove(toolCallId);
        
        eventService.emit(AgentEventPayload.builder()
            .runId(evt.getRunId())
            .stream("tool")
            .data(Map.of(
                "phase", "result",
                "name", normalizeToolName(evt.getToolName()),
                "toolCallId", toolCallId,
                "meta", meta,
                "isError", evt.isError(),
                "result", evt.getResult()
            ))
            .build());
    }
}
```

### 2. 块分割器

```java
@Slf4j
public class BlockChunker {
    
    private final StringBuilder buffer = new StringBuilder();
    private final int minChars;
    private final int maxChars;
    private final BreakPreference breakPreference;
    
    public enum BreakPreference {
        PARAGRAPH, NEWLINE, SENTENCE
    }
    
    public void append(String text) {
        if (text != null && !text.isEmpty()) {
            buffer.append(text);
        }
    }
    
    public void drain(boolean force, Consumer<String> emit) {
        while (buffer.length() >= minChars || (force && buffer.length() > 0)) {
            int breakIndex = pickBreakIndex(force);
            if (breakIndex <= 0) {
                if (force && buffer.length() > 0) {
                    emit.accept(buffer.toString());
                    buffer.setLength(0);
                }
                return;
            }
            
            String chunk = buffer.substring(0, breakIndex);
            if (!chunk.trim().isEmpty()) {
                emit.accept(chunk);
            }
            buffer.delete(0, breakIndex);
            stripLeadingNewlines();
        }
    }
    
    private int pickBreakIndex(boolean force) {
        String text = buffer.toString();
        int window = Math.min(maxChars, text.length());
        
        // 1. 段落边界
        if (breakPreference == BreakPreference.PARAGRAPH) {
            int idx = text.lastIndexOf("\n\n", window);
            if (idx >= minChars && isSafeBreak(idx)) return idx;
        }
        
        // 2. 换行边界
        if (breakPreference != BreakPreference.SENTENCE) {
            int idx = text.lastIndexOf("\n", window);
            if (idx >= minChars && isSafeBreak(idx)) return idx;
        }
        
        // 3. 句尾边界
        Matcher m = Pattern.compile("[.!?](?=\\s|$)").matcher(text.substring(0, window));
        int sentenceIdx = -1;
        while (m.find()) {
            if (m.end() >= minChars && isSafeBreak(m.end())) {
                sentenceIdx = m.end();
            }
        }
        if (sentenceIdx >= minChars) return sentenceIdx;
        
        // 4. 硬分割
        if (text.length() >= maxChars) return maxChars;
        
        return -1;
    }
    
    private boolean isSafeBreak(int index) {
        // 检查是否在代码块内
        return !isInsideFenceBlock(buffer.toString(), index);
    }
    
    public void reset() {
        buffer.setLength(0);
    }
}
```

### 3. 流式响应处理服务

```java
@Service
@Slf4j
public class StreamResponseHandler {
    
    private final AgentEventService eventService;
    private final ToolEventHandler toolHandler;
    
    public void handleEvent(AgentEvent event, StreamContext ctx) {
        switch (event.getType()) {
            case "message_start":
                handleMessageStart(ctx, event);
                break;
            case "message_update":
                handleMessageUpdate(ctx, event);
                break;
            case "message_end":
                handleMessageEnd(ctx, event);
                break;
            case "tool_execution_start":
                toolHandler.handleStart(event);
                break;
            case "tool_execution_update":
                toolHandler.handleUpdate(event);
                break;
            case "tool_execution_end":
                toolHandler.handleEnd(event);
                break;
        }
    }
    
    private void handleMessageUpdate(StreamContext ctx, AgentEvent event) {
        String delta = event.getDelta();
        ctx.getDeltaBuffer().append(delta);
        ctx.getBlockChunker().append(delta);
        
        // 发布增量更新
        String cleaned = stripTags(ctx.getDeltaBuffer().toString());
        eventService.emit(AgentEventPayload.builder()
            .runId(ctx.getRunId())
            .stream("assistant")
            .data(Map.of("text", cleaned, "delta", delta))
            .build());
        
        // 块分割排水
        ctx.getBlockChunker().drain(false, chunk -> 
            ctx.getOnBlockReply().accept(chunk));
    }
    
    private void handleMessageEnd(StreamContext ctx, AgentEvent event) {
        // 强制排水
        ctx.getBlockChunker().drain(true, chunk -> 
            ctx.getOnBlockReply().accept(chunk));
        
        // 重置状态
        ctx.getDeltaBuffer().setLength(0);
        ctx.getBlockChunker().reset();
    }
}
```

---

## ✅ 学习检查点

- [x] 理解工具事件三阶段(start/update/end)
- [x] 理解消息流式处理(delta增量)
- [x] 理解块分割算法(段落/换行/句尾优先)
- [x] 理解代码块安全保护机制
- [x] 能够用Java实现工具事件处理
- [x] 能够用Java实现块分割器

---

## 🔗 关键源文件链接

- [handlers.tools.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-embedded-subscribe.handlers.tools.ts) - 工具事件
- [handlers.messages.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-embedded-subscribe.handlers.messages.ts) - 消息事件
- [block-chunker.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-embedded-block-chunker.ts) - 块分割

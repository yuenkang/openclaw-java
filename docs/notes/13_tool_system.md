# 工具系统学习笔记

> 第二阶段:Agent工具实现和策略管理

## 📁 核心文件结构

```
src/agents/
├── pi-tools.ts              # Coding工具集 (454行)
├── bash-tools.ts            # Bash工具入口
├── bash-tools.exec.ts       # 命令执行 (1631行) ★
├── bash-tools.process.ts    # 进程管理
├── openclaw-tools.ts        # OpenClaw工具集 (171行)
└── tools/                   # 具体工具实现
    ├── browser-tool.js
    ├── canvas-tool.js
    ├── nodes-tool.js
    ├── message-tool.js
    └── ...
```

---

## 🔧 工具体系架构

```
┌────────────────────────────────────────────────────────────┐
│                    createOpenClawTools()                    │
├────────────────────────────────────────────────────────────┤
│  内置工具 (16+)              │  插件工具                    │
│  ├── browser-tool           │  ├── resolvePluginTools()   │
│  ├── canvas-tool            │  └── 动态加载                │
│  ├── nodes-tool             │                              │
│  ├── message-tool           │                              │
│  ├── cron-tool              │                              │
│  ├── gateway-tool           │                              │
│  ├── agents-list-tool       │                              │
│  ├── sessions-list-tool     │                              │
│  ├── sessions-history-tool  │                              │
│  ├── sessions-send-tool     │                              │
│  ├── sessions-spawn-tool    │                              │
│  ├── session-status-tool    │                              │
│  ├── web-search-tool        │                              │
│  ├── web-fetch-tool         │                              │
│  ├── image-tool             │                              │
│  └── tts-tool               │                              │
└────────────────────────────────────────────────────────────┘
                              +
┌────────────────────────────────────────────────────────────┐
│               createOpenClawCodingTools()                   │
├────────────────────────────────────────────────────────────┤
│  ├── read-tool              (文件读取)                      │
│  ├── write-tool             (文件写入)                      │
│  ├── edit-tool              (文件编辑)                      │
│  ├── exec-tool              (命令执行)                      │
│  └── process-tool           (进程管理)                      │
└────────────────────────────────────────────────────────────┘
```

---

## ⚡ 命令执行工具 (bash-tools.exec)

### 核心配置

```typescript
type ExecToolDefaults = {
  host?: "local" | "sandbox" | "node";
  security?: "sandboxed" | "elevated" | "full";
  ask?: "off" | "on-miss" | "always";
  sandbox?: BashSandboxConfig;
  elevated?: ExecElevatedDefaults;
  timeoutSec?: number;
  backgroundMs?: number;
  allowBackground?: boolean;
  notifyOnExit?: boolean;
  cwd?: string;
  pathPrepend?: string[];
  safeBins?: string[];
};
```

### 执行流程

```
1. 解析命令参数
   ↓
2. 确定执行主机 (local/sandbox/node)
   ↓
3. 安全检查 (环境变量/路径)
   ↓
4. 需要审批? → 等待用户确认
   ↓
5. 创建进程 (pty/spawn)
   ↓
6. 监控输出和退出状态
   ↓
7. 返回执行结果
```

### 执行结果类型

```typescript
type ExecToolDetails =
  | { status: "running"; sessionId: string; pid?: number; tail?: string }
  | { status: "completed"|"failed"; exitCode: number; aggregated: string }
  | { status: "approval-pending"; approvalId: string; expiresAtMs: number };
```

---

## 🎨 Canvas工具

```typescript
// 用于AI画图和视觉内容生成
createCanvasTool(): AgentTool
```

---

## 🌐 Browser工具

```typescript
createBrowserTool(options: {
  sandboxBridgeUrl?: string;    // 沙箱浏览器桥接URL
  allowHostControl?: boolean;   // 是否允许主机浏览器控制
}): AgentTool
```

---

## 📡 Nodes工具

```typescript
createNodesTool(options: {
  agentSessionKey?: string;
  config?: OpenClawConfig;
}): AgentTool

// 用于管理分布式节点
// - 列出可用节点
// - 在特定节点执行命令
// - 节点状态查询
```

---

## ☕ Java实现对照

### 1. 工具接口定义

```java
public interface AgentTool {
    
    String getName();
    
    String getDescription();
    
    JsonSchema getParameterSchema();
    
    CompletableFuture<ToolResult> execute(ToolContext context);
}

@Data
@Builder
public class ToolResult {
    private boolean success;
    private String output;
    private Object data;
    private String error;
}

@Data
@Builder
public class ToolContext {
    private JsonNode parameters;
    private String sessionKey;
    private OpenClawConfig config;
    private AbortSignal abortSignal;
}
```

### 2. 工具注册表

```java
@Component
public class ToolRegistry {
    
    private final Map<String, AgentTool> tools = new ConcurrentHashMap<>();
    
    public void register(AgentTool tool) {
        tools.put(tool.getName(), tool);
    }
    
    public Optional<AgentTool> get(String name) {
        return Optional.ofNullable(tools.get(name));
    }
    
    public List<AgentTool> listAll() {
        return new ArrayList<>(tools.values());
    }
    
    public List<ToolDefinition> toDefinitions() {
        return tools.values().stream()
            .map(t -> ToolDefinition.builder()
                .name(t.getName())
                .description(t.getDescription())
                .parameters(t.getParameterSchema())
                .build())
            .collect(Collectors.toList());
    }
}
```

### 3. 命令执行工具

```java
@Component
public class ExecTool implements AgentTool {
    
    private final ProcessRegistry processRegistry;
    private final ApprovalService approvalService;
    
    @Override
    public String getName() { return "exec"; }
    
    @Override
    public CompletableFuture<ToolResult> execute(ToolContext ctx) {
        String command = ctx.getParameters().get("command").asText();
        String workdir = Optional.ofNullable(ctx.getParameters().get("workdir"))
            .map(JsonNode::asText)
            .orElse(ctx.getConfig().getCwd());
        
        // 安全检查
        validateCommand(command);
        
        // 创建进程
        ProcessBuilder pb = new ProcessBuilder("sh", "-c", command)
            .directory(new File(workdir))
            .redirectErrorStream(true);
        
        // 配置环境
        Map<String, String> env = pb.environment();
        applyPathPrepend(env, config.getPathPrepend());
        
        Process process = pb.start();
        String sessionId = processRegistry.register(process);
        
        // 监控输出
        return readOutputAsync(process, sessionId)
            .thenApply(output -> ToolResult.builder()
                .success(process.exitValue() == 0)
                .output(output)
                .data(Map.of(
                    "status", process.exitValue() == 0 ? "completed" : "failed",
                    "exitCode", process.exitValue(),
                    "sessionId", sessionId
                ))
                .build());
    }
}
```

### 4. 浏览器工具

```java
@Component
public class BrowserTool implements AgentTool {
    
    private final PlaywrightService playwright;
    
    @Override
    public String getName() { return "browser"; }
    
    @Override
    public CompletableFuture<ToolResult> execute(ToolContext ctx) {
        String action = ctx.getParameters().get("action").asText();
        
        return switch (action) {
            case "navigate" -> navigate(ctx);
            case "click" -> click(ctx);
            case "type" -> type(ctx);
            case "screenshot" -> screenshot(ctx);
            default -> CompletableFuture.failedFuture(
                new UnsupportedOperationException(action));
        };
    }
    
    private CompletableFuture<ToolResult> navigate(ToolContext ctx) {
        String url = ctx.getParameters().get("url").asText();
        return playwright.navigate(url)
            .thenApply(page -> ToolResult.builder()
                .success(true)
                .output("Navigated to " + url)
                .build());
    }
}
```

### 5. 工具集创建器

```java
@Service
public class OpenClawToolFactory {
    
    private final ExecTool execTool;
    private final BrowserTool browserTool;
    private final CanvasTool canvasTool;
    private final NodesTool nodesTool;
    private final MessageTool messageTool;
    private final PluginToolResolver pluginResolver;
    
    public List<AgentTool> createTools(ToolOptions options) {
        List<AgentTool> tools = new ArrayList<>();
        
        // 内置工具
        tools.add(execTool);
        tools.add(browserTool);
        tools.add(canvasTool);
        tools.add(nodesTool);
        
        if (!options.isDisableMessageTool()) {
            tools.add(messageTool);
        }
        
        // 插件工具
        Set<String> existingNames = tools.stream()
            .map(AgentTool::getName)
            .collect(Collectors.toSet());
            
        tools.addAll(pluginResolver.resolveTools(
            options.getPluginAllowlist(),
            existingNames
        ));
        
        return tools;
    }
}
```

---

## ✅ 学习检查点

- [x] 理解工具体系架构(两层:OpenClaw+Coding)
- [x] 理解命令执行工具的安全机制
- [x] 理解工具执行上下文和结果格式
- [x] 理解插件工具动态加载
- [x] 能够用Java实现工具接口和注册表

---

## 🔗 关键源文件链接

- [pi-tools.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/pi-tools.ts) - Coding工具
- [bash-tools.exec.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/bash-tools.exec.ts) - 命令执行
- [openclaw-tools.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/openclaw-tools.ts) - OpenClaw工具

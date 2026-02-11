# 配置管理学习笔记

> 第一阶段第四课:配置加载与热重载机制

## 📁 核心文件结构

```
src/config/
├── config.ts             # 导出入口 (15行)
├── io.ts                 # 配置I/O核心 (615行)
├── schema.ts             # Zod验证Schema (55KB)
├── defaults.ts           # 默认值应用 (12KB)
├── includes.ts           # $include指令解析
├── env-substitution.ts   # 环境变量替换
├── validation.ts         # 验证逻辑 (11KB)
└── types.*.ts           # 各模块类型定义

src/gateway/
├── config-reload.ts      # 热重载核心 (389行)
└── server-reload-handlers.ts # 重载处理器
```

---

## 🔄 配置加载管道

```
┌─────────────────────────────────────────────────────────────┐
│                    Config Loading Pipeline                   │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 1. Read config file (JSON5)                                 │
│    ~/.openclaw/config.json5                                 │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. Resolve $include directives                              │
│    { "$include": "./providers.json5" }                      │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. Apply config.env to process.env                         │
│    config.env.OPENAI_API_KEY → process.env                 │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. Substitute ${VAR} env references                         │
│    "apiKey": "${OPENAI_API_KEY}"                            │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. Validate with Zod schema                                 │
│    validateConfigObjectWithPlugins()                        │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. Apply defaults (layered)                                 │
│    applyModelDefaults → applyAgentDefaults → ...            │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 7. Normalize paths                                          │
│    normalizeConfigPaths()                                   │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 8. Cache result (200ms default)                             │
│    configCache = { config, expiresAt }                      │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔥 热重载机制

### 1. 重载模式 (GatewayReloadMode)

| 模式 | 说明 |
|------|------|
| `off` | 禁用热重载 |
| `hot` | 仅热重载,忽略需要重启的变更 |
| `restart` | 所有变更都触发重启 |
| `hybrid` | **默认** 热重载优先,必要时重启 |

### 2. 重载规则 (ReloadRule)

```typescript
type ReloadRule = {
  prefix: string;                    // 配置路径前缀
  kind: "restart" | "hot" | "none";  // 重载类型
  actions?: ReloadAction[];          // 热重载动作
};
```

**规则表:**

| 前缀 | 类型 | 动作 |
|------|------|------|
| `gateway.remote` | none | - |
| `hooks.gmail` | hot | restart-gmail-watcher |
| `hooks` | hot | reload-hooks |
| `cron` | hot | restart-cron |
| `browser` | hot | restart-browser-control |
| `telegram` | hot | restart-channel:telegram |
| `identity` | none | - |
| `agents` | none | - |
| `gateway` | restart | - |
| `plugins` | restart | - |

### 3. 热重载计划 (GatewayReloadPlan)

```typescript
type GatewayReloadPlan = {
  changedPaths: string[];        // 变更的配置路径
  restartGateway: boolean;       // 是否需要重启
  restartReasons: string[];      // 重启原因
  hotReasons: string[];          // 热重载原因
  reloadHooks: boolean;          // 重新加载Hooks
  restartCron: boolean;          // 重启定时任务
  restartChannels: Set<ChannelId>; // 重启的渠道
  noopPaths: string[];           // 无需处理的路径
};
```

### 4. 配置差异检测

```typescript
// config-reload.ts L138-165
function diffConfigPaths(prev: unknown, next: unknown, prefix = ""): string[] {
  if (prev === next) return [];
  
  if (isPlainObject(prev) && isPlainObject(next)) {
    const keys = new Set([...Object.keys(prev), ...Object.keys(next)]);
    const paths: string[] = [];
    for (const key of keys) {
      const childPrefix = prefix ? `${prefix}.${key}` : key;
      const childPaths = diffConfigPaths(prev[key], next[key], childPrefix);
      paths.push(...childPaths);
    }
    return paths;
  }
  
  return [prefix || "<root>"];
}
```

### 5. 文件监听 (chokidar)

```typescript
// config-reload.ts L358-375
const watcher = chokidar.watch(opts.watchPath, {
  ignoreInitial: true,
  awaitWriteFinish: { stabilityThreshold: 200, pollInterval: 50 },
});

watcher.on("add", schedule);
watcher.on("change", schedule);
watcher.on("unlink", schedule);
```

---

## 💾 配置缓存

```typescript
// io.ts L550-604
const DEFAULT_CONFIG_CACHE_MS = 200;
let configCache: {
  configPath: string;
  expiresAt: number;
  config: OpenClawConfig;
} | null = null;

export function loadConfig(): OpenClawConfig {
  const now = Date.now();
  if (cached && cached.expiresAt > now) {
    return cached.config;  // 返回缓存
  }
  const config = io.loadConfig();
  configCache = { configPath, expiresAt: now + cacheMs, config };
  return config;
}
```

---

## ☕ Java实现对照

### 1. 配置实体

```java
@Data
@ConfigurationProperties(prefix = "openclaw")
public class OpenClawConfig {
    private GatewayConfig gateway;
    private AgentsConfig agents;
    private SessionConfig session;
    private Map<String, Object> models;
    // ...
}

@Data
public class GatewayConfig {
    private AuthConfig auth;
    private ReloadConfig reload;
    private Integer port;
    private String host;
}

@Data
public class ReloadConfig {
    private String mode = "hybrid";  // off, hot, restart, hybrid
    private Integer debounceMs = 300;
}
```

### 2. 配置服务

```java
@Service
@Slf4j
public class ConfigService {
    
    private final Path configPath;
    private volatile OpenClawConfig cachedConfig;
    private volatile long cacheExpiresAt;
    private static final long CACHE_MS = 200;
    
    public ConfigService() {
        this.configPath = resolveConfigPath();
    }
    
    public OpenClawConfig loadConfig() {
        long now = System.currentTimeMillis();
        if (cachedConfig != null && cacheExpiresAt > now) {
            return cachedConfig;
        }
        
        OpenClawConfig config = doLoadConfig();
        cachedConfig = config;
        cacheExpiresAt = now + CACHE_MS;
        return config;
    }
    
    private OpenClawConfig doLoadConfig() {
        try {
            // 1. 读取文件
            String raw = Files.readString(configPath);
            
            // 2. 解析JSON5 (需要第三方库)
            ObjectMapper mapper = new ObjectMapper();
            Map<String, Object> parsed = mapper.readValue(raw, Map.class);
            
            // 3. 解析$include
            parsed = resolveIncludes(parsed);
            
            // 4. 环境变量替换
            parsed = substituteEnvVars(parsed);
            
            // 5. 验证
            validateConfig(parsed);
            
            // 6. 应用默认值
            applyDefaults(parsed);
            
            // 7. 转换为对象
            return mapper.convertValue(parsed, OpenClawConfig.class);
            
        } catch (Exception e) {
            log.error("Failed to load config", e);
            return new OpenClawConfig();  // 返回默认配置
        }
    }
    
    private Map<String, Object> substituteEnvVars(Map<String, Object> config) {
        // 递归替换 ${VAR} 模式
        return transformValues(config, value -> {
            if (value instanceof String s) {
                return resolveEnvString(s);
            }
            return value;
        });
    }
    
    private String resolveEnvString(String value) {
        Pattern pattern = Pattern.compile("\\$\\{([^}]+)}");
        Matcher matcher = pattern.matcher(value);
        StringBuilder result = new StringBuilder();
        while (matcher.find()) {
            String envVar = matcher.group(1);
            String envValue = System.getenv(envVar);
            matcher.appendReplacement(result, 
                envValue != null ? envValue : "");
        }
        matcher.appendTail(result);
        return result.toString();
    }
}
```

### 3. 热重载服务

```java
@Service
@Slf4j
public class ConfigReloadService {
    
    private final ConfigService configService;
    private final List<ReloadRule> reloadRules;
    private WatchService watchService;
    private volatile OpenClawConfig currentConfig;
    private ScheduledExecutorService scheduler;
    private volatile ScheduledFuture<?> debounceTask;
    
    @PostConstruct
    public void start() throws IOException {
        this.currentConfig = configService.loadConfig();
        this.reloadRules = initReloadRules();
        this.scheduler = Executors.newSingleThreadScheduledExecutor();
        startWatching();
    }
    
    private void startWatching() throws IOException {
        watchService = FileSystems.getDefault().newWatchService();
        Path configDir = configService.getConfigPath().getParent();
        configDir.register(watchService, 
            StandardWatchEventKinds.ENTRY_CREATE,
            StandardWatchEventKinds.ENTRY_MODIFY,
            StandardWatchEventKinds.ENTRY_DELETE);
        
        Thread watchThread = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    WatchKey key = watchService.take();
                    for (WatchEvent<?> event : key.pollEvents()) {
                        scheduleReload();
                    }
                    key.reset();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
        watchThread.setDaemon(true);
        watchThread.start();
    }
    
    private void scheduleReload() {
        if (debounceTask != null) {
            debounceTask.cancel(false);
        }
        int debounceMs = currentConfig.getGateway()
            .getReload().getDebounceMs();
        debounceTask = scheduler.schedule(
            this::runReload, debounceMs, TimeUnit.MILLISECONDS);
    }
    
    private void runReload() {
        try {
            OpenClawConfig nextConfig = configService.loadConfig();
            List<String> changedPaths = diffConfigPaths(currentConfig, nextConfig);
            
            if (changedPaths.isEmpty()) return;
            
            log.info("Config change detected: {}", changedPaths);
            GatewayReloadPlan plan = buildReloadPlan(changedPaths);
            
            String mode = currentConfig.getGateway().getReload().getMode();
            if ("off".equals(mode)) {
                log.info("Config reload disabled");
                return;
            }
            
            if (plan.isRestartGateway()) {
                if ("hot".equals(mode)) {
                    log.warn("Restart required but mode=hot, ignoring");
                    return;
                }
                onRestart(plan, nextConfig);
            } else {
                onHotReload(plan, nextConfig);
            }
            
            currentConfig = nextConfig;
        } catch (Exception e) {
            log.error("Config reload failed", e);
        }
    }
    
    private List<String> diffConfigPaths(Object prev, Object next) {
        return diffConfigPaths(prev, next, "");
    }
    
    private List<String> diffConfigPaths(Object prev, Object next, String prefix) {
        if (Objects.equals(prev, next)) {
            return Collections.emptyList();
        }
        
        if (prev instanceof Map && next instanceof Map) {
            Map<?,?> prevMap = (Map<?,?>) prev;
            Map<?,?> nextMap = (Map<?,?>) next;
            Set<Object> keys = new HashSet<>();
            keys.addAll(prevMap.keySet());
            keys.addAll(nextMap.keySet());
            
            List<String> paths = new ArrayList<>();
            for (Object key : keys) {
                String childPrefix = prefix.isEmpty() 
                    ? key.toString() 
                    : prefix + "." + key;
                paths.addAll(diffConfigPaths(
                    prevMap.get(key), nextMap.get(key), childPrefix));
            }
            return paths;
        }
        
        return List.of(prefix.isEmpty() ? "<root>" : prefix);
    }
    
    private GatewayReloadPlan buildReloadPlan(List<String> changedPaths) {
        GatewayReloadPlan plan = new GatewayReloadPlan();
        plan.setChangedPaths(changedPaths);
        
        for (String path : changedPaths) {
            ReloadRule rule = matchRule(path);
            if (rule == null) {
                plan.setRestartGateway(true);
                plan.getRestartReasons().add(path);
            } else if (rule.getKind() == ReloadKind.RESTART) {
                plan.setRestartGateway(true);
                plan.getRestartReasons().add(path);
            } else if (rule.getKind() == ReloadKind.NONE) {
                plan.getNoopPaths().add(path);
            } else {
                plan.getHotReasons().add(path);
                applyActions(plan, rule.getActions());
            }
        }
        
        return plan;
    }
}
```

---

## ✅ 学习检查点

- [x] 理解配置加载管道 (8步)
- [x] 理解$include和环境变量替换
- [x] 理解热重载规则系统
- [x] 理解配置差异检测
- [x] 理解文件监听和防抖
- [x] 能够用Java实现配置加载和热重载

---

## 📚 下一步

1. **事件路由** - Agent事件如何路由到客户端

---

## 🔗 关键源文件链接

- [io.ts](https://github.com/openclaw/openclaw/blob/main/src/config/io.ts) - 配置I/O
- [config-reload.ts](https://github.com/openclaw/openclaw/blob/main/src/gateway/config-reload.ts) - 热重载
- [validation.ts](https://github.com/openclaw/openclaw/blob/main/src/config/validation.ts) - 验证

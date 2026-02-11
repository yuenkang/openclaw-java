# 配置管理与插件系统学习笔记

> 第二阶段:配置层级和插件加载机制

## 📁 核心文件结构

```
src/config/
├── io.ts              # 配置加载/缓存 (615行)
├── defaults.ts        # 默认值应用 (470行)
├── schema.ts          # 配置Schema (55112字节)
├── validation.ts      # 配置验证
├── env-substitution.ts # 环境变量替换
├── includes.ts        # include处理
└── types.*.ts         # 分类型定义

src/plugins/
├── loader.ts          # 插件加载 (454行)
├── registry.ts        # 插件注册表 (516行)
├── discovery.ts       # 插件发现
├── hooks.ts           # 钩子管理
└── types.ts           # 插件类型
```

---

## ⚙️ 配置加载流程

```
┌─────────────────────────────────────────────────────────────┐
│                    loadConfig() 流程                         │
├─────────────────────────────────────────────────────────────┤
│  1. 解析配置路径 (OPENCLAW_CONFIG_PATH / 默认路径)           │
│     ↓                                                        │
│  2. 读取JSON5文件                                            │
│     ↓                                                        │
│  3. 处理includes (支持相对/绝对路径)                         │
│     ↓                                                        │
│  4. 环境变量替换 (${VAR} / ${VAR:-default})                 │
│     ↓                                                        │
│  5. 应用默认值 (applyModelDefaults/applySessionDefaults等)  │
│     ↓                                                        │
│  6. 验证配置 (validateConfigObjectWithPlugins)              │
│     ↓                                                        │
│  7. 缓存配置 (默认200ms TTL)                                │
└─────────────────────────────────────────────────────────────┘
```

### 核心函数

```typescript
// 创建ConfigIO实例
function createConfigIO(overrides?: ConfigIoDeps) {
  return {
    loadConfig(): OpenClawConfig,
    readConfigFileSnapshot(): Promise<ConfigFileSnapshot>,
    writeConfigFile(cfg: OpenClawConfig): Promise<void>,
  };
}

// 默认值应用链
cfg = applyMessageDefaults(cfg);
cfg = applySessionDefaults(cfg);
cfg = applyModelDefaults(cfg);
cfg = applyAgentDefaults(cfg);
cfg = applyLoggingDefaults(cfg);
cfg = applyContextPruningDefaults(cfg);
cfg = applyCompactionDefaults(cfg);
```

---

## 🔌 插件系统架构

```
┌─────────────────────────────────────────────────────────────┐
│                   PluginRegistry                             │
├─────────────────────────────────────────────────────────────┤
│  plugins: PluginRecord[]          元数据                     │
│  tools: PluginToolRegistration[]  Agent工具                  │
│  hooks: PluginHookRegistration[]  事件钩子                   │
│  channels: PluginChannelRegistration[]  渠道插件             │
│  providers: PluginProviderRegistration[] 模型提供者          │
│  httpHandlers: PluginHttpRegistration[]  HTTP处理器          │
│  httpRoutes: PluginHttpRouteRegistration[] HTTP路由          │
│  cliRegistrars: PluginCliRegistration[]  CLI命令             │
│  services: PluginServiceRegistration[]  后台服务             │
│  commands: PluginCommandRegistration[]  Agent命令            │
│  diagnostics: PluginDiagnostic[]  诊断信息                   │
└─────────────────────────────────────────────────────────────┘
```

### 插件加载流程

```typescript
function loadOpenClawPlugins(options: PluginLoadOptions): PluginRegistry {
  // 1. 发现插件 (discovery)
  const plugins = discoverPlugins(config);
  
  // 2. 创建注册表
  const registry = createPluginRegistry({ logger, runtime });
  
  // 3. 动态加载每个插件 (jiti)
  for (const plugin of plugins) {
    const module = jiti.import(plugin.source);
    const { register } = resolvePluginModuleExport(module);
    
    // 4. 执行注册
    register(api);
  }
  
  return registry;
}
```

### 插件定义

```typescript
type OpenClawPluginDefinition = {
  name: string;
  version?: string;
  description?: string;
  configSchema?: Record<string, unknown>;
  register: (api: OpenClawPluginApi) => void;
};

// 注册API
interface OpenClawPluginApi {
  registerTool(tool: AgentTool | ToolFactory, opts?);
  registerHook(events: string[], handler, opts?);
  registerChannel(plugin: ChannelPlugin);
  registerProvider(provider: ProviderPlugin);
  registerHttpHandler(handler);
  registerHttpRoute(path, handler);
  registerCli(registrar);
  registerService(service);
  registerCommand(command);
}
```

---

## ☕ Java实现对照

### 1. 配置加载服务

```java
@Service
public class ConfigService {
    
    private final Cache<String, OpenClawConfig> cache = Caffeine.newBuilder()
        .expireAfterWrite(Duration.ofMillis(200))
        .build();
    
    @Value("${openclaw.config.path:~/.openclaw/config.json5}")
    private String configPath;
    
    public OpenClawConfig loadConfig() {
        String resolvedPath = resolveConfigPath();
        
        return cache.get(resolvedPath, path -> {
            // 1. 读取文件
            String raw = Files.readString(Path.of(path));
            
            // 2. 解析JSON5
            JsonNode parsed = json5Parser.parse(raw);
            
            // 3. 处理includes
            parsed = processIncludes(parsed, path);
            
            // 4. 环境变量替换
            parsed = substituteEnvVars(parsed);
            
            // 5. 映射到配置对象
            OpenClawConfig config = objectMapper.treeToValue(parsed, OpenClawConfig.class);
            
            // 6. 应用默认值
            config = applyDefaults(config);
            
            // 7. 验证
            validate(config);
            
            return config;
        });
    }
    
    private OpenClawConfig applyDefaults(OpenClawConfig cfg) {
        cfg = applyMessageDefaults(cfg);
        cfg = applySessionDefaults(cfg);
        cfg = applyModelDefaults(cfg);
        cfg = applyAgentDefaults(cfg);
        return cfg;
    }
}
```

### 2. 插件注册表

```java
@Component
public class PluginRegistry {
    
    private final List<PluginRecord> plugins = new CopyOnWriteArrayList<>();
    private final List<PluginToolRegistration> tools = new CopyOnWriteArrayList<>();
    private final List<PluginHookRegistration> hooks = new CopyOnWriteArrayList<>();
    private final List<PluginChannelRegistration> channels = new CopyOnWriteArrayList<>();
    
    public void registerTool(PluginRecord record, AgentTool tool) {
        tools.add(PluginToolRegistration.builder()
            .pluginId(record.getId())
            .tool(tool)
            .names(List.of(tool.getName()))
            .source(record.getSource())
            .build());
        record.getToolNames().add(tool.getName());
    }
    
    public void registerHook(PluginRecord record, List<String> events, HookHandler handler) {
        hooks.add(PluginHookRegistration.builder()
            .pluginId(record.getId())
            .events(events)
            .handler(handler)
            .source(record.getSource())
            .build());
        record.setHookCount(record.getHookCount() + 1);
    }
    
    public List<AgentTool> getTools() {
        return tools.stream()
            .map(PluginToolRegistration::getTool)
            .collect(Collectors.toList());
    }
}
```

### 3. 插件加载器

```java
@Service
public class PluginLoader {
    
    private final PluginRegistry registry;
    private final PluginDiscovery discovery;
    
    public PluginRegistry loadPlugins(OpenClawConfig config) {
        // 1. 发现插件
        List<PluginSource> sources = discovery.discoverPlugins(config);
        
        for (PluginSource source : sources) {
            try {
                // 2. 加载插件类
                Class<?> pluginClass = loadPluginClass(source);
                
                // 3. 创建实例
                OpenClawPlugin plugin = (OpenClawPlugin) pluginClass
                    .getDeclaredConstructor()
                    .newInstance();
                
                // 4. 创建插件记录
                PluginRecord record = PluginRecord.builder()
                    .id(plugin.getId())
                    .name(plugin.getName())
                    .version(plugin.getVersion())
                    .source(source.getPath())
                    .enabled(true)
                    .build();
                
                // 5. 执行注册
                plugin.register(new PluginApiImpl(registry, record));
                
                registry.getPlugins().add(record);
            } catch (Exception e) {
                log.error("Failed to load plugin: {}", source.getPath(), e);
            }
        }
        
        return registry;
    }
}
```

### 4. 插件接口

```java
public interface OpenClawPlugin {
    
    String getId();
    
    String getName();
    
    default String getVersion() { return "1.0.0"; }
    
    default String getDescription() { return ""; }
    
    void register(OpenClawPluginApi api);
}

public interface OpenClawPluginApi {
    
    void registerTool(AgentTool tool);
    
    void registerHook(List<String> events, HookHandler handler);
    
    void registerChannel(ChannelPlugin plugin);
    
    void registerProvider(ProviderPlugin provider);
    
    void registerHttpRoute(String path, HttpRouteHandler handler);
    
    void registerCommand(CommandDefinition command);
}
```

---

## ✅ 学习检查点

- [x] 理解配置加载流程(JSON5→includes→env→defaults→validate)
- [x] 理解配置缓存机制(200ms TTL)
- [x] 理解PluginRegistry结构(10种注册类型)
- [x] 理解插件加载流程(discover→load→register)
- [x] 能够用Java实现配置和插件系统

---

## 🔗 关键源文件链接

- [io.ts](https://github.com/openclaw/openclaw/blob/main/src/config/io.ts) - 配置IO
- [defaults.ts](https://github.com/openclaw/openclaw/blob/main/src/config/defaults.ts) - 默认值
- [loader.ts](https://github.com/openclaw/openclaw/blob/main/src/plugins/loader.ts) - 插件加载
- [registry.ts](https://github.com/openclaw/openclaw/blob/main/src/plugins/registry.ts) - 插件注册

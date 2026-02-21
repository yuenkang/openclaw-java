# OpenClaw Plugin 开发指南

本文档介绍如何为 OpenClaw-Java 开发、安装和管理插件。

## 概述

OpenClaw 的插件系统允许你通过 Java JAR 包扩展平台能力，包括：

- **工具 (Tool)** — 为 AI Agent 添加可调用的自定义工具
- **钩子 (Hook)** — 在 Agent 生命周期事件中插入自定义逻辑
- **命令 (Command)** — 注册自定义斜杠命令（如 `/mycommand`）

## 快速开始

### 1. 创建插件项目

新建 Maven 项目，添加 `openclaw-plugin` 依赖：

```xml
<dependency>
    <groupId>com.openclaw</groupId>
    <artifactId>openclaw-plugin</artifactId>
    <version>1.0-SNAPSHOT</version>
    <scope>provided</scope>
</dependency>
```

### 2. 编写插件入口类

实现 `PluginTypes.PluginDefinition`，在 `register` 回调中注册能力：

```java
package com.example.myplugin;

import com.openclaw.plugin.PluginTypes;
import com.openclaw.plugin.PluginTypes.*;

public class MyPlugin extends PluginDefinition {
    public MyPlugin() {
        setName("my-plugin");
        setRegister(api -> {
            // 注册一个工具
            api.getToolRegistrar().accept(ToolRegistration.builder()
                    .name("my_tool")
                    .names(java.util.List.of("my_tool"))
                    .optional(false)
                    .build());

            // 注册一个钩子
            api.getHookRegistrar().accept(HookRegistration.builder()
                    .events(java.util.List.of("before_agent_start"))
                    .name("my-hook")
                    .description("My custom hook")
                    .build());

            // 注册一个斜杠命令
            api.getCommandRegistrar().accept(CommandRegistration.builder()
                    .name("greet")
                    .description("Say hello")
                    .acceptsArgs(true)
                    .requireAuth(false)
                    .build());

            api.getLogger().info("MyPlugin registered successfully!");
        });
    }
}
```

### 3. 编写插件清单

在项目资源目录 `src/main/resources/` 下创建 `openclaw.plugin.json`：

```json
{
  "id": "my-plugin",
  "name": "My Plugin",
  "version": "1.0.0",
  "description": "A sample OpenClaw plugin",
  "author": "Your Name",
  "entryClass": "com.example.myplugin.MyPlugin",
  "configSchema": {}
}
```

字段说明：

| 字段 | 必填 | 说明 |
|------|:----:|------|
| `id` | ✅ | 唯一标识符（小写字母 + 连字符） |
| `name` | ✅ | 人类可读名称 |
| `version` | ❌ | 语义化版本号（默认 `1.0.0`） |
| `description` | ❌ | 插件描述 |
| `author` | ❌ | 作者 |
| `entryClass` | ✅ | 实现 `PluginDefinition` 的完整类名 |
| `configSchema` | ❌ | JSON Schema 定义插件配置结构 |

### 4. 打包为 JAR

```bash
mvn clean package
```

确保 JAR 的根目录包含 `openclaw.plugin.json` 文件。

## 安装插件

### 方式一：JAR 安装

将 JAR 放入扩展目录（默认 `~/.openclaw/extensions/`），或通过命令安装：

```
/plugin install /path/to/my-plugin.jar
```

### 方式二：目录安装

将插件目录（包含 `openclaw.plugin.json`）放入扩展目录。

### 方式三：配置启用

在 `~/.openclaw/config.json` 中声明：

```json
{
  "plugins": {
    "entries": {
      "my-plugin": {
        "enabled": true,
        "config": {}
      }
    }
  }
}
```

## 插件能力详解

### 工具 (Tool)

插件注册的工具会自动注入 Agent 运行时。AI Agent 在对话中可以调用这些工具。

```java
// 通过 PluginApi 注册
api.getToolRegistrar().accept(ToolRegistration.builder()
        .name("weather")                    // 工具名（LLM 看到的名字）
        .names(List.of("weather"))          // 工具名列表
        .optional(false)                    // 是否可选（受 allowlist 控制）
        .build());
```

注册后，工具经过 `PluginToolResolver` 解析，通过 `PluginToolAdapter` 适配为 `AgentTool`，最终注入 `ToolRegistry`。

### 钩子 (Hook)

钩子在 Agent 生命周期的关键节点执行。支持的钩子事件：

| 钩子事件 | 类型 | 说明 |
|---------|------|------|
| `before_agent_start` | 修改型 | 在 Agent 启动前运行，可注入系统提示 |
| `agent_end` | 触发型 | Agent 对话结束后运行 |
| `before_compaction` | 触发型 | 历史压缩前运行 |
| `after_compaction` | 触发型 | 历史压缩后运行 |
| `message_received` | 触发型 | 收到用户消息时运行 |
| `message_sending` | 修改型 | 发送消息前运行，可修改或取消消息 |
| `message_sent` | 触发型 | 消息发送后运行 |
| `before_tool_call` | 修改型 | 工具调用前运行，可修改或阻止调用 |
| `after_tool_call` | 触发型 | 工具调用后运行 |
| `tool_result_persist` | 修改型 | 工具结果持久化时运行 |
| `session_start` | 触发型 | 会话开始时运行 |
| `session_end` | 触发型 | 会话结束时运行 |
| `gateway_start` | 触发型 | Gateway 启动时运行 |
| `gateway_stop` | 触发型 | Gateway 停止时运行 |

**触发型钩子** 并行执行，不返回结果。**修改型钩子** 按优先级顺序执行，结果会合并。

```java
api.getHookRegistrar().accept(HookRegistration.builder()
        .events(List.of("before_agent_start", "agent_end"))
        .name("my-analytics")
        .description("Track agent usage")
        .build());
```

### 命令 (Command)

注册自定义斜杠命令，用户可以在聊天中直接调用。

```java
api.getCommandRegistrar().accept(CommandRegistration.builder()
        .name("ping")               // 命令名（用户输入 /ping）
        .description("Pong!")        // 命令描述（/help 中显示）
        .acceptsArgs(true)           // 是否接受参数
        .requireAuth(true)           // 是否需要授权
        .build());
```

**保留命令名**：以下名称被系统占用，不可注册：
`help` `commands` `status` `stop` `restart` `reset` `new` `compact`
`config` `debug` `allowlist` `activation` `skill` `subagents` `model`
`models` `queue` `send` `bash` `exec` `think` `verbose` `reasoning`
`elevated` `usage`

## PluginApi 参考

插件在 `register(api)` 回调中通过 `PluginApi` 对象与系统交互：

| 属性 | 类型 | 说明 |
|------|------|------|
| `api.getId()` | `String` | 插件 ID |
| `api.getSource()` | `String` | 插件来源路径 |
| `api.getLogger()` | `PluginLogger` | 日志器（debug/info/warn/error） |
| `api.getConfig()` | `OpenClawConfig` | 全局配置 |
| `api.getPluginConfig()` | `Map<String, Object>` | 插件私有配置 |
| `api.getHookRegistrar()` | `Consumer<HookRegistration>` | 注册钩子 |
| `api.getToolRegistrar()` | `Consumer<ToolRegistration>` | 注册工具 |
| `api.getCommandRegistrar()` | `Consumer<CommandRegistration>` | 注册命令 |

## 插件管理命令

在聊天中使用 `/plugin` 斜杠命令管理插件：

| 命令 | 说明 |
|------|------|
| `/plugin list` | 列出所有已加载插件及状态 |
| `/plugin info <id>` | 查看插件详情（版本/来源/注册的工具&钩子&命令） |
| `/plugin status` | 显示插件子系统状态报告 |
| `/plugin install <path>` | 安装 JAR 插件 |

## 插件加载流程

```
~/.openclaw/extensions/           PluginLoader
 └── my-plugin/                   ┌─────────────────────────────────┐
     ├── openclaw.plugin.json ──▶ │ 1. discover (PluginDiscovery)   │
     └── my-plugin.jar            │ 2. parse manifest               │
                                  │ 3. check enable state           │
                                  │ 4. validate config schema       │
                                  │ 5. activatePlugin()             │
                                  │    ├─ buildPluginClassLoader()  │
                                  │    ├─ loadClass(entryClass)     │
                                  │    ├─ buildPluginApi(registrars)│
                                  │    └─ definition.register(api)  │
                                  └─────────────────────────────────┘
                                           │
                              ┌────────────┼────────────┐
                              ▼            ▼            ▼
                        PluginRegistry  PluginCommand  PluginHook
                         .registerTool  Processor      Runner
                                        .register
```

## 插件配置 Schema

插件可以通过 `configSchema` 定义自己的配置结构，系统会在加载时进行 JSON Schema 验证：

```json
{
  "id": "my-plugin",
  "entryClass": "com.example.MyPlugin",
  "configSchema": {
    "type": "object",
    "properties": {
      "apiKey": { "type": "string" },
      "maxRetries": { "type": "integer", "default": 3 }
    },
    "required": ["apiKey"]
  }
}
```

用户在 `config.json` 中配置：

```json
{
  "plugins": {
    "entries": {
      "my-plugin": {
        "enabled": true,
        "config": {
          "apiKey": "sk-xxx",
          "maxRetries": 5
        }
      }
    }
  }
}
```

插件在 `register()` 中通过 `api.getPluginConfig()` 读取配置。

## 目录结构

插件系统的代码组织：

```
openclaw-plugin/src/main/java/com/openclaw/plugin/
├── PluginTypes.java              # 类型定义 (PluginApi/PluginDefinition/Registration)
├── cli/PluginCli.java            # CLI 注册桥接
├── commands/PluginCommandProcessor.java  # 命令注册/匹配/执行
├── config/
│   ├── PluginConfigSchema.java   # Schema 工厂 (SDK 工具类)
│   ├── PluginEnable.java         # 启用/禁用判定
│   └── PluginSchemaValidator.java # JSON Schema 验证
├── hooks/
│   ├── PluginHookRunner.java     # 14 种钩子执行器
│   └── PluginHookRunnerGlobal.java # 全局 HookRunner 单例
├── http/
│   ├── PluginHttpPath.java       # HTTP 路径规范化
│   └── PluginHttpRegistry.java   # HTTP 处理器注册
├── install/
│   ├── PluginInstallRecord.java  # 安装记录持久化
│   ├── PluginInstaller.java      # JAR/目录安装器
│   └── PluginUpdater.java        # 插件更新器
├── loader/
│   ├── PluginBundledDir.java     # 内置插件目录
│   ├── PluginConfigState.java    # 配置状态解析
│   ├── PluginDiscovery.java      # 插件发现 (扫描 extensions/)
│   ├── PluginLoader.java         # 核心加载器 (activate + classloader)
│   ├── PluginManifest.java       # 清单解析
│   └── PluginManifestRegistry.java # 清单注册表
├── providers/PluginProviderResolver.java  # Provider 解析
├── registry/PluginRegistry.java  # 注册表 (tools/hooks/commands)
├── runtime/PluginRuntime.java    # 全局 Registry 状态
├── services/PluginServiceManager.java # 后台服务管理
├── slots/PluginSlots.java        # 插槽系统
├── status/PluginStatus.java      # 状态报告
└── tools/PluginToolResolver.java # 工具解析注入 Agent
```

# TUI 使用指南

OpenClaw TUI 是一个终端交互界面，让你在终端中直接与 AI Agent 对话。

## 启动

```bash
# 先启动 Java 后端
cd openclaw-java && mvn spring-boot:run

# 再启动 TUI（在另一个终端）
cd openclaw && npx openclaw tui
```

## 基本操作

- 直接输入文字发送消息
- 以 `/` 开头输入斜杠命令
- 输入时支持命令自动补全（Tab 键）
- `Ctrl+C` 或 `/exit` 退出

## 状态栏

TUI 底部显示当前状态信息：

```
agent default (Default Agent) | session main | openai/claude-opus-4-6-thinking | tokens 1234
```

依次为：当前 Agent、Session 名称、模型、Token 用量。

## 斜杠命令

### 帮助与状态

| 命令 | 说明 |
|------|------|
| `/help` | 显示所有可用命令 |
| `/status` | 显示 Gateway 状态（系统信息、活跃 Session 等） |

### 模型管理

| 命令 | 说明 |
|------|------|
| `/model <provider/model>` | 设置当前 Session 使用的模型，如 `/model openai/gpt-4o` |
| `/model` | 不带参数，打开模型选择器（可搜索） |
| `/models` | 打开模型选择器 |

### Session 管理

| 命令 | 说明 |
|------|------|
| `/session <key>` | 切换到指定 Session，如 `/session test` |
| `/session` | 不带参数，打开 Session 选择器 |
| `/sessions` | 打开 Session 选择器 |
| `/new` | 重置当前 Session（清除聊天记录和 Token 计数） |
| `/reset` | 同 `/new` |

### Agent 管理

| 命令 | 说明 |
|------|------|
| `/agent <id>` | 切换到指定 Agent，如 `/agent coding` |
| `/agent` | 不带参数，打开 Agent 选择器 |
| `/agents` | 打开 Agent 选择器 |

### 思考与推理

| 命令 | 说明 |
|------|------|
| `/think <level>` | 设置思考级别（`off`、`low`、`medium`、`high`） |
| `/reasoning <on\|off>` | 开启或关闭推理模式 |

### 显示控制

| 命令 | 说明 |
|------|------|
| `/verbose <on\|off>` | 显示或隐藏工具调用详情 |
| `/usage <off\|tokens\|full>` | 设置每条回复后是否显示 Token 用量 |
| `/settings` | 打开设置面板（工具输出展开/折叠、思考显示等） |

### 权限控制

| 命令 | 说明 |
|------|------|
| `/elevated <on\|off\|ask\|full>` | 设置提权级别 |
| `/activation <mention\|always>` | 设置群组激活模式 |

### 其他

| 命令 | 说明 |
|------|------|
| `/abort` | 中止当前正在运行的任务 |
| `/exit` | 退出 TUI |
| `/quit` | 同 `/exit` |

## 快捷操作

- **模型选择器**中可以输入关键字搜索模型（如输入 `gemini` 过滤出 Gemini 系列模型）
- **Session 选择器**中可以搜索历史会话
- 所有选择器使用 **上下箭头** 选择，**Enter** 确认，**Esc** 取消

## 典型使用流程

```
# 1. 选择模型
/model openai/gemini-2.5-flash

# 2. 开始对话
你好，帮我写一个 Hello World 程序

# 3. 查看状态
/status

# 4. 清除历史，开始新对话
/reset

# 5. 切换到另一个模型
/models
```

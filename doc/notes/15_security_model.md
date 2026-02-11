# 安全模型学习笔记

> 第三阶段:DM配对、设备认证和沙箱安全

## 📁 核心文件结构

```
src/pairing/
├── pairing-store.ts      # DM配对存储 (497行)
├── pairing-messages.ts   # 配对消息
└── pairing-labels.ts     # 配对标签

src/infra/
├── device-pairing.ts     # 设备配对 (559行) ★
└── node-pairing.ts       # 节点配对

src/agents/sandbox/
├── types.ts              # 沙箱类型 (86行)
├── config.ts             # 沙箱配置解析
├── context.ts            # 沙箱上下文
├── docker.ts             # Docker参数
└── tool-policy.ts        # 工具策略
```

---

## 🔐 两层配对系统

### 1. DM配对 (pairing-store)

渠道用户首次DM机器人时的验证机制:

```
用户发消息 → 生成配对码 → 管理员审批 → 加入allowFrom
                ↓
      ┌────────────────────────┐
      │   PairingRequest       │
      │   id: string           │
      │   code: "ABC123"       │  ← 6位随机码
      │   createdAt: ISO时间   │
      │   meta: { name, ... }  │
      └────────────────────────┘
                ↓ 审批
      ┌────────────────────────┐
      │   AllowFromStore       │
      │   allowFrom: ["+123"]  │  ← 允许的发送者
      └────────────────────────┘
```

### 2. 设备配对 (device-pairing)

API/ACP客户端的认证机制:

```
设备请求配对 → 待审批 → 管理员审批 → 生成Token
     ↓                                    ↓
 ┌─────────────────┐     ┌─────────────────────────┐
 │ PendingRequest   │     │ PairedDevice             │
 │ deviceId         │     │ deviceId                 │
 │ publicKey        │     │ publicKey                │
 │ platform         │     │ role: "admin"|"user"     │
 │ role/scopes      │     │ roles: ["admin"]         │
 │ ts: 时间戳       │     │ scopes: ["read","write"] │
 │ TTL: 5分钟       │     │ tokens: {                │
 └─────────────────┘     │   admin: {               │
                          │     token: "abc...",      │
                          │     role, scopes,         │
                          │     createdAtMs,          │
                          │     rotatedAtMs,          │
                          │     revokedAtMs           │
                          │   }                      │
                          │ }                        │
                          └─────────────────────────┘
```

### Token生命周期

| 操作 | 函数 | 说明 |
|------|------|------|
| 创建/确保 | `ensureDeviceToken` | 不存在则创建 |
| 轮换 | `rotateDeviceToken` | 生成新token |
| 撤销 | `revokeDeviceToken` | 标记已撤销 |
| 验证 | `verifyDeviceToken` | 检查token+scope |

---

## 🏖️ 沙箱系统

### 配置类型

```typescript
type SandboxConfig = {
  mode: "off" | "non-main" | "all";  // 沙箱启用策略
  scope: "session" | "agent" | "shared"; // 隔离范围
  workspaceAccess: "none" | "ro" | "rw"; // 工作空间权限
  workspaceRoot: string;
  docker: SandboxDockerConfig;
  browser: SandboxBrowserConfig;
  tools: SandboxToolPolicy;      // 工具白/黑名单
  prune: SandboxPruneConfig;     // 清理策略
};

type SandboxToolPolicy = {
  allow?: string[];  // 工具白名单
  deny?: string[];   // 工具黑名单
};
```

### 沙箱浏览器

```typescript
type SandboxBrowserConfig = {
  enabled: boolean;
  image: string;          // Docker镜像
  cdpPort: number;        // Chrome DevTools端口
  vncPort: number;        // VNC端口
  headless: boolean;
  autoStart: boolean;
  allowHostControl: boolean;
};
```

---

## ☕ Java实现对照

### 1. 设备配对服务

```java
@Service
public class DevicePairingService {
    
    private final ConcurrentMap<String, PendingRequest> pending = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, PairedDevice> paired = new ConcurrentHashMap<>();
    private static final Duration PENDING_TTL = Duration.ofMinutes(5);
    
    public PendingRequest requestPairing(PairingInput input) {
        pruneExpiredPending();
        
        PendingRequest request = PendingRequest.builder()
            .requestId(UUID.randomUUID().toString())
            .deviceId(input.getDeviceId())
            .publicKey(input.getPublicKey())
            .role(input.getRole())
            .scopes(input.getScopes())
            .ts(Instant.now())
            .build();
        
        pending.put(request.getRequestId(), request);
        return request;
    }
    
    public PairedDevice approvePairing(String requestId) {
        PendingRequest req = pending.remove(requestId);
        if (req == null) return null;
        
        DeviceAuthToken token = DeviceAuthToken.builder()
            .token(generateToken())
            .role(req.getRole())
            .scopes(req.getScopes())
            .createdAtMs(Instant.now().toEpochMilli())
            .build();
        
        PairedDevice device = PairedDevice.builder()
            .deviceId(req.getDeviceId())
            .publicKey(req.getPublicKey())
            .tokens(Map.of(req.getRole(), token))
            .approvedAtMs(Instant.now().toEpochMilli())
            .build();
        
        paired.put(device.getDeviceId(), device);
        return device;
    }
    
    public VerifyResult verifyToken(String deviceId, String token, String role) {
        PairedDevice device = paired.get(deviceId);
        if (device == null) return VerifyResult.fail("device-not-paired");
        
        DeviceAuthToken entry = device.getTokens().get(role);
        if (entry == null) return VerifyResult.fail("token-missing");
        if (entry.getRevokedAtMs() != null) return VerifyResult.fail("token-revoked");
        if (!entry.getToken().equals(token)) return VerifyResult.fail("token-mismatch");
        
        entry.setLastUsedAtMs(Instant.now().toEpochMilli());
        return VerifyResult.ok();
    }
}
```

### 2. 沙箱管理

```java
@Service
public class SandboxService {
    
    private final DockerClient dockerClient;
    
    public SandboxContext createSandbox(SandboxConfig config, String sessionKey) {
        String containerName = buildContainerName(config.getScope(), sessionKey);
        
        CreateContainerCmd cmd = dockerClient.createContainerCmd(config.getDocker().getImage())
            .withName(containerName)
            .withNetworkMode("none");
        
        // 工作空间挂载
        if (config.getWorkspaceAccess() != WorkspaceAccess.NONE) {
            cmd.withBinds(new Bind(
                config.getWorkspaceRoot(),
                new Volume("/workspace"),
                config.getWorkspaceAccess() == WorkspaceAccess.RO 
                    ? AccessMode.ro : AccessMode.rw
            ));
        }
        
        CreateContainerResponse container = cmd.exec();
        dockerClient.startContainerCmd(container.getId()).exec();
        
        return SandboxContext.builder()
            .enabled(true)
            .sessionKey(sessionKey)
            .containerName(containerName)
            .workspaceAccess(config.getWorkspaceAccess())
            .build();
    }
    
    public boolean isToolAllowed(SandboxToolPolicy policy, String toolName) {
        if (policy.getDeny() != null && policy.getDeny().contains(toolName)) {
            return false;
        }
        if (policy.getAllow() != null) {
            return policy.getAllow().contains(toolName);
        }
        return true; // 默认允许
    }
}
```

---

## ✅ 学习检查点

- [x] 理解DM配对流程(配对码 → 审批 → allowFrom)
- [x] 理解设备配对和Token生命周期
- [x] 理解沙箱配置(mode/scope/docker/toolPolicy)
- [x] 能够用Java实现配对和沙箱服务

---

## 🔗 关键源文件链接

- [pairing-store.ts](https://github.com/openclaw/openclaw/blob/main/src/pairing/pairing-store.ts) - DM配对
- [device-pairing.ts](https://github.com/openclaw/openclaw/blob/main/src/infra/device-pairing.ts) - 设备配对
- [types.ts](https://github.com/openclaw/openclaw/blob/main/src/agents/sandbox/types.ts) - 沙箱类型

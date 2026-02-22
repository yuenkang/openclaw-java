package com.openclaw.node;

import java.util.List;

/**
 * Mobile-node detection utilities.
 * Corresponds to TypeScript's server-mobile-nodes.ts.
 */
public final class MobileNodes {

    private MobileNodes() {}

    /**
     * Check if a platform string represents a mobile OS (iOS or Android).
     */
    public static boolean isMobilePlatform(String platform) {
        if (platform == null) return false;
        String p = platform.trim().toLowerCase();
        if (p.isEmpty()) return false;
        return p.startsWith("ios") || p.startsWith("ipados") || p.startsWith("android");
    }

    /**
     * Check if any currently connected node is a mobile platform.
     */
    public static boolean hasConnectedMobileNode(NodeRegistry registry) {
        List<NodeRegistry.NodeSession> connected = registry.listConnected();
        return connected.stream().anyMatch(n -> isMobilePlatform(n.getPlatform()));
    }
}

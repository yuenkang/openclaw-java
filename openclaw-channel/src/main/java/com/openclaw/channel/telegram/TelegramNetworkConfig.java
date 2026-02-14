package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Telegram network configuration.
 * Resolves network-level settings like autoSelectFamily workarounds.
 * Corresponds to TypeScript's telegram/network-config.ts.
 */
@Slf4j
public class TelegramNetworkConfig {

    /**
     * Network decision result.
     */
    public record AutoSelectFamilyDecision(Boolean value, String source) {
    }

    /**
     * Resolve autoSelectFamily decision from config.
     * In Java (unlike Node.js), this is typically a no-op since Java handles
     * IPv4/IPv6 natively. Kept for config parity with TS.
     */
    public static AutoSelectFamilyDecision resolveAutoSelectFamilyDecision(
            Map<String, Object> networkConfig) {

        if (networkConfig == null) {
            return new AutoSelectFamilyDecision(null, null);
        }

        Object autoSelectFamily = networkConfig.get("autoSelectFamily");
        if (autoSelectFamily instanceof Boolean b) {
            return new AutoSelectFamilyDecision(b, "config");
        }

        // Check env
        String envVal = System.getenv("OPENCLAW_TELEGRAM_AUTO_SELECT_FAMILY");
        if (envVal != null) {
            boolean val = "true".equalsIgnoreCase(envVal) || "1".equals(envVal);
            return new AutoSelectFamilyDecision(val, "env");
        }

        return new AutoSelectFamilyDecision(null, null);
    }

    /**
     * Resolve connection timeout from config.
     */
    public static int resolveConnectTimeoutMs(Map<String, Object> networkConfig) {
        if (networkConfig == null)
            return 30_000;
        Object timeout = networkConfig.get("connectTimeoutMs");
        if (timeout instanceof Number n)
            return n.intValue();
        return 30_000;
    }

    /**
     * Resolve read timeout from config.
     */
    public static int resolveReadTimeoutMs(Map<String, Object> networkConfig) {
        if (networkConfig == null)
            return 60_000;
        Object timeout = networkConfig.get("readTimeoutMs");
        if (timeout instanceof Number n)
            return n.intValue();
        return 60_000;
    }
}

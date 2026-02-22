package com.openclaw.node;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for NodeCommandPolicy — platform detection, allowlist resolution,
 * and command permission checks.
 */
class NodeCommandPolicyTest {

    // ==================== Platform Normalization ====================

    @Test
    void normalizePlatform_ios() {
        assertEquals("ios", NodeCommandPolicy.normalizePlatformId("iOS 17.2", null));
        assertEquals("ios", NodeCommandPolicy.normalizePlatformId("ios", null));
    }

    @Test
    void normalizePlatform_android() {
        assertEquals("android", NodeCommandPolicy.normalizePlatformId("Android 14", null));
    }

    @Test
    void normalizePlatform_macos() {
        assertEquals("macos", NodeCommandPolicy.normalizePlatformId("macOS 14.3", null));
        assertEquals("macos", NodeCommandPolicy.normalizePlatformId("Darwin", null));
    }

    @Test
    void normalizePlatform_windows() {
        assertEquals("windows", NodeCommandPolicy.normalizePlatformId("Windows 11", null));
    }

    @Test
    void normalizePlatform_linux() {
        assertEquals("linux", NodeCommandPolicy.normalizePlatformId("Linux", null));
    }

    @Test
    void normalizePlatform_fromDeviceFamily() {
        assertEquals("ios", NodeCommandPolicy.normalizePlatformId("", "iPhone15,3"));
        assertEquals("macos", NodeCommandPolicy.normalizePlatformId(null, "MacBookPro"));
    }

    @Test
    void normalizePlatform_unknown() {
        assertEquals("unknown", NodeCommandPolicy.normalizePlatformId("", ""));
        assertEquals("unknown", NodeCommandPolicy.normalizePlatformId(null, null));
    }

    // ==================== Allowlist Resolution ====================

    @Test
    void resolveAllowlist_iosIncludesCanvasAndCamera() {
        Set<String> allowlist = NodeCommandPolicy.resolveAllowlist(null, "iOS", null);
        assertTrue(allowlist.contains("canvas.present"));
        assertTrue(allowlist.contains("camera.snap"));
        assertTrue(allowlist.contains("screen.record"));
        assertTrue(allowlist.contains("location.get"));
        // iOS should NOT have system.run by default
        assertFalse(allowlist.contains("system.run"));
    }

    @Test
    void resolveAllowlist_macosIncludesSystemCommands() {
        Set<String> allowlist = NodeCommandPolicy.resolveAllowlist(null, "macOS", null);
        assertTrue(allowlist.contains("system.run"));
        assertTrue(allowlist.contains("canvas.present"));
    }

    @Test
    void resolveAllowlist_linuxIsSystemOnly() {
        Set<String> allowlist = NodeCommandPolicy.resolveAllowlist(null, "Linux", null);
        assertTrue(allowlist.contains("system.run"));
        assertFalse(allowlist.contains("canvas.present"));
    }

    @Test
    void resolveAllowlist_configDenyOverride() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.GatewayConfig gw = new OpenClawConfig.GatewayConfig();
        OpenClawConfig.GatewayNodesConfig nodes = new OpenClawConfig.GatewayNodesConfig();
        nodes.setDenyCommands(List.of("system.run"));
        gw.setNodes(nodes);
        config.setGateway(gw);

        Set<String> allowlist = NodeCommandPolicy.resolveAllowlist(config, "macOS", null);
        assertFalse(allowlist.contains("system.run"));
        assertTrue(allowlist.contains("canvas.present"));
    }

    @Test
    void resolveAllowlist_configAllowAddsExtra() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.GatewayConfig gw = new OpenClawConfig.GatewayConfig();
        OpenClawConfig.GatewayNodesConfig nodes = new OpenClawConfig.GatewayNodesConfig();
        nodes.setAllowCommands(List.of("custom.command"));
        gw.setNodes(nodes);
        config.setGateway(gw);

        Set<String> allowlist = NodeCommandPolicy.resolveAllowlist(config, "iOS", null);
        assertTrue(allowlist.contains("custom.command"));
    }

    // ==================== Permission Check ====================

    @Test
    void isCommandAllowed_success() {
        Set<String> allowlist = Set.of("canvas.present", "camera.snap");
        List<String> declared = List.of("canvas.present", "camera.snap", "canvas.hide");

        var result = NodeCommandPolicy.isCommandAllowed("canvas.present", declared, allowlist);
        assertTrue(result.ok());
    }

    @Test
    void isCommandAllowed_notInAllowlist() {
        Set<String> allowlist = Set.of("canvas.present");
        List<String> declared = List.of("system.run");

        var result = NodeCommandPolicy.isCommandAllowed("system.run", declared, allowlist);
        assertFalse(result.ok());
        assertEquals("command not allowlisted", result.reason());
    }

    @Test
    void isCommandAllowed_notDeclaredByNode() {
        Set<String> allowlist = Set.of("canvas.present", "system.run");
        List<String> declared = List.of("canvas.present");

        var result = NodeCommandPolicy.isCommandAllowed("system.run", declared, allowlist);
        assertFalse(result.ok());
        assertEquals("command not declared by node", result.reason());
    }

    @Test
    void isCommandAllowed_emptyCommand() {
        var result = NodeCommandPolicy.isCommandAllowed("", List.of(), Set.of());
        assertFalse(result.ok());
        assertEquals("command required", result.reason());
    }

    @Test
    void isCommandAllowed_noDeclaredCommands() {
        Set<String> allowlist = Set.of("canvas.present");
        var result = NodeCommandPolicy.isCommandAllowed("canvas.present", List.of(), allowlist);
        assertFalse(result.ok());
        assertEquals("node did not declare commands", result.reason());
    }
}

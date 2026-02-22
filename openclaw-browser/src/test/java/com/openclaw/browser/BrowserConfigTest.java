package com.openclaw.browser;

import com.openclaw.common.config.OpenClawConfig;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for BrowserConfig — config resolution, profile parsing, and helpers.
 */
class BrowserConfigTest {

    @Test
    void resolve_defaults_whenNoConfig() {
        var resolved = BrowserConfig.resolve(null);

        assertTrue(resolved.isEnabled());
        assertTrue(resolved.isEvaluateEnabled());
        assertEquals(BrowserConstants.DEFAULT_CONTROL_PORT, resolved.getControlPort());
        assertEquals(BrowserConstants.DEFAULT_CDP_PROTOCOL, resolved.getCdpProtocol());
        assertEquals(BrowserConstants.DEFAULT_CDP_HOST, resolved.getCdpHost());
        assertTrue(resolved.isCdpIsLoopback());
        assertFalse(resolved.isHeadless());
        assertFalse(resolved.isNoSandbox());
        assertFalse(resolved.isAttachOnly());
        assertEquals(BrowserConstants.DEFAULT_PROFILE_NAME, resolved.getDefaultProfile());
        assertEquals(1, resolved.getProfiles().size());
    }

    @Test
    void resolve_respectsCustomConfig() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.BrowserConfig browserConfig = new OpenClawConfig.BrowserConfig();
        browserConfig.setEnabled(false);
        browserConfig.setHeadless(true);
        browserConfig.setNoSandbox(true);
        browserConfig.setControlPort(19999);
        browserConfig.setColor("FF0000");
        config.setBrowser(browserConfig);

        var resolved = BrowserConfig.resolve(config);

        assertFalse(resolved.isEnabled());
        assertTrue(resolved.isHeadless());
        assertTrue(resolved.isNoSandbox());
        assertEquals(19999, resolved.getControlPort());
        assertEquals("#FF0000", resolved.getColor());
    }

    @Test
    void resolve_parsesCdpUrl() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.BrowserConfig browserConfig = new OpenClawConfig.BrowserConfig();
        browserConfig.setCdpUrl("https://remote-host:9333");
        config.setBrowser(browserConfig);

        var resolved = BrowserConfig.resolve(config);

        assertEquals("https", resolved.getCdpProtocol());
        assertEquals("remote-host", resolved.getCdpHost());
        assertFalse(resolved.isCdpIsLoopback());
    }

    @Test
    void resolve_includesUserDefinedProfiles() {
        OpenClawConfig config = new OpenClawConfig();
        OpenClawConfig.BrowserConfig browserConfig = new OpenClawConfig.BrowserConfig();
        Map<String, OpenClawConfig.BrowserProfileConfig> profiles = new LinkedHashMap<>();
        OpenClawConfig.BrowserProfileConfig customProfile = new OpenClawConfig.BrowserProfileConfig();
        customProfile.setCdpPort(9333);
        customProfile.setColor("00FF00");
        customProfile.setDriver("extension");
        profiles.put("custom", customProfile);
        browserConfig.setProfiles(profiles);
        config.setBrowser(browserConfig);

        var resolved = BrowserConfig.resolve(config);

        assertEquals(2, resolved.getProfiles().size());
        var custom = resolved.getProfiles().get("custom");
        assertNotNull(custom);
        assertEquals(9333, custom.getCdpPort());
        assertEquals("#00FF00", custom.getColor());
        assertEquals("extension", custom.getDriver());
    }

    @Test
    void resolveProfile_defaultProfile() {
        var resolved = BrowserConfig.resolve(null);
        var profile = BrowserConfig.resolveProfile(resolved, null);

        assertNotNull(profile);
        assertEquals(BrowserConstants.DEFAULT_PROFILE_NAME, profile.getName());
    }

    @Test
    void resolveProfile_unknownProfile() {
        var resolved = BrowserConfig.resolve(null);
        var profile = BrowserConfig.resolveProfile(resolved, "nonexistent");
        assertNull(profile);
    }

    @Test
    void shouldStartLocalServer_whenEnabled() {
        var resolved = BrowserConfig.resolve(null);
        assertTrue(BrowserConfig.shouldStartLocalServer(resolved));
    }

    // ==================== Helper Tests ====================

    @Test
    void isLoopbackHost_trueCases() {
        assertTrue(BrowserConfig.isLoopbackHost("localhost"));
        assertTrue(BrowserConfig.isLoopbackHost("127.0.0.1"));
        assertTrue(BrowserConfig.isLoopbackHost("::1"));
        assertTrue(BrowserConfig.isLoopbackHost("[::1]"));
    }

    @Test
    void isLoopbackHost_falseCases() {
        assertFalse(BrowserConfig.isLoopbackHost("192.168.1.1"));
        assertFalse(BrowserConfig.isLoopbackHost("example.com"));
        assertFalse(BrowserConfig.isLoopbackHost(null));
    }

    @Test
    void normalizeHexColor_addsHash() {
        assertEquals("#FF0000", BrowserConfig.normalizeHexColor("FF0000"));
        assertEquals("#FF0000", BrowserConfig.normalizeHexColor("#FF0000"));
        assertEquals(BrowserConstants.DEFAULT_PROFILE_COLOR, BrowserConfig.normalizeHexColor(null));
        assertEquals(BrowserConstants.DEFAULT_PROFILE_COLOR, BrowserConfig.normalizeHexColor(""));
    }

    @Test
    void normalizeTimeoutMs_clamp() {
        assertEquals(5000, BrowserConfig.normalizeTimeoutMs(5000, 10000));
        assertEquals(10000, BrowserConfig.normalizeTimeoutMs(null, 10000));
        assertEquals(10000, BrowserConfig.normalizeTimeoutMs(-1, 10000));
    }
}

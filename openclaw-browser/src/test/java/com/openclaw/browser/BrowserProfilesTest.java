package com.openclaw.browser;

import com.openclaw.common.config.OpenClawConfig.BrowserProfileConfig;
import com.openclaw.common.config.PortDefaults;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link BrowserProfiles}.
 * Pure Java — no Playwright required.
 */
class BrowserProfilesTest {

    // =========================================================================
    // isValidProfileName
    // =========================================================================

    @Test
    void validProfileNames() {
        assertTrue(BrowserProfiles.isValidProfileName("chrome"));
        assertTrue(BrowserProfiles.isValidProfileName("my-profile"));
        assertTrue(BrowserProfiles.isValidProfileName("test123"));
        assertTrue(BrowserProfiles.isValidProfileName("a"));
        assertTrue(BrowserProfiles.isValidProfileName("0abc"));
    }

    @Test
    void invalidProfileNames() {
        assertFalse(BrowserProfiles.isValidProfileName(null));
        assertFalse(BrowserProfiles.isValidProfileName(""));
        assertFalse(BrowserProfiles.isValidProfileName("-starts-with-dash"));
        assertFalse(BrowserProfiles.isValidProfileName("UPPER"));
        assertFalse(BrowserProfiles.isValidProfileName("has space"));
        assertFalse(BrowserProfiles.isValidProfileName("has_underscore"));
        assertFalse(BrowserProfiles.isValidProfileName("has.dot"));
        assertFalse(BrowserProfiles.isValidProfileName("a".repeat(65))); // too long
    }

    @Test
    void maxLengthProfileName() {
        assertTrue(BrowserProfiles.isValidProfileName("a".repeat(64)));
    }

    // =========================================================================
    // allocateCdpPort
    // =========================================================================

    @Test
    void allocatesFirstAvailablePort() {
        Set<Integer> used = new HashSet<>();
        PortDefaults.PortRange range = new PortDefaults.PortRange(18800, 18899);

        Integer port = BrowserProfiles.allocateCdpPort(used, range);
        assertEquals(18800, port);
    }

    @Test
    void skipsUsedPorts() {
        Set<Integer> used = Set.of(18800, 18801, 18802);
        PortDefaults.PortRange range = new PortDefaults.PortRange(18800, 18899);

        Integer port = BrowserProfiles.allocateCdpPort(used, range);
        assertEquals(18803, port);
    }

    @Test
    void returnsNullWhenAllPortsUsed() {
        Set<Integer> used = new HashSet<>();
        for (int p = 100; p <= 102; p++)
            used.add(p);
        PortDefaults.PortRange range = new PortDefaults.PortRange(100, 102);

        Integer port = BrowserProfiles.allocateCdpPort(used, range);
        assertNull(port);
    }

    @Test
    void returnsNullForInvalidRange() {
        assertNull(BrowserProfiles.allocateCdpPort(Set.of(), new PortDefaults.PortRange(100, 50)));
        assertNull(BrowserProfiles.allocateCdpPort(Set.of(), new PortDefaults.PortRange(0, 100)));
        assertNull(BrowserProfiles.allocateCdpPort(Set.of(), new PortDefaults.PortRange(-1, 100)));
    }

    @Test
    void usesDefaultRangeWhenNull() {
        Integer port = BrowserProfiles.allocateCdpPort(Set.of(), null);
        assertEquals(PortDefaults.DEFAULT_BROWSER_CDP_PORT_RANGE_START, port);
    }

    // =========================================================================
    // getUsedPorts
    // =========================================================================

    @Test
    void extractsPortsFromCdpPort() {
        Map<String, BrowserProfileConfig> profiles = new LinkedHashMap<>();
        BrowserProfileConfig p1 = new BrowserProfileConfig();
        p1.setCdpPort(18800);
        profiles.put("chrome", p1);

        BrowserProfileConfig p2 = new BrowserProfileConfig();
        p2.setCdpPort(18801);
        profiles.put("firefox", p2);

        Set<Integer> used = BrowserProfiles.getUsedPorts(profiles);
        assertEquals(Set.of(18800, 18801), used);
    }

    @Test
    void extractsPortsFromCdpUrl() {
        Map<String, BrowserProfileConfig> profiles = new LinkedHashMap<>();
        BrowserProfileConfig p = new BrowserProfileConfig();
        p.setCdpUrl("http://localhost:9222");
        profiles.put("remote", p);

        Set<Integer> used = BrowserProfiles.getUsedPorts(profiles);
        assertTrue(used.contains(9222));
    }

    @Test
    void extractsDefaultPortFromHttpsUrl() {
        Map<String, BrowserProfileConfig> profiles = new LinkedHashMap<>();
        BrowserProfileConfig p = new BrowserProfileConfig();
        p.setCdpUrl("https://remote-browser.example.com");
        profiles.put("remote", p);

        Set<Integer> used = BrowserProfiles.getUsedPorts(profiles);
        assertTrue(used.contains(443));
    }

    @Test
    void returnsEmptySetForNullProfiles() {
        assertTrue(BrowserProfiles.getUsedPorts(null).isEmpty());
        assertTrue(BrowserProfiles.getUsedPorts(Map.of()).isEmpty());
    }

    // =========================================================================
    // allocateColor
    // =========================================================================

    @Test
    void allocatesFirstAvailableColor() {
        String color = BrowserProfiles.allocateColor(Set.of());
        assertEquals("#FF4500", color);
    }

    @Test
    void skipsUsedColors() {
        Set<String> used = Set.of("#FF4500"); // first color taken
        String color = BrowserProfiles.allocateColor(used);
        assertEquals("#0066CC", color); // second color
    }

    @Test
    void cyclesWhenAllColorsUsed() {
        Set<String> used = new HashSet<>();
        for (String c : BrowserProfiles.PROFILE_COLORS) {
            used.add(c.toUpperCase());
        }
        // Should still return a color (cycled)
        String color = BrowserProfiles.allocateColor(used);
        assertNotNull(color);
        assertTrue(BrowserProfiles.PROFILE_COLORS.contains(color));
    }

    // =========================================================================
    // getUsedColors
    // =========================================================================

    @Test
    void collectsUsedColors() {
        Map<String, BrowserProfileConfig> profiles = new LinkedHashMap<>();
        BrowserProfileConfig p1 = new BrowserProfileConfig();
        p1.setColor("#FF4500");
        profiles.put("chrome", p1);

        BrowserProfileConfig p2 = new BrowserProfileConfig();
        p2.setColor("#0066CC");
        profiles.put("firefox", p2);

        Set<String> colors = BrowserProfiles.getUsedColors(profiles);
        assertEquals(Set.of("#FF4500", "#0066CC"), colors);
    }

    @Test
    void returnsEmptySetForNullProfileColors() {
        assertTrue(BrowserProfiles.getUsedColors(null).isEmpty());
        assertTrue(BrowserProfiles.getUsedColors(Map.of()).isEmpty());
    }
}

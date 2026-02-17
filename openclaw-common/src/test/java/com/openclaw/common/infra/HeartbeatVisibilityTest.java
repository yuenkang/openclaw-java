package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class HeartbeatVisibilityTest {

    @Test
    void default_silentOk_showAlerts_useIndicator() {
        var vis = HeartbeatVisibility.DEFAULT;
        assertFalse(vis.showOk());
        assertTrue(vis.showAlerts());
        assertTrue(vis.useIndicator());
    }

    @Test
    void resolveDefaults_withNulls_usesDefault() {
        var vis = HeartbeatVisibility.resolveDefaults(null, null, null);
        assertEquals(HeartbeatVisibility.DEFAULT, vis);
    }

    @Test
    void resolveDefaults_overridesSpecificField() {
        var vis = HeartbeatVisibility.resolveDefaults(true, null, false);
        assertTrue(vis.showOk());
        assertTrue(vis.showAlerts()); // default
        assertFalse(vis.useIndicator());
    }

    @Test
    void resolve_layeredPrecedence() {
        // account-level overrides channel-level, which overrides defaults
        var vis = HeartbeatVisibility.resolve(
                /* defaults */ true, true, true,
                /* channel */ false, null, null,
                /* account */ null, false, null);
        assertFalse(vis.showOk()); // channel overrides default
        assertFalse(vis.showAlerts()); // account overrides channel+default
        assertTrue(vis.useIndicator()); // falls through to default
    }

    @Test
    void resolve_accountOverridesEverything() {
        var vis = HeartbeatVisibility.resolve(
                true, true, true,
                false, false, false,
                true, true, true);
        assertTrue(vis.showOk());
        assertTrue(vis.showAlerts());
        assertTrue(vis.useIndicator());
    }
}

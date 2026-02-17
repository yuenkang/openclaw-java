package com.openclaw.agent.hooks;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class HookStatusTest {

        @Nested
        class BuildEntryStatus {
                @Test
                void simpleHook_eligible() {
                        var hook = HookTypes.Hook.builder()
                                        .name("test-hook")
                                        .description("Test")
                                        .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                                        .build();
                        var entry = HookTypes.HookEntry.builder()
                                        .hook(hook)
                                        .build();

                        var report = HookStatus.buildWorkspaceHookStatus("/tmp", null, List.of(entry));
                        assertEquals(1, report.getHooks().size());

                        var status = report.getHooks().get(0);
                        assertEquals("test-hook", status.getName());
                        assertTrue(status.isEligible());
                        assertFalse(status.isDisabled());
                }

                @Test
                void disabledHook_notEligible() {
                        var hook = HookTypes.Hook.builder()
                                        .name("disabled-hook")
                                        .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                                        .build();
                        var invocation = HookTypes.HookInvocationPolicy.builder()
                                        .enabled(false)
                                        .build();
                        var entry = HookTypes.HookEntry.builder()
                                        .hook(hook).invocation(invocation)
                                        .build();

                        var report = HookStatus.buildWorkspaceHookStatus("/tmp", null, List.of(entry));
                        var status = report.getHooks().get(0);
                        assertTrue(status.isDisabled());
                        assertFalse(status.isEligible());
                }

                @Test
                void missingBinary_notEligible() {
                        var hook = HookTypes.Hook.builder()
                                        .name("needs-binary")
                                        .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                                        .build();
                        var metadata = HookTypes.HookMetadata.builder()
                                        .requires(HookTypes.HookRequirements.builder()
                                                        .bins(List.of("nonexistent_binary_xyz_999"))
                                                        .build())
                                        .build();
                        var entry = HookTypes.HookEntry.builder()
                                        .hook(hook).metadata(metadata)
                                        .build();

                        var report = HookStatus.buildWorkspaceHookStatus("/tmp", null, List.of(entry));
                        var status = report.getHooks().get(0);
                        assertFalse(status.isEligible());
                        assertEquals(List.of("nonexistent_binary_xyz_999"), status.getMissing().getBins());
                }

                @Test
                void alwaysHook_eligibleDespiteMissingBins() {
                        var hook = HookTypes.Hook.builder()
                                        .name("always-hook")
                                        .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                                        .build();
                        var metadata = HookTypes.HookMetadata.builder()
                                        .always(true)
                                        .requires(HookTypes.HookRequirements.builder()
                                                        .bins(List.of("nonexistent_binary_xyz_999"))
                                                        .build())
                                        .build();
                        var entry = HookTypes.HookEntry.builder()
                                        .hook(hook).metadata(metadata)
                                        .build();

                        var report = HookStatus.buildWorkspaceHookStatus("/tmp", null, List.of(entry));
                        var status = report.getHooks().get(0);
                        assertTrue(status.isEligible());
                        // always hooks report empty missing
                        assertTrue(status.getMissing().getBins().isEmpty());
                }

                @Test
                void wrongOs_notEligible() {
                        var hook = HookTypes.Hook.builder()
                                        .name("os-hook")
                                        .source(HookTypes.HookSource.OPENCLAW_WORKSPACE)
                                        .build();
                        var metadata = HookTypes.HookMetadata.builder()
                                        .os(List.of("windows_99_fake"))
                                        .build();
                        var entry = HookTypes.HookEntry.builder()
                                        .hook(hook).metadata(metadata)
                                        .build();

                        var report = HookStatus.buildWorkspaceHookStatus("/tmp", null, List.of(entry));
                        var status = report.getHooks().get(0);
                        assertFalse(status.isEligible());
                        assertEquals(List.of("windows_99_fake"), status.getMissing().getOs());
                }

                @Test
                void pluginHook_managedByPlugin() {
                        var hook = HookTypes.Hook.builder()
                                        .name("plugin-hook")
                                        .source(HookTypes.HookSource.OPENCLAW_PLUGIN)
                                        .pluginId("test-plugin")
                                        .build();
                        var entry = HookTypes.HookEntry.builder()
                                        .hook(hook)
                                        .build();

                        var report = HookStatus.buildWorkspaceHookStatus("/tmp", null, List.of(entry));
                        var status = report.getHooks().get(0);
                        assertTrue(status.isManagedByPlugin());
                        assertEquals("test-plugin", status.getPluginId());
                }
        }
}

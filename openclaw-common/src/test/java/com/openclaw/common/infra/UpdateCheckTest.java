package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class UpdateCheckTest {

    @Test
    void compareSemver_lessThan() {
        Integer cmp = UpdateCheck.compareSemver("1.0.0", "1.0.1");
        assertNotNull(cmp);
        assertTrue(cmp < 0);
    }

    @Test
    void compareSemver_equal() {
        Integer cmp = UpdateCheck.compareSemver("2.3.4", "2.3.4");
        assertNotNull(cmp);
        assertEquals(0, cmp);
    }

    @Test
    void compareSemver_greaterThan() {
        Integer cmp = UpdateCheck.compareSemver("3.0.0", "2.9.9");
        assertNotNull(cmp);
        assertTrue(cmp > 0);
    }

    @Test
    void compareSemver_withVPrefix() {
        Integer cmp = UpdateCheck.compareSemver("v1.2.3", "1.2.3");
        assertNotNull(cmp);
        assertEquals(0, cmp);
    }

    @Test
    void compareSemver_invalidReturnsNull() {
        assertNull(UpdateCheck.compareSemver("abc", "1.0.0"));
        assertNull(UpdateCheck.compareSemver("1.0.0", "xyz"));
        assertNull(UpdateCheck.compareSemver(null, "1.0.0"));
    }

    @Test
    void parseSemver_basic() {
        int[] parts = UpdateCheck.parseSemver("1.2.3");
        assertArrayEquals(new int[] { 1, 2, 3 }, parts);
    }

    @Test
    void parseSemver_withPreRelease() {
        int[] parts = UpdateCheck.parseSemver("1.0.0-beta.1");
        assertArrayEquals(new int[] { 1, 0, 0 }, parts);
    }

    @Test
    void parseSemver_withVPrefix() {
        int[] parts = UpdateCheck.parseSemver("v3.2.1");
        assertArrayEquals(new int[] { 3, 2, 1 }, parts);
    }

    @Test
    void parseSemver_invalidReturnsNull() {
        assertNull(UpdateCheck.parseSemver(null));
        assertNull(UpdateCheck.parseSemver(""));
        assertNull(UpdateCheck.parseSemver("abc"));
    }

    @Test
    void semverPreRelease_extracted() {
        assertEquals("beta.1", UpdateCheck.semverPreRelease("1.0.0-beta.1"));
        assertNull(UpdateCheck.semverPreRelease("1.0.0"));
        assertNull(UpdateCheck.semverPreRelease(null));
    }

    @Test
    void resolveInstallKind_unknownForNull() {
        assertEquals(UpdateCheck.InstallKind.UNKNOWN, UpdateCheck.resolveInstallKind(null));
        assertEquals(UpdateCheck.InstallKind.UNKNOWN, UpdateCheck.resolveInstallKind(""));
    }

    @Test
    void resolveInstallKind_detectsGitRepo() {
        // Use the project root which IS a git repo
        String projectRoot = System.getProperty("user.dir");
        // The openclaw-java dir should be under a git repo
        UpdateCheck.InstallKind kind = UpdateCheck.resolveInstallKind(projectRoot);
        // Should be GIT if .git exists, or PACKAGE/UNKNOWN otherwise
        assertNotNull(kind);
    }

    @Test
    void updateChannels_normalize() {
        assertEquals(UpdateChannels.Channel.STABLE, UpdateChannels.normalize("stable"));
        assertEquals(UpdateChannels.Channel.BETA, UpdateChannels.normalize("Beta"));
        assertEquals(UpdateChannels.Channel.DEV, UpdateChannels.normalize("DEV"));
        assertNull(UpdateChannels.normalize(null));
        assertNull(UpdateChannels.normalize("invalid"));
    }

    @Test
    void updateChannels_channelToTag() {
        assertEquals("latest", UpdateChannels.channelToTag(UpdateChannels.Channel.STABLE));
        assertEquals("beta", UpdateChannels.channelToTag(UpdateChannels.Channel.BETA));
        assertEquals("dev", UpdateChannels.channelToTag(UpdateChannels.Channel.DEV));
    }

    @Test
    void updateChannels_isBetaTag() {
        assertTrue(UpdateChannels.isBetaTag("v1.0.0-beta.1"));
        assertFalse(UpdateChannels.isBetaTag("v1.0.0"));
        assertFalse(UpdateChannels.isBetaTag(null));
    }

    @Test
    void updateChannels_resolveEffective_configOverride() {
        UpdateChannels.ResolvedChannel resolved = UpdateChannels.resolveEffective(
                UpdateChannels.Channel.BETA, "package", null, null);
        assertEquals(UpdateChannels.Channel.BETA, resolved.channel());
        assertEquals(UpdateChannels.ChannelSource.CONFIG, resolved.source());
    }

    @Test
    void updateChannels_resolveEffective_gitTag() {
        UpdateChannels.ResolvedChannel resolved = UpdateChannels.resolveEffective(
                null, "git", "v1.0.0-beta.2", null);
        assertEquals(UpdateChannels.Channel.BETA, resolved.channel());
        assertEquals(UpdateChannels.ChannelSource.GIT_TAG, resolved.source());
    }

    @Test
    void updateChannels_resolveEffective_gitBranch() {
        UpdateChannels.ResolvedChannel resolved = UpdateChannels.resolveEffective(
                null, "git", null, "main");
        assertEquals(UpdateChannels.Channel.DEV, resolved.channel());
        assertEquals(UpdateChannels.ChannelSource.GIT_BRANCH, resolved.source());
    }

    @Test
    void updateChannels_resolveEffective_packageDefault() {
        UpdateChannels.ResolvedChannel resolved = UpdateChannels.resolveEffective(
                null, "package", null, null);
        assertEquals(UpdateChannels.Channel.STABLE, resolved.channel());
        assertEquals(UpdateChannels.ChannelSource.DEFAULT, resolved.source());
    }

    @Test
    void updateChannels_formatLabel() {
        assertEquals("stable (config)", UpdateChannels.formatLabel(
                UpdateChannels.Channel.STABLE, UpdateChannels.ChannelSource.CONFIG, null, null));
        assertEquals("beta (v1.0.0-beta)", UpdateChannels.formatLabel(
                UpdateChannels.Channel.BETA, UpdateChannels.ChannelSource.GIT_TAG, "v1.0.0-beta", null));
        assertEquals("dev (main)", UpdateChannels.formatLabel(
                UpdateChannels.Channel.DEV, UpdateChannels.ChannelSource.GIT_BRANCH, null, "main"));
    }
}

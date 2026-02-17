package com.openclaw.common.infra;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class GitCommitTest {

    @AfterEach
    void cleanup() {
        GitCommit.resetCache();
    }

    @Test
    void resolveCommitHash_fromCurrentRepo() {
        // This test runs inside the openclaw-java git repo, so should find a commit
        String commit = GitCommit.resolveCommitHash();
        assertNotNull(commit, "Should resolve commit hash from .git/HEAD in the project");
        assertTrue(commit.length() == 7, "Short hash should be 7 chars, got: " + commit);
        assertTrue(commit.matches("[0-9a-f]{7}"), "Hash should be hex: " + commit);
    }

    @Test
    void resolveCommitHash_isCached() {
        String first = GitCommit.resolveCommitHash();
        String second = GitCommit.resolveCommitHash();
        assertEquals(first, second);
    }

    @Test
    void resolveCommitHash_fromNonGitDir() {
        GitCommit.resetCache();
        String commit = GitCommit.resolveCommitHash("/tmp");
        // May be null if no .git found, which is fine
        assertTrue(commit == null || commit.length() == 7);
    }
}

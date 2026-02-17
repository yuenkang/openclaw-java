package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ExecApprovalsTest {

    @Nested
    class AllowlistMatching {
        @Test
        void exactMatch() {
            var entries = List.of(
                    ExecApprovals.AllowlistEntry.builder().pattern("ls").build());
            assertTrue(ExecApprovals.isAllowed(entries, "ls"));
            assertFalse(ExecApprovals.isAllowed(entries, "rm"));
        }

        @Test
        void wildcardMatchesAll() {
            var entries = List.of(
                    ExecApprovals.AllowlistEntry.builder().pattern("*").build());
            assertTrue(ExecApprovals.isAllowed(entries, "ls"));
            assertTrue(ExecApprovals.isAllowed(entries, "rm"));
        }

        @Test
        void emptyAllowlist_returnsFalse() {
            assertFalse(ExecApprovals.isAllowed(List.of(), "ls"));
            assertFalse(ExecApprovals.isAllowed(null, "ls"));
        }
    }

    @Nested
    class SafeBins {
        @Test
        void defaultSafeBinsRecognized() {
            assertTrue(ExecApprovals.isSafeBin("grep"));
            assertTrue(ExecApprovals.isSafeBin("jq"));
            assertTrue(ExecApprovals.isSafeBin("wc"));
        }

        @Test
        void fullPathHandled() {
            assertTrue(ExecApprovals.isSafeBin("/usr/bin/grep"));
        }

        @Test
        void dangerousCommandsNotSafe() {
            assertFalse(ExecApprovals.isSafeBin("rm"));
            assertFalse(ExecApprovals.isSafeBin("curl"));
        }
    }

    @Nested
    class FileResolution {
        @Test
        void defaultsUsedForEmptyFile() {
            var file = ExecApprovals.ExecApprovalsFile.builder().build();
            var resolved = ExecApprovals.resolve(file, "test-agent");
            assertEquals(ExecApprovals.ExecSecurity.DENY, resolved.getSecurity());
            assertEquals(ExecApprovals.ExecAsk.ON_MISS, resolved.getAsk());
            assertTrue(resolved.getAllowlist().isEmpty());
        }
    }

    @Nested
    class AddToAllowlist {
        @Test
        void addsEntryToAgentAllowlist() {
            var file = ExecApprovals.ExecApprovalsFile.builder().build();
            ExecApprovals.addToAllowlist(file, "agent1",
                    ExecApprovals.AllowlistEntry.builder().pattern("ls").build());

            var resolved = ExecApprovals.resolve(file, "agent1");
            assertTrue(ExecApprovals.isAllowed(resolved.getAllowlist(), "ls"));
        }
    }
}

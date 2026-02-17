package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ExecHostTest {

    @Test
    void execRequest_serializesCorrectly() {
        var req = ExecHost.ExecRequest.builder()
                .command(new String[] { "ls", "-la" })
                .cwd("/tmp")
                .agentId("main")
                .build();

        assertNotNull(req);
        assertEquals(2, req.getCommand().length);
        assertEquals("ls", req.getCommand()[0]);
        assertEquals("/tmp", req.getCwd());
    }

    @Test
    void requestViaSocket_returnsNullOnInvalidParams() {
        var req = ExecHost.ExecRequest.builder()
                .command(new String[] { "echo", "hello" })
                .build();

        // Null host → null
        assertNull(ExecHost.requestViaSocket(null, 0, "token", req, 1000));
        // Empty token → null
        assertNull(ExecHost.requestViaSocket("127.0.0.1", 12345, "", req, 1000));
    }

    @Test
    void requestViaSocket_returnsNullOnConnectionRefused() {
        var req = ExecHost.ExecRequest.builder()
                .command(new String[] { "echo", "test" })
                .build();

        // Connect to a likely-unused port — should return null (connection refused)
        ExecHost.ExecResponse result = ExecHost.requestViaSocket(
                "127.0.0.1", 39999, "test-token", req, 2000);
        assertNull(result);
    }

    @Test
    void execRunResult_defaultValues() {
        var result = ExecHost.ExecRunResult.builder()
                .success(true)
                .stdout("hello")
                .stderr("")
                .build();

        assertTrue(result.isSuccess());
        assertFalse(result.isTimedOut());
        assertEquals("hello", result.getStdout());
    }
}

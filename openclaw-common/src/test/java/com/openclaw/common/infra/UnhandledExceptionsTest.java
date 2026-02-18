package com.openclaw.common.infra;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.net.ConnectException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;

import static org.junit.jupiter.api.Assertions.*;

class UnhandledExceptionsTest {

    @AfterEach
    void tearDown() {
        UnhandledExceptions.resetForTest();
    }

    @Test
    void classify_fatal_outOfMemory() {
        assertEquals(UnhandledExceptions.Category.FATAL,
                UnhandledExceptions.classify(new OutOfMemoryError()));
    }

    @Test
    void classify_fatal_stackOverflow() {
        assertEquals(UnhandledExceptions.Category.FATAL,
                UnhandledExceptions.classify(new StackOverflowError()));
    }

    @Test
    void classify_abort_interrupted() {
        assertEquals(UnhandledExceptions.Category.ABORT,
                UnhandledExceptions.classify(new InterruptedException()));
    }

    @Test
    void classify_abort_cancelMessage() {
        assertEquals(UnhandledExceptions.Category.ABORT,
                UnhandledExceptions.classify(new RuntimeException("Request cancelled")));
    }

    @Test
    void classify_config_apiKey() {
        assertEquals(UnhandledExceptions.Category.CONFIG,
                UnhandledExceptions.classify(new RuntimeException("Invalid API key")));
    }

    @Test
    void classify_config_unauthorized() {
        assertEquals(UnhandledExceptions.Category.CONFIG,
                UnhandledExceptions.classify(new RuntimeException("401 Unauthorized")));
    }

    @Test
    void classify_transient_socketTimeout() {
        assertEquals(UnhandledExceptions.Category.TRANSIENT,
                UnhandledExceptions.classify(new SocketTimeoutException("Read timed out")));
    }

    @Test
    void classify_transient_connectionRefused() {
        assertEquals(UnhandledExceptions.Category.TRANSIENT,
                UnhandledExceptions.classify(new ConnectException("Connection refused")));
    }

    @Test
    void classify_transient_unknownHost() {
        assertEquals(UnhandledExceptions.Category.TRANSIENT,
                UnhandledExceptions.classify(new UnknownHostException("api.example.com")));
    }

    @Test
    void classify_transient_rateLimitMessage() {
        assertEquals(UnhandledExceptions.Category.TRANSIENT,
                UnhandledExceptions.classify(new RuntimeException("Rate limit exceeded")));
    }

    @Test
    void classify_transient_429() {
        assertEquals(UnhandledExceptions.Category.TRANSIENT,
                UnhandledExceptions.classify(new RuntimeException("HTTP 429 Too Many Requests")));
    }

    @Test
    void classify_unknown_generic() {
        assertEquals(UnhandledExceptions.Category.UNKNOWN,
                UnhandledExceptions.classify(new RuntimeException("some error")));
    }

    @Test
    void classify_null() {
        assertEquals(UnhandledExceptions.Category.UNKNOWN,
                UnhandledExceptions.classify(null));
    }

    @Test
    void messageChain_singleException() {
        String chain = UnhandledExceptions.messageChain(new RuntimeException("hello"));
        assertTrue(chain.contains("RuntimeException"));
        assertTrue(chain.contains("hello"));
    }

    @Test
    void messageChain_nestedCause() {
        Exception inner = new SocketTimeoutException("timed out");
        Exception outer = new RuntimeException("request failed", inner);
        String chain = UnhandledExceptions.messageChain(outer);

        assertTrue(chain.contains("RuntimeException"));
        assertTrue(chain.contains("SocketTimeoutException"));
        assertTrue(chain.contains("timed out"));
    }

    @Test
    void install_idempotent() {
        assertFalse(UnhandledExceptions.isInstalled());
        UnhandledExceptions.install();
        assertTrue(UnhandledExceptions.isInstalled());
        // Second install should not throw
        UnhandledExceptions.install();
        assertTrue(UnhandledExceptions.isInstalled());
    }
}

package com.openclaw.agent.tools.policy;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Abort signal wrapper for tool execution — checks abort before execution,
 * combines signals.
 * Corresponds to TypeScript pi-tools.abort.ts.
 * 
 * In Java, we use Thread.interrupted() as the abort mechanism.
 */
@Slf4j
public final class ToolAbortHandler {

    private ToolAbortHandler() {
    }

    /**
     * Exception thrown when a tool execution is aborted.
     */
    public static class AbortException extends RuntimeException {
        public AbortException() {
            super("Aborted");
        }

        public AbortException(String message) {
            super(message);
        }
    }

    /**
     * Check if the current thread has been interrupted (abort requested).
     * 
     * @throws AbortException if the thread is interrupted
     */
    public static void checkAborted() {
        if (Thread.currentThread().isInterrupted()) {
            throw new AbortException();
        }
    }

    /**
     * Execute a tool with abort checking — checks before execution starts.
     *
     * @param toolName tool name for logging
     * @param runner   the actual tool execution logic
     * @return the tool result
     * @throws AbortException if aborted before or during execution
     */
    public static Object executeWithAbortCheck(String toolName, ToolRunner runner) {
        checkAborted();
        try {
            return runner.run();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AbortException("Tool " + toolName + " was aborted");
        } catch (AbortException e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException("Tool " + toolName + " failed: " + e.getMessage(), e);
        }
    }

    @FunctionalInterface
    public interface ToolRunner {
        Object run() throws Exception;
    }
}

package com.openclaw.gateway.runtime;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link RuntimeMetrics}.
 */
class RuntimeMetricsTest {

    private RuntimeMetrics metrics;

    @BeforeEach
    void setUp() {
        metrics = RuntimeMetrics.getInstance();
        metrics.reset();
    }

    @Nested
    class WebSocket {

        @Test
        void connectAndDisconnect() {
            metrics.onWebSocketConnect();
            metrics.onWebSocketConnect();
            assertEquals(2, metrics.getActiveWebSocketConnections());
            assertEquals(2, metrics.getTotalWebSocketConnections());

            metrics.onWebSocketDisconnect();
            assertEquals(1, metrics.getActiveWebSocketConnections());
            assertEquals(2, metrics.getTotalWebSocketConnections());
        }

        @Test
        void initiallyZero() {
            assertEquals(0, metrics.getActiveWebSocketConnections());
            assertEquals(0, metrics.getTotalWebSocketConnections());
        }
    }

    @Nested
    class Http {

        @Test
        void requestCounting() {
            metrics.onHttpRequest("chat.send");
            metrics.onHttpRequest("chat.send");
            metrics.onHttpRequest("session.create");

            assertEquals(3, metrics.getTotalHttpRequests());
            assertEquals(2, metrics.getHttpRequestsByMethod("chat.send"));
            assertEquals(1, metrics.getHttpRequestsByMethod("session.create"));
            assertEquals(0, metrics.getHttpRequestsByMethod("nonexistent"));
        }
    }

    @Nested
    class AgentRuns {

        @Test
        void runLifecycle() {
            metrics.onAgentRunStart();
            assertEquals(1, metrics.getActiveAgentRuns());
            assertEquals(1, metrics.getTotalAgentRuns());

            metrics.onAgentRunEnd(1500, true);
            assertEquals(0, metrics.getActiveAgentRuns());
            assertEquals(1, metrics.getTotalAgentRuns());
            assertEquals(0, metrics.getAgentRunErrors());
            assertEquals(1500, metrics.getLastAgentRunDurationMs());
        }

        @Test
        void errorCounting() {
            metrics.onAgentRunStart();
            metrics.onAgentRunEnd(500, false);
            assertEquals(1, metrics.getAgentRunErrors());
        }

        @Test
        void averageDuration() {
            metrics.onAgentRunStart();
            metrics.onAgentRunEnd(1000, true);
            metrics.onAgentRunStart();
            metrics.onAgentRunEnd(3000, true);

            assertEquals(2000, metrics.getAverageAgentRunDurationMs());
        }

        @Test
        void averageDuration_noRuns() {
            assertEquals(0, metrics.getAverageAgentRunDurationMs());
        }

        @Test
        void toolCallCounting() {
            metrics.onToolCall();
            metrics.onToolCall();
            metrics.onToolCall();
            assertEquals(3, metrics.getTotalToolCalls());
        }
    }

    @Nested
    class Messages {

        @Test
        void messageCounting() {
            metrics.onInboundMessage();
            metrics.onInboundMessage();
            metrics.onOutboundMessage();

            assertEquals(2, metrics.getInboundMessages());
            assertEquals(1, metrics.getOutboundMessages());
        }
    }

    @Nested
    class SnapshotTest {

        @Test
        void snapshotCapturesAllMetrics() {
            metrics.onWebSocketConnect();
            metrics.onHttpRequest("chat.send");
            metrics.onAgentRunStart();
            metrics.onToolCall();
            metrics.onInboundMessage();
            metrics.onOutboundMessage();
            metrics.onAgentRunEnd(2000, true);

            var snap = metrics.snapshot();

            assertEquals(1, snap.activeWebSocketConnections());
            assertEquals(1, snap.totalWebSocketConnections());
            assertEquals(1, snap.totalHttpRequests());
            assertEquals(0, snap.activeAgentRuns());
            assertEquals(1, snap.totalAgentRuns());
            assertEquals(0, snap.agentRunErrors());
            assertEquals(1, snap.totalToolCalls());
            assertEquals(2000, snap.lastAgentRunDurationMs());
            assertEquals(1, snap.inboundMessages());
            assertEquals(1, snap.outboundMessages());
        }

        @Test
        void snapshotIsImmutable() {
            metrics.onWebSocketConnect();
            var snap1 = metrics.snapshot();

            metrics.onWebSocketConnect();
            var snap2 = metrics.snapshot();

            // snap1 should not change
            assertEquals(1, snap1.activeWebSocketConnections());
            assertEquals(2, snap2.activeWebSocketConnections());
        }
    }

    @Test
    void resetClearsEverything() {
        metrics.onWebSocketConnect();
        metrics.onHttpRequest("test");
        metrics.onAgentRunStart();
        metrics.onToolCall();
        metrics.onInboundMessage();
        metrics.onOutboundMessage();

        metrics.reset();

        assertEquals(0, metrics.getActiveWebSocketConnections());
        assertEquals(0, metrics.getTotalWebSocketConnections());
        assertEquals(0, metrics.getTotalHttpRequests());
        assertEquals(0, metrics.getActiveAgentRuns());
        assertEquals(0, metrics.getTotalAgentRuns());
        assertEquals(0, metrics.getTotalToolCalls());
        assertEquals(0, metrics.getInboundMessages());
        assertEquals(0, metrics.getOutboundMessages());
    }
}

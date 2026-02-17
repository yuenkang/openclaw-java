package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

class HeartbeatRunnerTest {

    private HeartbeatRunner runner;

    @AfterEach
    void tearDown() {
        if (runner != null) {
            runner.close();
        }
    }

    @Test
    void startAndStop() {
        runner = new HeartbeatRunner(60_000, reason -> {
        });
        assertFalse(runner.isRunning());
        runner.start();
        assertTrue(runner.isRunning());
        runner.stop();
        assertFalse(runner.isRunning());
    }

    @Test
    void triggerNow_executesImmediately() throws Exception {
        var latch = new CountDownLatch(1);
        var counter = new AtomicInteger(0);

        runner = new HeartbeatRunner(60_000, reason -> {
            counter.incrementAndGet();
            latch.countDown();
        });

        runner.start();
        runner.triggerNow("test");

        assertTrue(latch.await(2, TimeUnit.SECONDS));
        assertTrue(counter.get() >= 1);
    }

    @Test
    void updateInterval() {
        runner = new HeartbeatRunner(60_000, reason -> {
        });
        assertEquals(60_000, runner.getIntervalMs());
        runner.updateInterval(30_000);
        assertEquals(30_000, runner.getIntervalMs());
    }

    @Test
    void minimumInterval_enforced() {
        runner = new HeartbeatRunner(100, reason -> {
        });
        assertEquals(1000, runner.getIntervalMs()); // Minimum 1s
    }
}

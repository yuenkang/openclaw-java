package com.openclaw.plugin.services;

import com.openclaw.plugin.PluginTypes;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for PluginServiceManager.
 */
class PluginServiceManagerTest {

    PluginServiceManager manager;

    @BeforeEach
    void setUp() {
        manager = new PluginServiceManager();
    }

    @Test
    void registerAndStartService() {
        AtomicBoolean started = new AtomicBoolean(false);
        AtomicBoolean stopped = new AtomicBoolean(false);

        manager.register("test-plugin", new PluginServiceManager.PluginService() {
            @Override
            public String getId() {
                return "test-svc";
            }

            @Override
            public void start(PluginServiceManager.ServiceContext ctx) {
                started.set(true);
            }

            @Override
            public void stop(PluginServiceManager.ServiceContext ctx) {
                stopped.set(true);
            }
        }, "test-source");

        assertEquals(1, manager.getRegistrations().size());

        var handle = manager.startAll(null, null, null);
        assertTrue(started.get());
        assertEquals(1, manager.getRunningCount());

        handle.stop();
        assertTrue(stopped.get());
        assertEquals(0, manager.getRunningCount());
    }

    @Test
    void serviceStartFailureDoesNotBlockOthers() {
        AtomicBoolean secondStarted = new AtomicBoolean(false);

        manager.register("fail-plugin", new PluginServiceManager.PluginService() {
            @Override
            public String getId() {
                return "fail-svc";
            }

            @Override
            public void start(PluginServiceManager.ServiceContext ctx) throws Exception {
                throw new RuntimeException("Boom");
            }
        }, "");

        manager.register("ok-plugin", new PluginServiceManager.PluginService() {
            @Override
            public String getId() {
                return "ok-svc";
            }

            @Override
            public void start(PluginServiceManager.ServiceContext ctx) {
                secondStarted.set(true);
            }
        }, "");

        manager.startAll(null, null, null);
        assertTrue(secondStarted.get());
        assertEquals(1, manager.getRunningCount()); // only ok-svc
    }

    @Test
    void clearStopsAndRemoves() {
        AtomicBoolean stopped = new AtomicBoolean(false);
        manager.register("p", new PluginServiceManager.PluginService() {
            @Override
            public String getId() {
                return "svc";
            }

            @Override
            public void start(PluginServiceManager.ServiceContext ctx) {
            }

            @Override
            public void stop(PluginServiceManager.ServiceContext ctx) {
                stopped.set(true);
            }
        }, "");

        manager.startAll(null, null, null);
        manager.clear();
        assertTrue(stopped.get());
        assertEquals(0, manager.getRegistrations().size());
        assertEquals(0, manager.getRunningCount());
    }
}

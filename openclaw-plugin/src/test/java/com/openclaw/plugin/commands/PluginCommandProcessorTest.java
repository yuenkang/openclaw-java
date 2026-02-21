package com.openclaw.plugin.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for PluginCommandProcessor.
 */
class PluginCommandProcessorTest {

    PluginCommandProcessor processor;

    @BeforeEach
    void setUp() {
        processor = new PluginCommandProcessor();
    }

    // --- Validation ---

    @Test
    void validCommandNames() {
        assertNull(PluginCommandProcessor.validateCommandName("myplugin"));
        assertNull(PluginCommandProcessor.validateCommandName("my-plugin"));
        assertNull(PluginCommandProcessor.validateCommandName("my_plugin2"));
    }

    @Test
    void invalidCommandNames() {
        assertNotNull(PluginCommandProcessor.validateCommandName(""));
        assertNotNull(PluginCommandProcessor.validateCommandName("123abc"));
        assertNotNull(PluginCommandProcessor.validateCommandName("my plugin"));
    }

    @Test
    void reservedCommandNames() {
        assertNotNull(PluginCommandProcessor.validateCommandName("help"));
        assertNotNull(PluginCommandProcessor.validateCommandName("status"));
        assertNotNull(PluginCommandProcessor.validateCommandName("config"));
    }

    // --- Registration ---

    @Test
    void registerValidCommand() {
        var result = processor.register("test-plugin",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("greet")
                        .description("Say hello")
                        .acceptsArgs(true)
                        .handler(ctx -> PluginCommandProcessor.CommandResult.builder()
                                .text("Hello " + ctx.getArgs())
                                .success(true)
                                .build())
                        .build());
        assertTrue(result.isOk());
    }

    @Test
    void registerDuplicateCommandFails() {
        processor.register("plugin-a",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("greet").build());
        var result = processor.register("plugin-b",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("greet").build());
        assertFalse(result.isOk());
        assertTrue(result.getError().contains("already registered"));
    }

    @Test
    void registerReservedCommandFails() {
        var result = processor.register("test",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("help").build());
        assertFalse(result.isOk());
    }

    // --- Matching ---

    @Test
    void matchRegisteredCommand() {
        processor.register("test",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("greet")
                        .acceptsArgs(true)
                        .build());
        var match = processor.match("/greet world");
        assertNotNull(match);
        assertEquals("greet", match.getCommand().getName());
        assertEquals("world", match.getArgs());
    }

    @Test
    void matchNoArgCommandWithArgs() {
        processor.register("test",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("ping")
                        .acceptsArgs(false)
                        .build());
        // Should not match because command doesn't accept args
        assertNull(processor.match("/ping extra"));
    }

    @Test
    void matchUnregisteredReturnsNull() {
        assertNull(processor.match("/unknown"));
    }

    @Test
    void matchNonSlashReturnsNull() {
        assertNull(processor.match("hello"));
    }

    // --- Sanitization & listing ---

    @Test
    void sanitizeArgsRemovesControlChars() {
        assertEquals("hello", PluginCommandProcessor.sanitizeArgs("he\u0000llo"));
        assertEquals("hi\tthere", PluginCommandProcessor.sanitizeArgs("hi\tthere")); // tabs preserved
        assertNull(PluginCommandProcessor.sanitizeArgs(null));
    }

    @Test
    void listAndClear() {
        processor.register("p1",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("cmd1").build());
        processor.register("p2",
                PluginCommandProcessor.RegisteredCommand.builder()
                        .name("cmd2").build());
        assertEquals(2, processor.listCommands().size());

        processor.clearForPlugin("p1");
        assertEquals(1, processor.listCommands().size());

        processor.clear();
        assertEquals(0, processor.listCommands().size());
    }
}

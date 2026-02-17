package com.openclaw.common.infra;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class MachineDisplayNameTest {

    @Test
    void get_returnsNonEmpty() {
        String name = MachineDisplayName.get();
        assertNotNull(name);
        assertFalse(name.isEmpty());
    }

    @Test
    void get_isCached() {
        String first = MachineDisplayName.get();
        String second = MachineDisplayName.get();
        assertSame(first, second); // same String instance (cached)
    }
}

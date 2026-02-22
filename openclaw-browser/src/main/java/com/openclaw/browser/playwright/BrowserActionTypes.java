package com.openclaw.browser.playwright;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * Type definitions for browser form fields and act kinds.
 * Corresponds to TypeScript's client-actions-types.ts.
 */
public final class BrowserActionTypes {

    private BrowserActionTypes() {
    }

    /**
     * A form field for batch fill operations.
     */
    @Data
    @Builder
    public static class BrowserFormField {
        private String ref;
        private String selector;
        private String value;
        private String ariaLabel;

        /** For select elements — one or more values */
        private List<String> options;
        /** For checkboxes */
        private Boolean checked;
    }

    /**
     * Known act kinds supported by the agent.
     */
    public enum ActKind {
        CLICK("click"),
        TYPE("type"),
        PRESS("press"),
        HOVER("hover"),
        SCROLL_INTO_VIEW("scrollIntoView"),
        DRAG("drag"),
        SELECT("select"),
        FILL("fill"),
        FORM_FILL("form_fill"),
        EVALUATE("evaluate"),
        WAIT("wait"),
        NAVIGATE("navigate"),
        RESIZE("resize"),
        DOWNLOAD("download"),
        WAIT_FOR_DOWNLOAD("wait_for_download"),
        SCREENSHOT("screenshot"),
        CLOSE("close"),
        GO_BACK("goBack"),
        GO_FORWARD("goForward"),
        SET_LOCALE("set_locale"),
        SET_TIMEZONE("set_timezone"),
        SET_DEVICE("set_device"),
        RESPONSE_BODY("response_body");

        private final String value;

        ActKind(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        /**
         * Parse an act kind string, returning null if not recognized.
         */
        public static ActKind fromString(String s) {
            if (s == null) return null;
            for (ActKind kind : values()) {
                if (kind.value.equals(s)) return kind;
            }
            return null;
        }

        public static boolean isValid(String s) {
            return fromString(s) != null;
        }
    }
}

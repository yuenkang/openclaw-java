package com.openclaw.channel;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Sender identity validation for inbound messages.
 * Corresponds to TypeScript's channels/sender-identity.ts.
 */
public final class SenderIdentity {

    private SenderIdentity() {
    }

    private static final Pattern E164_PATTERN = Pattern.compile("^\\+\\d{3,}$");
    private static final Pattern WHITESPACE = Pattern.compile("\\s");

    /**
     * Validate sender identity fields from a message context.
     *
     * @return list of validation issues (empty if valid)
     */
    public static List<String> validate(String chatType,
            String senderId,
            String senderName,
            String senderUsername,
            String senderE164) {
        List<String> issues = new ArrayList<>();
        boolean isDirect = "direct".equals(ChatType.normalize(chatType));
        String id = trimOrEmpty(senderId);
        String name = trimOrEmpty(senderName);
        String username = trimOrEmpty(senderUsername);
        String e164 = trimOrEmpty(senderE164);

        if (!isDirect) {
            if (id.isEmpty() && name.isEmpty() && username.isEmpty() && e164.isEmpty()) {
                issues.add("missing sender identity (SenderId/SenderName/SenderUsername/SenderE164)");
            }
        }

        if (!e164.isEmpty()) {
            if (!E164_PATTERN.matcher(e164).matches()) {
                issues.add("invalid SenderE164: " + e164);
            }
        }

        if (!username.isEmpty()) {
            if (username.contains("@")) {
                issues.add("SenderUsername should not include \"@\": " + username);
            }
            if (WHITESPACE.matcher(username).find()) {
                issues.add("SenderUsername should not include whitespace: " + username);
            }
        }

        if (senderId != null && id.isEmpty()) {
            issues.add("SenderId is set but empty");
        }

        return issues;
    }

    private static String trimOrEmpty(String value) {
        return value != null ? value.trim() : "";
    }
}

package com.openclaw.channel;

import java.util.List;

/**
 * Channel message action name constants.
 * Corresponds to TypeScript's channels/plugins/message-action-names.ts.
 */
public final class ChannelMessageActions {

    private ChannelMessageActions() {
    }

    public static final String SEND = "send";
    public static final String BROADCAST = "broadcast";
    public static final String POLL = "poll";
    public static final String REACT = "react";
    public static final String REACTIONS = "reactions";
    public static final String READ = "read";
    public static final String EDIT = "edit";
    public static final String UNSEND = "unsend";
    public static final String REPLY = "reply";
    public static final String SEND_WITH_EFFECT = "sendWithEffect";
    public static final String RENAME_GROUP = "renameGroup";
    public static final String SET_GROUP_ICON = "setGroupIcon";
    public static final String ADD_PARTICIPANT = "addParticipant";
    public static final String REMOVE_PARTICIPANT = "removeParticipant";
    public static final String LEAVE_GROUP = "leaveGroup";
    public static final String SEND_ATTACHMENT = "sendAttachment";
    public static final String DELETE = "delete";
    public static final String PIN = "pin";
    public static final String UNPIN = "unpin";
    public static final String LIST_PINS = "list-pins";
    public static final String PERMISSIONS = "permissions";
    public static final String THREAD_CREATE = "thread-create";
    public static final String THREAD_LIST = "thread-list";
    public static final String THREAD_REPLY = "thread-reply";
    public static final String SEARCH = "search";
    public static final String STICKER = "sticker";
    public static final String STICKER_SEARCH = "sticker-search";
    public static final String MEMBER_INFO = "member-info";
    public static final String ROLE_INFO = "role-info";
    public static final String EMOJI_LIST = "emoji-list";
    public static final String EMOJI_UPLOAD = "emoji-upload";
    public static final String STICKER_UPLOAD = "sticker-upload";
    public static final String ROLE_ADD = "role-add";
    public static final String ROLE_REMOVE = "role-remove";
    public static final String CHANNEL_INFO = "channel-info";
    public static final String CHANNEL_LIST = "channel-list";
    public static final String CHANNEL_CREATE = "channel-create";
    public static final String CHANNEL_EDIT = "channel-edit";
    public static final String CHANNEL_DELETE = "channel-delete";
    public static final String CHANNEL_MOVE = "channel-move";
    public static final String CATEGORY_CREATE = "category-create";
    public static final String CATEGORY_EDIT = "category-edit";
    public static final String CATEGORY_DELETE = "category-delete";
    public static final String VOICE_STATUS = "voice-status";
    public static final String EVENT_LIST = "event-list";
    public static final String EVENT_CREATE = "event-create";
    public static final String TIMEOUT = "timeout";
    public static final String KICK = "kick";
    public static final String BAN = "ban";
    public static final String SET_PRESENCE = "set-presence";

    /** All action name values in order. */
    public static final List<String> ALL = List.of(
            SEND, BROADCAST, POLL, REACT, REACTIONS, READ, EDIT, UNSEND, REPLY,
            SEND_WITH_EFFECT, RENAME_GROUP, SET_GROUP_ICON, ADD_PARTICIPANT,
            REMOVE_PARTICIPANT, LEAVE_GROUP, SEND_ATTACHMENT, DELETE, PIN, UNPIN,
            LIST_PINS, PERMISSIONS, THREAD_CREATE, THREAD_LIST, THREAD_REPLY,
            SEARCH, STICKER, STICKER_SEARCH, MEMBER_INFO, ROLE_INFO, EMOJI_LIST,
            EMOJI_UPLOAD, STICKER_UPLOAD, ROLE_ADD, ROLE_REMOVE, CHANNEL_INFO,
            CHANNEL_LIST, CHANNEL_CREATE, CHANNEL_EDIT, CHANNEL_DELETE, CHANNEL_MOVE,
            CATEGORY_CREATE, CATEGORY_EDIT, CATEGORY_DELETE, VOICE_STATUS,
            EVENT_LIST, EVENT_CREATE, TIMEOUT, KICK, BAN, SET_PRESENCE);
}

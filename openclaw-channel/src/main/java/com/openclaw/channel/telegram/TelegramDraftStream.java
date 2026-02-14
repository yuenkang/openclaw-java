package com.openclaw.channel.telegram;

import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * Telegram draft (live-streaming) support.
 * Edits a "draft" message in real-time as the agent generates text.
 * Corresponds to TypeScript's telegram/draft-stream.ts.
 */
@Slf4j
public class TelegramDraftStream {

    private final String token;
    private final String chatId;
    private final Integer messageThreadId;
    private final AtomicReference<Integer> draftMessageId = new AtomicReference<>(null);
    private final AtomicReference<String> currentText = new AtomicReference<>("");
    private final AtomicBoolean active = new AtomicBoolean(false);
    private final int editIntervalMs;
    private Consumer<String> onComplete;

    public TelegramDraftStream(String token, String chatId, Integer messageThreadId) {
        this(token, chatId, messageThreadId, 1000);
    }

    public TelegramDraftStream(String token, String chatId, Integer messageThreadId,
            int editIntervalMs) {
        this.token = token;
        this.chatId = chatId;
        this.messageThreadId = messageThreadId;
        this.editIntervalMs = editIntervalMs;
    }

    /**
     * Start the draft stream: send an initial empty/placeholder message
     * that will be edited as content arrives.
     */
    public void start(String initialText) {
        if (active.get())
            return;
        active.set(true);
        currentText.set(initialText != null ? initialText : "▍");

        log.debug("Starting draft stream in chatId={}", chatId);
        // Send initial message and capture its message_id
        // In production, parse the response to get message_id
        TelegramSend.sendMessage(token, chatId, currentText.get(),
                null, messageThreadId, null);
    }

    /**
     * Update the draft with new text.
     */
    public void update(String text) {
        if (!active.get())
            return;
        currentText.set(text);

        if (draftMessageId.get() != null) {
            TelegramSend.editMessage(token, chatId, draftMessageId.get(), text);
        }
    }

    /**
     * Finalize the draft: edit the message with the final text and stop.
     */
    public void finalize(String finalText) {
        if (!active.get())
            return;
        active.set(false);

        String text = finalText != null ? finalText : currentText.get();
        if (draftMessageId.get() != null) {
            TelegramSend.editMessage(token, chatId, draftMessageId.get(), text);
        }

        if (onComplete != null) {
            onComplete.accept(text);
        }

        log.debug("Draft stream finalized in chatId={}", chatId);
    }

    /**
     * Cancel the draft stream.
     */
    public void cancel() {
        active.set(false);
        log.debug("Draft stream cancelled in chatId={}", chatId);
    }

    public boolean isActive() {
        return active.get();
    }

    public void setOnComplete(Consumer<String> onComplete) {
        this.onComplete = onComplete;
    }

    public void setDraftMessageId(Integer messageId) {
        this.draftMessageId.set(messageId);
    }
}

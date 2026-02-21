package com.openclaw.autoreply.reply;

/**
 * Audio-tag parsing facade — re-exports from the media layer.
 * Mirrors {@code auto-reply/reply/audio-tags.ts}.
 *
 * <p>
 * In Java callers should use {@code com.openclaw.media.AudioTags}
 * directly instead of this thin redirect.
 * </p>
 */
public final class AudioTags {
    private AudioTags() {
    }

    // Delegates to media.AudioTags.parseAudioTag — full integration deferred.
}

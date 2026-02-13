package com.openclaw.common.config;

import java.util.Date;
import java.util.List;

/**
 * Session reset policy resolution and freshness evaluation.
 * Corresponds to TypeScript's sessions/reset.ts.
 */
public final class SessionReset {

    private SessionReset() {
    }

    // =========================================================================
    // Constants
    // =========================================================================

    public static final String DEFAULT_RESET_MODE = "daily";
    public static final int DEFAULT_RESET_AT_HOUR = 4;
    public static final int DEFAULT_IDLE_MINUTES = 60;

    private static final List<String> THREAD_SESSION_MARKERS = List.of(":thread:", ":topic:");
    private static final List<String> GROUP_SESSION_MARKERS = List.of(":group:", ":channel:");

    // =========================================================================
    // Types
    // =========================================================================

    /** "daily" | "idle" */
    public record SessionResetPolicy(String mode, int atHour, Integer idleMinutes) {
    }

    /** Session freshness evaluation result. */
    public record SessionFreshness(boolean fresh, Long dailyResetAt, Long idleExpiresAt) {
    }

    /** "dm" | "group" | "thread" */
    public enum SessionResetType {
        DM, GROUP, THREAD
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Check whether a session key represents a thread session.
     */
    public static boolean isThreadSessionKey(String sessionKey) {
        String normalized = (sessionKey != null ? sessionKey : "").toLowerCase();
        if (normalized.isEmpty()) {
            return false;
        }
        return THREAD_SESSION_MARKERS.stream().anyMatch(normalized::contains);
    }

    /**
     * Resolve the reset type for a session.
     */
    public static SessionResetType resolveSessionResetType(
            String sessionKey, boolean isGroup, boolean isThread) {
        if (isThread || isThreadSessionKey(sessionKey)) {
            return SessionResetType.THREAD;
        }
        if (isGroup) {
            return SessionResetType.GROUP;
        }
        String normalized = (sessionKey != null ? sessionKey : "").toLowerCase();
        if (GROUP_SESSION_MARKERS.stream().anyMatch(normalized::contains)) {
            return SessionResetType.GROUP;
        }
        return SessionResetType.DM;
    }

    /**
     * Resolve whether a message looks like a thread.
     */
    public static boolean resolveThreadFlag(String sessionKey,
            Object messageThreadId, String threadLabel,
            String threadStarterBody, String parentSessionKey) {
        if (messageThreadId != null) {
            return true;
        }
        if (threadLabel != null && !threadLabel.trim().isEmpty()) {
            return true;
        }
        if (threadStarterBody != null && !threadStarterBody.trim().isEmpty()) {
            return true;
        }
        if (parentSessionKey != null && !parentSessionKey.trim().isEmpty()) {
            return true;
        }
        return isThreadSessionKey(sessionKey);
    }

    /**
     * Calculate the daily reset timestamp (ms) for the given time and hour.
     */
    public static long resolveDailyResetAtMs(long now, int atHour) {
        int normalizedAtHour = normalizeResetAtHour(atHour);
        @SuppressWarnings("deprecation")
        Date resetAt = new Date(now);
        resetAt.setHours(normalizedAtHour);
        resetAt.setMinutes(0);
        resetAt.setSeconds(0);
        // zero ms
        long resetMs = resetAt.getTime() / 1000 * 1000;
        if (now < resetMs) {
            resetMs -= 24 * 60 * 60 * 1000L;
        }
        return resetMs;
    }

    /**
     * Resolve the session reset policy from configuration.
     */
    public static SessionResetPolicy resolveSessionResetPolicy(
            OpenClawConfig.SessionConfig sessionCfg,
            SessionResetType resetType,
            OpenClawConfig.SessionResetConfig resetOverride) {

        OpenClawConfig.SessionResetConfig baseReset = resetOverride != null
                ? resetOverride
                : (sessionCfg != null ? sessionCfg.getReset() : null);

        OpenClawConfig.SessionResetConfig typeReset = null;
        if (resetOverride == null && sessionCfg != null && sessionCfg.getResetByType() != null) {
            typeReset = switch (resetType) {
                case DM -> sessionCfg.getResetByType().getDm();
                case GROUP -> sessionCfg.getResetByType().getGroup();
                case THREAD -> sessionCfg.getResetByType().getThread();
            };
        }

        boolean hasExplicitReset = baseReset != null
                || (sessionCfg != null && sessionCfg.getResetByType() != null);

        Integer legacyIdleMinutes = (resetOverride == null && sessionCfg != null)
                ? sessionCfg.getIdleMinutes()
                : null;

        // Resolve mode
        String mode = getFirst(
                typeReset != null ? typeReset.getMode() : null,
                baseReset != null ? baseReset.getMode() : null);
        if (mode == null) {
            mode = (!hasExplicitReset && legacyIdleMinutes != null) ? "idle" : DEFAULT_RESET_MODE;
        }

        // Resolve atHour
        int atHour = normalizeResetAtHour(firstInt(
                typeReset != null ? typeReset.getAtHour() : null,
                baseReset != null ? baseReset.getAtHour() : null,
                DEFAULT_RESET_AT_HOUR));

        // Resolve idleMinutes
        Integer idleMinutesRaw = firstInt(
                typeReset != null ? typeReset.getIdleMinutes() : null,
                baseReset != null ? baseReset.getIdleMinutes() : null,
                legacyIdleMinutes);

        Integer idleMinutes = null;
        if (idleMinutesRaw != null) {
            int normalized = idleMinutesRaw;
            if (Double.isFinite(normalized)) {
                idleMinutes = Math.max(normalized, 1);
            }
        } else if ("idle".equals(mode)) {
            idleMinutes = DEFAULT_IDLE_MINUTES;
        }

        return new SessionResetPolicy(mode, atHour, idleMinutes);
    }

    /**
     * Resolve per-channel reset config.
     */
    public static OpenClawConfig.SessionResetConfig resolveChannelResetConfig(
            OpenClawConfig.SessionConfig sessionCfg, String channel) {
        if (sessionCfg == null || sessionCfg.getResetByChannel() == null) {
            return null;
        }
        String key = normalizeChannel(channel);
        if (key == null || key.isEmpty()) {
            return null;
        }
        var byChannel = sessionCfg.getResetByChannel();
        OpenClawConfig.SessionResetConfig result = byChannel.get(key);
        if (result == null) {
            result = byChannel.get(key.toLowerCase());
        }
        return result;
    }

    /**
     * Evaluate whether a session is still fresh (not stale).
     */
    public static SessionFreshness evaluateSessionFreshness(
            long updatedAt, long now, SessionResetPolicy policy) {
        Long dailyResetAt = "daily".equals(policy.mode())
                ? resolveDailyResetAtMs(now, policy.atHour())
                : null;
        Long idleExpiresAt = policy.idleMinutes() != null
                ? updatedAt + policy.idleMinutes() * 60_000L
                : null;
        boolean staleDaily = dailyResetAt != null && updatedAt < dailyResetAt;
        boolean staleIdle = idleExpiresAt != null && now > idleExpiresAt;
        return new SessionFreshness(!(staleDaily || staleIdle), dailyResetAt, idleExpiresAt);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    public static int normalizeResetAtHour(int value) {
        if (value < 0)
            return 0;
        if (value > 23)
            return 23;
        return value;
    }

    private static int normalizeResetAtHour(Integer value) {
        if (value == null) {
            return DEFAULT_RESET_AT_HOUR;
        }
        return normalizeResetAtHour(value.intValue());
    }

    private static String getFirst(String... values) {
        for (String v : values) {
            if (v != null && !v.isBlank()) {
                return v;
            }
        }
        return null;
    }

    private static Integer firstInt(Integer... values) {
        for (Integer v : values) {
            if (v != null) {
                return v;
            }
        }
        return null;
    }

    private static String normalizeChannel(String channel) {
        if (channel == null) {
            return null;
        }
        return channel.trim().toLowerCase();
    }
}

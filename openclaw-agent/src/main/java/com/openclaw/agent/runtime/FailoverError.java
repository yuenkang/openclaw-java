package com.openclaw.agent.runtime;

import lombok.Getter;

import java.util.regex.Pattern;

/**
 * Error type for model failover scenarios.
 * Classifies errors by reason (auth, billing, rate limit, timeout, etc.)
 * to support intelligent fallback decisions.
 * Corresponds to TypeScript's failover-error.ts.
 */
@Getter
public class FailoverError extends RuntimeException {

    private final FailoverReason reason;
    private final String provider;
    private final String model;
    private final String profileId;
    private final Integer status;
    private final String code;

    public FailoverError(String message, FailoverReason reason) {
        this(message, reason, null, null, null, null, null, null);
    }

    public FailoverError(String message, FailoverReason reason,
            String provider, String model, String profileId,
            Integer status, String code, Throwable cause) {
        super(message, cause);
        this.reason = reason;
        this.provider = provider;
        this.model = model;
        this.profileId = profileId;
        this.status = status;
        this.code = code;
    }

    // =========================================================================
    // Reason enum
    // =========================================================================

    public enum FailoverReason {
        AUTH,
        BILLING,
        RATE_LIMIT,
        TIMEOUT,
        FORMAT,
        CONTEXT_OVERFLOW,
        SERVER_ERROR,
        UNKNOWN
    }

    // =========================================================================
    // Static helpers
    // =========================================================================

    private static final Pattern TIMEOUT_HINT = Pattern.compile(
            "timeout|timed out|deadline exceeded|context deadline exceeded",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern BILLING_HINT = Pattern.compile(
            "billing|payment required|insufficient.*(funds|credit|balance)",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern RATE_LIMIT_HINT = Pattern.compile("rate.?limit|too many requests|throttl",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern AUTH_HINT = Pattern.compile(
            "unauthorized|forbidden|invalid.*(api.?key|token|credential)|authentication",
            Pattern.CASE_INSENSITIVE);
    private static final Pattern CONTEXT_HINT = Pattern.compile(
            "context.*(length|window|limit)|maximum.*tokens|too long|input too large",
            Pattern.CASE_INSENSITIVE);

    /**
     * Check if this is a FailoverError.
     */
    public static boolean isFailoverError(Throwable err) {
        return err instanceof FailoverError;
    }

    /**
     * Check if the error is a timeout.
     */
    public static boolean isTimeoutError(Throwable err) {
        if (err == null)
            return false;
        String name = err.getClass().getSimpleName();
        if ("TimeoutException".equals(name) || "SocketTimeoutException".equals(name)) {
            return true;
        }
        String msg = err.getMessage();
        if (msg != null && TIMEOUT_HINT.matcher(msg).find()) {
            return true;
        }
        // Check cause
        if (err.getCause() != null && err.getCause() != err) {
            return isTimeoutError(err.getCause());
        }
        return false;
    }

    /**
     * Resolve the HTTP status code for a failover reason.
     */
    public static Integer resolveFailoverStatus(FailoverReason reason) {
        return switch (reason) {
            case BILLING -> 402;
            case RATE_LIMIT -> 429;
            case AUTH -> 401;
            case TIMEOUT -> 408;
            case FORMAT -> 400;
            case SERVER_ERROR -> 500;
            default -> null;
        };
    }

    /**
     * Extract HTTP status from an exception (if available).
     */
    public static Integer getStatusCode(Throwable err) {
        if (err instanceof FailoverError fe)
            return fe.getStatus();
        // Check for statusCode field via reflection-free approach
        if (err instanceof com.openclaw.agent.models.ModelFallbackRunner.HttpStatusException hse) {
            return hse.getStatusCode();
        }
        return null;
    }

    /**
     * Resolve the failover reason from an arbitrary error.
     */
    public static FailoverReason resolveReasonFromError(Throwable err) {
        if (err instanceof FailoverError fe)
            return fe.getReason();

        // By status code
        Integer status = getStatusCode(err);
        if (status != null) {
            if (status == 402)
                return FailoverReason.BILLING;
            if (status == 429)
                return FailoverReason.RATE_LIMIT;
            if (status == 401 || status == 403)
                return FailoverReason.AUTH;
            if (status == 408)
                return FailoverReason.TIMEOUT;
            if (status >= 500)
                return FailoverReason.SERVER_ERROR;
        }

        // By message pattern
        if (isTimeoutError(err))
            return FailoverReason.TIMEOUT;

        String msg = err.getMessage();
        if (msg != null) {
            if (BILLING_HINT.matcher(msg).find())
                return FailoverReason.BILLING;
            if (RATE_LIMIT_HINT.matcher(msg).find())
                return FailoverReason.RATE_LIMIT;
            if (AUTH_HINT.matcher(msg).find())
                return FailoverReason.AUTH;
            if (CONTEXT_HINT.matcher(msg).find())
                return FailoverReason.CONTEXT_OVERFLOW;
        }

        return null;
    }

    /**
     * Coerce any error into a FailoverError if it has a recognizable reason.
     * Returns null if the error is not failover-eligible.
     */
    public static FailoverError coerceToFailoverError(
            Throwable err, String provider, String model, String profileId) {
        if (err instanceof FailoverError fe)
            return fe;

        FailoverReason reason = resolveReasonFromError(err);
        if (reason == null)
            return null;

        String message = err.getMessage() != null ? err.getMessage() : err.getClass().getSimpleName();
        Integer status = getStatusCode(err);
        if (status == null)
            status = resolveFailoverStatus(reason);

        String code = null;
        if (err instanceof com.openclaw.agent.models.ModelFallbackRunner.HttpStatusException hse) {
            code = hse.getCode();
        }

        return new FailoverError(message, reason, provider, model, profileId,
                status, code, err);
    }

    /**
     * Describe an error for logging/display.
     */
    public static ErrorDescription describeError(Throwable err) {
        if (err instanceof FailoverError fe) {
            return new ErrorDescription(fe.getMessage(), fe.getReason(), fe.getStatus(), fe.getCode());
        }
        String message = err.getMessage() != null ? err.getMessage() : String.valueOf(err);
        return new ErrorDescription(message, resolveReasonFromError(err),
                getStatusCode(err), null);
    }

    public record ErrorDescription(
            String message,
            FailoverReason reason,
            Integer status,
            String code) {
    }
}

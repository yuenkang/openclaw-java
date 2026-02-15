package com.openclaw.gateway.net;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * HTTP request guard with SSRF protection, redirect tracking, and timeout
 * control.
 * Wraps {@link java.net.http.HttpClient} with security checks before each
 * request.
 * Corresponds to TypeScript's infra/net/fetch-guard.ts.
 */
public final class FetchGuard {

    private FetchGuard() {
    }

    private static final int DEFAULT_MAX_REDIRECTS = 3;

    // ── Options ─────────────────────────────────────────────────────────

    /**
     * Options for a guarded HTTP fetch.
     */
    public static class Options {
        private final String url;
        private int maxRedirects = DEFAULT_MAX_REDIRECTS;
        private Duration timeout;
        private SsrfGuard.Policy ssrfPolicy;
        private Map<String, String> headers;
        private String method = "GET";

        public Options(String url) {
            this.url = url;
        }

        public Options maxRedirects(int maxRedirects) {
            this.maxRedirects = Math.max(0, maxRedirects);
            return this;
        }

        public Options timeout(Duration timeout) {
            this.timeout = timeout;
            return this;
        }

        public Options ssrfPolicy(SsrfGuard.Policy policy) {
            this.ssrfPolicy = policy;
            return this;
        }

        public Options headers(Map<String, String> headers) {
            this.headers = headers;
            return this;
        }

        public Options method(String method) {
            this.method = method;
            return this;
        }
    }

    // ── Result ──────────────────────────────────────────────────────────

    /**
     * Result of a guarded fetch, including the final URL after any redirects.
     */
    public record Result(HttpResponse<String> response, String finalUrl) {
    }

    /**
     * Result with byte body.
     */
    public record ByteResult(HttpResponse<byte[]> response, String finalUrl) {
    }

    // ── Fetch ───────────────────────────────────────────────────────────

    /**
     * Perform a guarded HTTP fetch with SSRF validation and redirect tracking.
     *
     * @param options fetch options
     * @return fetch result with response and final URL
     * @throws IOException                on network errors
     * @throws InterruptedException       if interrupted
     * @throws SsrfGuard.SsrfBlockedError if SSRF check fails
     * @throws FetchGuardError            on redirect limit/loop/invalid URL
     */
    public static Result fetch(Options options) throws IOException, InterruptedException {
        HttpClient client = buildClient(options);
        Set<String> visited = new HashSet<>();
        String currentUrl = options.url;
        int redirectCount = 0;

        while (true) {
            URI uri = validateAndParseUrl(currentUrl, options.ssrfPolicy);

            HttpRequest.Builder reqBuilder = HttpRequest.newBuilder()
                    .uri(uri)
                    .method(options.method, HttpRequest.BodyPublishers.noBody());

            if (options.headers != null) {
                options.headers.forEach(reqBuilder::header);
            }
            if (options.timeout != null) {
                reqBuilder.timeout(options.timeout);
            }

            HttpResponse<String> response = client.send(reqBuilder.build(),
                    HttpResponse.BodyHandlers.ofString());

            if (isRedirectStatus(response.statusCode())) {
                String location = response.headers().firstValue("location").orElse(null);
                if (location == null || location.isEmpty()) {
                    throw new FetchGuardError(
                            "Redirect missing location header (" + response.statusCode() + ")");
                }
                redirectCount++;
                if (redirectCount > options.maxRedirects) {
                    throw new FetchGuardError("Too many redirects (limit: " + options.maxRedirects + ")");
                }
                String nextUrl = uri.resolve(location).toString();
                if (visited.contains(nextUrl)) {
                    throw new FetchGuardError("Redirect loop detected");
                }
                visited.add(nextUrl);
                currentUrl = nextUrl;
                continue;
            }

            return new Result(response, currentUrl);
        }
    }

    /**
     * Perform a guarded HTTP fetch returning raw bytes.
     */
    public static ByteResult fetchBytes(Options options) throws IOException, InterruptedException {
        HttpClient client = buildClient(options);
        Set<String> visited = new HashSet<>();
        String currentUrl = options.url;
        int redirectCount = 0;

        while (true) {
            URI uri = validateAndParseUrl(currentUrl, options.ssrfPolicy);

            HttpRequest.Builder reqBuilder = HttpRequest.newBuilder()
                    .uri(uri)
                    .method(options.method, HttpRequest.BodyPublishers.noBody());

            if (options.headers != null) {
                options.headers.forEach(reqBuilder::header);
            }
            if (options.timeout != null) {
                reqBuilder.timeout(options.timeout);
            }

            HttpResponse<byte[]> response = client.send(reqBuilder.build(),
                    HttpResponse.BodyHandlers.ofByteArray());

            if (isRedirectStatus(response.statusCode())) {
                String location = response.headers().firstValue("location").orElse(null);
                if (location == null || location.isEmpty()) {
                    throw new FetchGuardError(
                            "Redirect missing location header (" + response.statusCode() + ")");
                }
                redirectCount++;
                if (redirectCount > options.maxRedirects) {
                    throw new FetchGuardError("Too many redirects (limit: " + options.maxRedirects + ")");
                }
                String nextUrl = uri.resolve(location).toString();
                if (visited.contains(nextUrl)) {
                    throw new FetchGuardError("Redirect loop detected");
                }
                visited.add(nextUrl);
                currentUrl = nextUrl;
                continue;
            }

            return new ByteResult(response, currentUrl);
        }
    }

    // ── Helpers ─────────────────────────────────────────────────────────

    private static HttpClient buildClient(Options options) {
        HttpClient.Builder builder = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NEVER); // we handle redirects manually
        if (options.timeout != null) {
            builder.connectTimeout(options.timeout);
        }
        return builder.build();
    }

    private static URI validateAndParseUrl(String urlStr, SsrfGuard.Policy policy) {
        URI uri;
        try {
            uri = URI.create(urlStr);
        } catch (IllegalArgumentException e) {
            throw new FetchGuardError("Invalid URL: " + urlStr);
        }
        String scheme = uri.getScheme();
        if (!"http".equals(scheme) && !"https".equals(scheme)) {
            throw new FetchGuardError("Invalid URL: must be http or https");
        }
        String host = uri.getHost();
        if (host == null || host.isEmpty()) {
            throw new FetchGuardError("Invalid URL: missing host");
        }

        // SSRF check
        SsrfGuard.validateHostname(host, policy);
        return uri;
    }

    private static boolean isRedirectStatus(int status) {
        return status == 301 || status == 302 || status == 303 || status == 307 || status == 308;
    }

    // ── Error ───────────────────────────────────────────────────────────

    /**
     * Error thrown for fetch guard violations (redirect loops, invalid URLs, etc.).
     */
    public static class FetchGuardError extends RuntimeException {
        public FetchGuardError(String message) {
            super(message);
        }

        public FetchGuardError(String message, Throwable cause) {
            super(message, cause);
        }
    }
}

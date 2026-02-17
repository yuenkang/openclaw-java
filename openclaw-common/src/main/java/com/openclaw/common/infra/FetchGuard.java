package com.openclaw.common.infra;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Set;

/**
 * HTTP fetch with SSRF guard — validates target hostnames before making
 * requests.
 * Follows redirects manually to check each hop against the SSRF policy.
 * <p>
 * Corresponds to TypeScript's {@code infra/net/fetch-guard.ts}.
 */
@Slf4j
public class FetchGuard {

    private static final int DEFAULT_MAX_REDIRECTS = 3;
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);
    private static final Set<Integer> REDIRECT_STATUSES = Set.of(301, 302, 303, 307, 308);

    /**
     * Result of a guarded fetch operation.
     */
    public record GuardedFetchResult(
            int statusCode,
            String body,
            String finalUrl,
            java.net.http.HttpHeaders headers) {
    }

    /**
     * Options for guarded fetch.
     */
    public record FetchOptions(
            String url,
            String method,
            String body,
            java.util.Map<String, String> headers,
            int maxRedirects,
            Duration timeout,
            SsrfGuard.SsrfPolicy policy) {
        public static FetchOptions get(String url) {
            return new FetchOptions(url, "GET", null, java.util.Map.of(),
                    DEFAULT_MAX_REDIRECTS, DEFAULT_TIMEOUT, SsrfGuard.SsrfPolicy.DEFAULT);
        }

        public static FetchOptions post(String url, String body) {
            return new FetchOptions(url, "POST", body, java.util.Map.of(),
                    DEFAULT_MAX_REDIRECTS, DEFAULT_TIMEOUT, SsrfGuard.SsrfPolicy.DEFAULT);
        }

        public FetchOptions withTimeout(Duration timeout) {
            return new FetchOptions(url, method, body, headers, maxRedirects, timeout, policy);
        }

        public FetchOptions withPolicy(SsrfGuard.SsrfPolicy policy) {
            return new FetchOptions(url, method, body, headers, maxRedirects, timeout, policy);
        }

        public FetchOptions withHeaders(java.util.Map<String, String> headers) {
            return new FetchOptions(url, method, body, headers, maxRedirects, timeout, policy);
        }
    }

    private static final HttpClient client = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.NEVER) // Manual redirect handling
            .build();

    /**
     * Fetch a URL with SSRF protection.
     * Each redirect hop is validated against the SSRF policy.
     *
     * @throws SsrfGuard.SsrfBlockedError if a target host is blocked
     * @throws IOException                on network errors
     */
    public static GuardedFetchResult fetch(FetchOptions options) throws IOException, InterruptedException {
        String currentUrl = options.url();
        int redirectCount = 0;
        java.util.Set<String> visited = new java.util.HashSet<>();

        while (true) {
            URI uri;
            try {
                uri = URI.create(currentUrl);
            } catch (IllegalArgumentException e) {
                throw new IOException("Invalid URL: " + currentUrl);
            }

            String scheme = uri.getScheme();
            if (scheme == null || (!scheme.equals("http") && !scheme.equals("https"))) {
                throw new IOException("Invalid URL: must be http or https");
            }

            // SSRF check on the hostname
            SsrfGuard.assertPublicHostname(uri.getHost(), options.policy());

            // Build request
            HttpRequest.Builder reqBuilder = HttpRequest.newBuilder()
                    .uri(uri)
                    .timeout(options.timeout());

            // Set method & body
            if (options.body() != null && !options.body().isEmpty()) {
                reqBuilder.method(options.method(), HttpRequest.BodyPublishers.ofString(options.body()));
            } else {
                reqBuilder.method(options.method(), HttpRequest.BodyPublishers.noBody());
            }

            // Set headers
            if (options.headers() != null) {
                options.headers().forEach(reqBuilder::header);
            }

            HttpResponse<String> response = client.send(reqBuilder.build(),
                    HttpResponse.BodyHandlers.ofString());

            // Handle redirects manually
            if (REDIRECT_STATUSES.contains(response.statusCode())) {
                String location = response.headers().firstValue("location").orElse(null);
                if (location == null) {
                    throw new IOException("Redirect missing location header (" + response.statusCode() + ")");
                }

                redirectCount++;
                if (redirectCount > options.maxRedirects()) {
                    throw new IOException("Too many redirects (limit: " + options.maxRedirects() + ")");
                }

                String nextUrl = uri.resolve(location).toString();
                if (visited.contains(nextUrl)) {
                    throw new IOException("Redirect loop detected");
                }
                visited.add(nextUrl);
                currentUrl = nextUrl;
                continue;
            }

            return new GuardedFetchResult(
                    response.statusCode(),
                    response.body(),
                    currentUrl,
                    response.headers());
        }
    }

    /**
     * Convenience: simple GET with SSRF guard.
     */
    public static GuardedFetchResult get(String url) throws IOException, InterruptedException {
        return fetch(FetchOptions.get(url));
    }
}

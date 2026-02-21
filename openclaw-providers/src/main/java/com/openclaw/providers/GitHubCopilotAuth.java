package com.openclaw.providers;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import okhttp3.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * GitHub Copilot authentication: device-flow OAuth → access token → Copilot API
 * token.
 * Combines TS providers/github-copilot-auth.ts + github-copilot-token.ts.
 *
 * <p>
 * Usage flow:
 * <ol>
 * <li>Call {@link #requestDeviceCode()} and display the user_code /
 * verification_uri</li>
 * <li>Poll with {@link #pollForAccessToken(String, long, long)}</li>
 * <li>Exchange GitHub access token for a Copilot API token with
 * {@link #resolveCopilotApiToken(String)}</li>
 * </ol>
 *
 */
public class GitHubCopilotAuth {

    private static final Logger log = LoggerFactory.getLogger(GitHubCopilotAuth.class);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    // -----------------------------------------------------------------------
    // Constants
    // -----------------------------------------------------------------------

    private static final String CLIENT_ID = "Iv1.b507a08c87ecfe98";
    private static final String DEVICE_CODE_URL = "https://github.com/login/device/code";
    private static final String ACCESS_TOKEN_URL = "https://github.com/login/oauth/access_token";
    private static final String COPILOT_TOKEN_URL = "https://api.github.com/copilot_internal/v2/token";
    public static final String DEFAULT_COPILOT_API_BASE_URL = "https://api.individual.githubcopilot.com";

    private static final long TOKEN_SAFETY_MARGIN_MS = 5 * 60 * 1000; // 5 minutes

    private final OkHttpClient client;
    private final Path cacheDir;

    public GitHubCopilotAuth(OkHttpClient client, Path stateDir) {
        this.client = client;
        this.cacheDir = stateDir.resolve("credentials");
    }

    public GitHubCopilotAuth(Path stateDir) {
        this(new OkHttpClient.Builder()
                .connectTimeout(15, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build(), stateDir);
    }

    // -----------------------------------------------------------------------
    // Types
    // -----------------------------------------------------------------------

    public record DeviceCode(
            String deviceCode,
            String userCode,
            String verificationUri,
            int expiresIn,
            int interval) {
    }

    public record CopilotToken(
            String token,
            long expiresAt,
            String source,
            String baseUrl) {
    }

    public record CachedToken(String token, long expiresAt, long updatedAt) {
    }

    // -----------------------------------------------------------------------
    // Step 1: Request device code
    // -----------------------------------------------------------------------

    public DeviceCode requestDeviceCode() throws IOException {
        return requestDeviceCode("read:user");
    }

    public DeviceCode requestDeviceCode(String scope) throws IOException {
        RequestBody body = new FormBody.Builder()
                .add("client_id", CLIENT_ID)
                .add("scope", scope)
                .build();

        Request request = new Request.Builder()
                .url(DEVICE_CODE_URL)
                .post(body)
                .addHeader("Accept", "application/json")
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("GitHub device code failed: HTTP " + response.code());
            }
            JsonNode json = MAPPER.readTree(response.body().string());
            String deviceCode = json.path("device_code").asText();
            String userCode = json.path("user_code").asText();
            String verificationUri = json.path("verification_uri").asText();
            if (deviceCode.isEmpty() || userCode.isEmpty() || verificationUri.isEmpty()) {
                throw new IOException("GitHub device code response missing fields");
            }
            return new DeviceCode(
                    deviceCode, userCode, verificationUri,
                    json.path("expires_in").asInt(900),
                    json.path("interval").asInt(5));
        }
    }

    // -----------------------------------------------------------------------
    // Step 2: Poll for access token
    // -----------------------------------------------------------------------

    public String pollForAccessToken(String deviceCode, long intervalMs, long expiresAt)
            throws IOException, InterruptedException {
        FormBody bodyBase = new FormBody.Builder()
                .add("client_id", CLIENT_ID)
                .add("device_code", deviceCode)
                .add("grant_type", "urn:ietf:params:oauth:grant-type:device_code")
                .build();

        while (System.currentTimeMillis() < expiresAt) {
            Request request = new Request.Builder()
                    .url(ACCESS_TOKEN_URL)
                    .post(bodyBase)
                    .addHeader("Accept", "application/json")
                    .build();

            try (Response response = client.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new IOException("GitHub device token failed: HTTP " + response.code());
                }
                JsonNode json = MAPPER.readTree(response.body().string());

                if (json.has("access_token")) {
                    return json.get("access_token").asText();
                }

                String error = json.path("error").asText("unknown");
                switch (error) {
                    case "authorization_pending" -> Thread.sleep(intervalMs);
                    case "slow_down" -> Thread.sleep(intervalMs + 2000);
                    case "expired_token" -> throw new IOException("GitHub device code expired; run login again");
                    case "access_denied" -> throw new IOException("GitHub login cancelled");
                    default -> throw new IOException("GitHub device flow error: " + error);
                }
            }
        }
        throw new IOException("GitHub device code expired; run login again");
    }

    // -----------------------------------------------------------------------
    // Step 3: Exchange GitHub token → Copilot API token
    // -----------------------------------------------------------------------

    public CopilotToken resolveCopilotApiToken(String githubToken) throws IOException {
        // Check cache
        CachedToken cached = loadCachedToken();
        if (cached != null && isTokenUsable(cached)) {
            log.debug("Using cached Copilot token (expires at {})", cached.expiresAt);
            return new CopilotToken(cached.token, cached.expiresAt,
                    "cache:" + getCacheFilePath(),
                    deriveCopilotApiBaseUrl(cached.token));
        }

        // Fetch new token
        Request request = new Request.Builder()
                .url(COPILOT_TOKEN_URL)
                .get()
                .addHeader("Accept", "application/json")
                .addHeader("Authorization", "Bearer " + githubToken)
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Copilot token exchange failed: HTTP " + response.code());
            }
            JsonNode json = MAPPER.readTree(response.body().string());
            String token = json.path("token").asText();
            if (token.isBlank()) {
                throw new IOException("Copilot token response missing token");
            }

            long expiresAt = parseExpiresAt(json);
            CachedToken payload = new CachedToken(token, expiresAt, System.currentTimeMillis());
            saveCachedToken(payload);

            return new CopilotToken(token, expiresAt,
                    "fetched:" + COPILOT_TOKEN_URL,
                    deriveCopilotApiBaseUrl(token));
        }
    }

    // -----------------------------------------------------------------------
    // Base URL derivation
    // -----------------------------------------------------------------------

    private static final Pattern PROXY_EP_PATTERN = Pattern.compile(
            "(?:^|;)\\s*proxy-ep=([^;\\s]+)", Pattern.CASE_INSENSITIVE);

    public static String deriveCopilotApiBaseUrl(String token) {
        if (token == null || token.isBlank())
            return DEFAULT_COPILOT_API_BASE_URL;
        Matcher m = PROXY_EP_PATTERN.matcher(token);
        if (!m.find())
            return DEFAULT_COPILOT_API_BASE_URL;
        String proxyEp = m.group(1).trim();
        if (proxyEp.isEmpty())
            return DEFAULT_COPILOT_API_BASE_URL;
        String host = proxyEp.replaceFirst("^https?://", "")
                .replaceFirst("^(?i)proxy\\.", "api.");
        return host.isEmpty() ? DEFAULT_COPILOT_API_BASE_URL : "https://" + host;
    }

    // -----------------------------------------------------------------------
    // Token cache
    // -----------------------------------------------------------------------

    private Path getCacheFilePath() {
        return cacheDir.resolve("github-copilot.token.json");
    }

    private CachedToken loadCachedToken() {
        Path path = getCacheFilePath();
        if (!Files.exists(path))
            return null;
        try {
            JsonNode json = MAPPER.readTree(Files.readString(path));
            String token = json.path("token").asText();
            long expiresAt = json.path("expiresAt").asLong();
            long updatedAt = json.path("updatedAt").asLong();
            if (token.isBlank() || expiresAt == 0)
                return null;
            return new CachedToken(token, expiresAt, updatedAt);
        } catch (Exception e) {
            log.warn("Failed to read Copilot token cache: {}", e.getMessage());
            return null;
        }
    }

    private void saveCachedToken(CachedToken payload) {
        try {
            Files.createDirectories(cacheDir);
            String json = MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(payload);
            Files.writeString(getCacheFilePath(), json);
        } catch (IOException e) {
            log.warn("Failed to save Copilot token cache: {}", e.getMessage());
        }
    }

    private boolean isTokenUsable(CachedToken cached) {
        return cached.expiresAt - System.currentTimeMillis() > TOKEN_SAFETY_MARGIN_MS;
    }

    private long parseExpiresAt(JsonNode json) {
        JsonNode expiresAt = json.get("expires_at");
        if (expiresAt == null)
            throw new IllegalArgumentException("Copilot token response missing expires_at");
        long value = expiresAt.isNumber() ? expiresAt.asLong() : Long.parseLong(expiresAt.asText());
        // GitHub returns unix seconds; accept ms if > 10 billion
        return value > 10_000_000_000L ? value : value * 1000;
    }
}

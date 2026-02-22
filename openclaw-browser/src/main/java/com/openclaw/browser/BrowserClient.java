package com.openclaw.browser;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * HTTP client for the browser control server.
 * Corresponds to TypeScript's browser/client.ts + client-actions.ts.
 *
 * <p>
 * All methods call the local browser control HTTP server (default:
 * http://127.0.0.1:{controlPort}).
 * </p>
 */
@Slf4j
public class BrowserClient {

    private static final MediaType JSON_TYPE = MediaType.parse("application/json");
    private static final ObjectMapper mapper = new ObjectMapper();

    private final String baseUrl;
    private final OkHttpClient httpClient;

    public BrowserClient(String baseUrl) {
        this.baseUrl = baseUrl != null ? baseUrl.replaceAll("/$", "") : "http://127.0.0.1:8367";
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(5, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(10, TimeUnit.SECONDS)
                .build();
    }

    // ===== Basic =====

    /** GET / — browser status */
    public BrowserTypes.BrowserStatus status(String profile) throws IOException {
        return get("/", profileQuery(profile), BrowserTypes.BrowserStatus.class);
    }

    /** POST /start — start browser */
    public void start(String profile, boolean headless) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("headless", headless);
        post("/start", profileQuery(profile), body);
    }

    /** POST /stop — stop browser */
    public void stop(String profile) throws IOException {
        post("/stop", profileQuery(profile), null);
    }

    /** GET /profiles — list profiles */
    public List<BrowserTypes.ProfileStatus> profiles() throws IOException {
        return getList("/profiles", null, new TypeReference<>() {
        });
    }

    // ===== Tabs =====

    /** GET /tabs — list tabs */
    public List<BrowserTypes.BrowserTab> tabs(String profile) throws IOException {
        JsonNode node = get("/tabs", profileQuery(profile), JsonNode.class);
        if (node.has("tabs")) {
            return mapper.convertValue(node.get("tabs"), new TypeReference<>() {
            });
        }
        return mapper.convertValue(node, new TypeReference<>() {
        });
    }

    /** POST /tabs/open — open new tab */
    public BrowserTypes.BrowserTab openTab(String url, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("url", url);
        return post("/tabs/open", profileQuery(profile), body, BrowserTypes.BrowserTab.class);
    }

    /** POST /tabs/focus — focus a tab */
    public void focusTab(String targetId, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("targetId", targetId);
        post("/tabs/focus", profileQuery(profile), body);
    }

    /** DELETE /tabs/{targetId} — close a tab */
    public void closeTab(String targetId, String profile) throws IOException {
        delete("/tabs/" + targetId, profileQuery(profile));
    }

    // ===== Snapshot =====

    /** GET /snapshot — page snapshot */
    public BrowserTypes.SnapshotResult snapshot(Map<String, Object> opts) throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        for (Map.Entry<String, Object> e : opts.entrySet()) {
            if (e.getValue() != null) {
                query.put(e.getKey(), String.valueOf(e.getValue()));
            }
        }
        return get("/snapshot", query, BrowserTypes.SnapshotResult.class);
    }

    // ===== Screenshot =====

    /** POST /screenshot — take screenshot */
    public BrowserTypes.ScreenshotResult screenshot(Map<String, Object> body, String profile)
            throws IOException {
        if (profile != null)
            body.put("profile", profile);
        return post("/screenshot", null, body, BrowserTypes.ScreenshotResult.class);
    }

    // ===== Navigate =====

    /** POST /navigate — navigate to URL */
    public BrowserTypes.NavigateResult navigate(String url, String targetId, String profile)
            throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("url", url);
        if (targetId != null)
            body.put("targetId", targetId);
        if (profile != null)
            body.put("profile", profile);
        return post("/navigate", null, body, BrowserTypes.NavigateResult.class);
    }

    // ===== Console =====

    /** GET /console — get console messages */
    public JsonNode consoleMessages(String level, String targetId, String profile)
            throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        if (level != null)
            query.put("level", level);
        if (targetId != null)
            query.put("targetId", targetId);
        if (profile != null)
            query.put("profile", profile);
        return get("/console", query, JsonNode.class);
    }

    // ===== Act =====

    /** POST /act — perform an action */
    public JsonNode act(Map<String, Object> request, String profile) throws IOException {
        if (profile != null)
            request.put("profile", profile);
        return post("/act", null, request, JsonNode.class);
    }

    // ===== PDF =====

    /** POST /pdf — save page as PDF */
    public BrowserTypes.PdfResult pdf(String targetId, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        if (targetId != null)
            body.put("targetId", targetId);
        if (profile != null)
            body.put("profile", profile);
        return post("/pdf", null, body, BrowserTypes.PdfResult.class);
    }

    // ===== File chooser (upload) =====

    /** POST /hooks/file-chooser — arm file chooser */
    public JsonNode armFileChooser(Map<String, Object> body) throws IOException {
        return post("/hooks/file-chooser", null, body, JsonNode.class);
    }

    // ===== Dialog =====

    /** POST /hooks/dialog — arm dialog handler */
    public JsonNode armDialog(Map<String, Object> body) throws IOException {
        return post("/hooks/dialog", null, body, JsonNode.class);
    }

    // ===== Profile CRUD =====

    /** POST /profiles/reset — reset a profile's user data */
    public JsonNode resetProfile(String profile) throws IOException {
        return post("/profiles/reset", profileQuery(profile), null, JsonNode.class);
    }

    /** POST /profiles/create — create a new profile */
    public JsonNode createProfile(String name, String color, String cdpUrl, String driver)
            throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("name", name);
        if (color != null) body.put("color", color);
        if (cdpUrl != null) body.put("cdpUrl", cdpUrl);
        if (driver != null) body.put("driver", driver);
        return post("/profiles/create", null, body, JsonNode.class);
    }

    /** DELETE /profiles/{name} — delete a profile */
    public JsonNode deleteProfile(String name) throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        query.put("profile", name);
        // Use POST /profiles/delete instead of HTTP DELETE for compatibility
        return post("/profiles/delete", query, null, JsonNode.class);
    }

    // ===== Tab Actions (unified) =====

    /** POST /tabs/action — unified tab action (list|new|close|select) */
    public JsonNode tabAction(String action, Integer index, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("action", action);
        if (index != null) body.put("index", index);
        if (profile != null) body.put("profile", profile);
        return post("/tabs/action", null, body, JsonNode.class);
    }

    // ===== HTTP helpers =====

    private <T> T get(String path, Map<String, String> query, Class<T> type) throws IOException {
        HttpUrl.Builder urlBuilder = HttpUrl.parse(baseUrl + path).newBuilder();
        if (query != null) {
            query.forEach(urlBuilder::addQueryParameter);
        }
        Request request = new Request.Builder()
                .url(urlBuilder.build())
                .get()
                .build();
        try (Response response = httpClient.newCall(request).execute()) {
            return handleResponse(response, type);
        }
    }

    private <T> List<T> getList(String path, Map<String, String> query, TypeReference<List<T>> type)
            throws IOException {
        HttpUrl.Builder urlBuilder = HttpUrl.parse(baseUrl + path).newBuilder();
        if (query != null) {
            query.forEach(urlBuilder::addQueryParameter);
        }
        Request request = new Request.Builder()
                .url(urlBuilder.build())
                .get()
                .build();
        try (Response response = httpClient.newCall(request).execute()) {
            String body = response.body() != null ? response.body().string() : "";
            if (!response.isSuccessful()) {
                throw new IOException("Browser server error " + response.code() + ": " + body);
            }
            return mapper.readValue(body, type);
        }
    }

    private void post(String path, Map<String, String> query, Object body) throws IOException {
        post(path, query, body, JsonNode.class);
    }

    private <T> T post(String path, Map<String, String> query, Object body, Class<T> type)
            throws IOException {
        HttpUrl.Builder urlBuilder = HttpUrl.parse(baseUrl + path).newBuilder();
        if (query != null) {
            query.forEach(urlBuilder::addQueryParameter);
        }
        RequestBody reqBody = body != null
                ? RequestBody.create(mapper.writeValueAsString(body), JSON_TYPE)
                : RequestBody.create("", JSON_TYPE);
        Request request = new Request.Builder()
                .url(urlBuilder.build())
                .post(reqBody)
                .build();
        try (Response response = httpClient.newCall(request).execute()) {
            return handleResponse(response, type);
        }
    }

    private void delete(String path, Map<String, String> query) throws IOException {
        HttpUrl.Builder urlBuilder = HttpUrl.parse(baseUrl + path).newBuilder();
        if (query != null) {
            query.forEach(urlBuilder::addQueryParameter);
        }
        Request request = new Request.Builder()
                .url(urlBuilder.build())
                .delete()
                .build();
        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                String msg = response.body() != null ? response.body().string() : "";
                throw new IOException("Browser server error " + response.code() + ": " + msg);
            }
        }
    }

    private <T> T handleResponse(Response response, Class<T> type) throws IOException {
        String body = response.body() != null ? response.body().string() : "";
        if (!response.isSuccessful()) {
            throw new IOException("Browser server error " + response.code() + ": " + body);
        }
        if (body.isEmpty() || type == Void.class) {
            return null;
        }
        return mapper.readValue(body, type);
    }

    private Map<String, String> profileQuery(String profile) {
        if (profile == null)
            return null;
        Map<String, String> q = new LinkedHashMap<>();
        q.put("profile", profile);
        return q;
    }
}

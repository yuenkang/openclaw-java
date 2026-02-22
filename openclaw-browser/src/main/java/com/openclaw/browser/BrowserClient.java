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

    // ===== Cookies =====

    /** GET /cookies — get cookies */
    public JsonNode cookies(String targetId, String profile) throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        if (targetId != null) query.put("targetId", targetId);
        if (profile != null) query.put("profile", profile);
        return get("/cookies", query, JsonNode.class);
    }

    /** POST /cookies — set a cookie */
    public JsonNode cookiesSet(Map<String, Object> cookie, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("cookie", cookie);
        if (profile != null) body.put("profile", profile);
        return post("/cookies", null, body, JsonNode.class);
    }

    /** DELETE /cookies — clear cookies */
    public void cookiesClear(String profile) throws IOException {
        delete("/cookies", profileQuery(profile));
    }

    // ===== Storage =====

    /** GET /storage — get storage entries */
    public JsonNode storageGet(String kind, String key, String targetId, String profile)
            throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        if (kind != null) query.put("kind", kind);
        if (key != null) query.put("key", key);
        if (targetId != null) query.put("targetId", targetId);
        if (profile != null) query.put("profile", profile);
        return get("/storage", query, JsonNode.class);
    }

    /** POST /storage — set a storage entry */
    public JsonNode storageSet(String kind, String key, String value, String profile)
            throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("kind", kind != null ? kind : "local");
        body.put("key", key);
        body.put("value", value);
        if (profile != null) body.put("profile", profile);
        return post("/storage", null, body, JsonNode.class);
    }

    /** DELETE /storage — clear storage */
    public void storageClear(String kind, String profile) throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        if (kind != null) query.put("kind", kind);
        if (profile != null) query.put("profile", profile);
        delete("/storage", query);
    }

    // ===== Observe =====

    /** GET /errors — get page errors */
    public JsonNode pageErrors(String targetId, String profile) throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        if (targetId != null) query.put("targetId", targetId);
        if (profile != null) query.put("profile", profile);
        return get("/errors", query, JsonNode.class);
    }

    /** GET /requests — get network requests */
    public JsonNode requests(String targetId, String filter, String profile) throws IOException {
        Map<String, String> query = new LinkedHashMap<>();
        if (targetId != null) query.put("targetId", targetId);
        if (filter != null) query.put("filter", filter);
        if (profile != null) query.put("profile", profile);
        return get("/requests", query, JsonNode.class);
    }

    /** POST /highlight — highlight element */
    public JsonNode highlight(String ref, String targetId, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("ref", ref);
        if (targetId != null) body.put("targetId", targetId);
        if (profile != null) body.put("profile", profile);
        return post("/highlight", null, body, JsonNode.class);
    }

    // ===== Trace =====

    /** POST /trace/start — start tracing */
    public JsonNode traceStart(boolean screenshots, boolean snapshots, String profile)
            throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("screenshots", screenshots);
        body.put("snapshots", snapshots);
        if (profile != null) body.put("profile", profile);
        return post("/trace/start", null, body, JsonNode.class);
    }

    /** POST /trace/stop — stop tracing */
    public JsonNode traceStop(String path, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        if (path != null) body.put("path", path);
        if (profile != null) body.put("profile", profile);
        return post("/trace/stop", null, body, JsonNode.class);
    }

    // ===== State =====

    /** POST /state/offline — set offline mode */
    public JsonNode setOffline(boolean offline, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("offline", offline);
        if (profile != null) body.put("profile", profile);
        return post("/state/offline", null, body, JsonNode.class);
    }

    /** POST /state/headers — set extra HTTP headers */
    public JsonNode setHeaders(Map<String, String> headers, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("headers", headers);
        if (profile != null) body.put("profile", profile);
        return post("/state/headers", null, body, JsonNode.class);
    }

    /** POST /state/credentials — set HTTP credentials */
    public JsonNode setHttpCredentials(String username, String password, boolean clear,
                                       String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        if (clear) body.put("clear", true);
        else {
            body.put("username", username);
            body.put("password", password);
        }
        if (profile != null) body.put("profile", profile);
        return post("/state/credentials", null, body, JsonNode.class);
    }

    /** POST /state/geolocation — set geolocation */
    public JsonNode setGeolocation(Double latitude, Double longitude, Double accuracy,
                                    boolean clear, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        if (clear) body.put("clear", true);
        else {
            body.put("latitude", latitude);
            body.put("longitude", longitude);
            if (accuracy != null) body.put("accuracy", accuracy);
        }
        if (profile != null) body.put("profile", profile);
        return post("/state/geolocation", null, body, JsonNode.class);
    }

    /** POST /state/media — emulate media */
    public JsonNode setMedia(String colorScheme, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("colorScheme", colorScheme);
        if (profile != null) body.put("profile", profile);
        return post("/state/media", null, body, JsonNode.class);
    }

    // ===== Resize =====

    /** POST /resize — resize viewport */
    public JsonNode resize(int width, int height, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("width", width);
        body.put("height", height);
        if (profile != null) body.put("profile", profile);
        return post("/resize", null, body, JsonNode.class);
    }

    // ===== Channels =====

    /** GET /channels — dual-channel status */
    public JsonNode channels(String profile) throws IOException {
        return get("/channels", profileQuery(profile), JsonNode.class);
    }

    // ===== Screenshot Labels =====

    /** POST /screenshot-labels — screenshot with labeled interactive elements */
    public JsonNode screenshotLabels(String targetId, int maxLabels, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        if (targetId != null) body.put("targetId", targetId);
        body.put("maxLabels", maxLabels);
        return post("/screenshot-labels", profileQuery(profile), body, JsonNode.class);
    }

    // ===== Response Body =====

    /** POST /response/body — capture response body matching URL pattern */
    public JsonNode responseBody(String url, String targetId, Integer timeoutMs,
                                  Integer maxChars, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("url", url);
        if (targetId != null) body.put("targetId", targetId);
        if (timeoutMs != null) body.put("timeoutMs", timeoutMs);
        if (maxChars != null) body.put("maxChars", maxChars);
        return post("/response/body", profileQuery(profile), body, JsonNode.class);
    }

    // ===== Arm Upload / Dialog =====

    /** POST /hooks/arm-upload — arm file upload (pre-register file chooser handler) */
    public JsonNode armUpload(List<String> paths, String targetId, Integer timeoutMs,
                               String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        if (paths != null) body.put("paths", paths);
        if (targetId != null) body.put("targetId", targetId);
        if (timeoutMs != null) body.put("timeoutMs", timeoutMs);
        return post("/hooks/arm-upload", profileQuery(profile), body, JsonNode.class);
    }

    /** POST /hooks/arm-dialog — arm dialog handler (pre-register accept/dismiss) */
    public JsonNode armDialogHandler(boolean accept, String promptText, String targetId,
                                      Integer timeoutMs, String profile) throws IOException {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("accept", accept);
        if (promptText != null) body.put("promptText", promptText);
        if (targetId != null) body.put("targetId", targetId);
        if (timeoutMs != null) body.put("timeoutMs", timeoutMs);
        return post("/hooks/arm-dialog", profileQuery(profile), body, JsonNode.class);
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

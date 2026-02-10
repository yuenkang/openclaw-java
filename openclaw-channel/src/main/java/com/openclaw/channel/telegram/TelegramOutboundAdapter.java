package com.openclaw.channel.telegram;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.openclaw.channel.adapter.ChannelOutboundAdapter;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.IOException;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Telegram outbound adapter using Bot API.
 * Corresponds to TypeScript's channels/plugins/outbound/telegram.ts.
 */
@Slf4j
public class TelegramOutboundAdapter implements ChannelOutboundAdapter {

    private static final String BASE_URL = "https://api.telegram.org/bot";

    private final String botToken;
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;

    public TelegramOutboundAdapter(String botToken) {
        this.botToken = botToken;
        this.objectMapper = new ObjectMapper();
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(Duration.ofSeconds(10))
                .readTimeout(Duration.ofSeconds(30))
                .build();
    }

    @Override
    public String getChannelId() {
        return "telegram";
    }

    @Override
    public DeliveryMode getDeliveryMode() {
        return DeliveryMode.DIRECT;
    }

    @Override
    public CompletableFuture<Void> sendText(OutboundTextPayload payload) {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("chat_id", payload.getTarget());
        body.put("text", payload.getText());
        body.put("parse_mode", "HTML");

        if (payload.getReplyTo() != null) {
            body.put("reply_to_message_id", payload.getReplyTo());
        }
        if (payload.getThreadId() != null) {
            body.put("message_thread_id", payload.getThreadId());
        }

        // Inline keyboard buttons
        if (payload.getButtons() != null && !payload.getButtons().isEmpty()) {
            var buttons = payload.getButtons().stream()
                    .map(b -> Map.of("text", b.getLabel(), "callback_data", b.getData()))
                    .toList();
            body.put("reply_markup", Map.of("inline_keyboard", java.util.List.of(buttons)));
        }

        return callApi("sendMessage", body);
    }

    @Override
    public CompletableFuture<Void> sendMedia(OutboundMediaPayload payload) {
        String method;
        String mediaField;

        String type = payload.getMediaType() != null ? payload.getMediaType() : "document";
        switch (type) {
            case "photo", "image" -> {
                method = "sendPhoto";
                mediaField = "photo";
            }
            case "audio" -> {
                method = "sendAudio";
                mediaField = "audio";
            }
            case "video" -> {
                method = "sendVideo";
                mediaField = "video";
            }
            default -> {
                method = "sendDocument";
                mediaField = "document";
            }
        }

        Map<String, Object> body = new LinkedHashMap<>();
        body.put("chat_id", payload.getTarget());
        body.put(mediaField, payload.getMediaUrl());
        if (payload.getCaption() != null) {
            body.put("caption", payload.getCaption());
            body.put("parse_mode", "HTML");
        }

        return callApi(method, body);
    }

    private CompletableFuture<Void> callApi(String method, Map<String, Object> body) {
        CompletableFuture<Void> future = new CompletableFuture<>();

        try {
            String url = BASE_URL + botToken + "/" + method;
            String json = objectMapper.writeValueAsString(body);

            Request request = new Request.Builder()
                    .url(url)
                    .post(RequestBody.create(json, MediaType.parse("application/json")))
                    .build();

            httpClient.newCall(request).enqueue(new Callback() {
                @Override
                public void onFailure(Call call, IOException e) {
                    log.error("Telegram API call failed: {}", e.getMessage());
                    future.completeExceptionally(e);
                }

                @Override
                public void onResponse(Call call, Response response) throws IOException {
                    try (ResponseBody responseBody = response.body()) {
                        if (!response.isSuccessful()) {
                            String error = responseBody != null ? responseBody.string() : "Unknown";
                            log.error("Telegram API error {}: {}", response.code(), error);
                            future.completeExceptionally(
                                    new RuntimeException("Telegram error " + response.code() + ": " + error));
                        } else {
                            future.complete(null);
                        }
                    }
                }
            });
        } catch (Exception e) {
            future.completeExceptionally(e);
        }

        return future;
    }
}

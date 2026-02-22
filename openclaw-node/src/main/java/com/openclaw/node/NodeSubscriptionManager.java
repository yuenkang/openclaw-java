package com.openclaw.node;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

/**
 * Bidirectional subscription manager for node ↔ session event delivery.
 * Corresponds to TypeScript's server-node-subscriptions.ts.
 *
 * <p>Thread-safe: all maps use ConcurrentHashMap; CopyOnWriteArraySet is
 * intentionally not used so we can batch-iterate without excessive copying.</p>
 */
@Slf4j
public class NodeSubscriptionManager {

    /**
     * nodeId → set of sessionKeys subscribed to that node.
     */
    private final Map<String, Set<String>> nodeSubscriptions = new ConcurrentHashMap<>();

    /**
     * sessionKey → set of nodeIds this session is subscribed to.
     */
    private final Map<String, Set<String>> sessionSubscribers = new ConcurrentHashMap<>();

    private final ObjectMapper objectMapper;

    public NodeSubscriptionManager(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    // ==================== Subscribe / Unsubscribe ====================

    /** Subscribe a session to receive events from a node. */
    public void subscribe(String nodeId, String sessionKey) {
        String nId = normalize(nodeId);
        String sKey = normalize(sessionKey);
        if (nId.isEmpty() || sKey.isEmpty()) return;

        nodeSubscriptions.computeIfAbsent(nId, k -> ConcurrentHashMap.newKeySet()).add(sKey);
        sessionSubscribers.computeIfAbsent(sKey, k -> ConcurrentHashMap.newKeySet()).add(nId);
    }

    /** Unsubscribe a session from a specific node. */
    public void unsubscribe(String nodeId, String sessionKey) {
        String nId = normalize(nodeId);
        String sKey = normalize(sessionKey);
        if (nId.isEmpty() || sKey.isEmpty()) return;

        Set<String> nodeSet = nodeSubscriptions.get(nId);
        if (nodeSet != null) {
            nodeSet.remove(sKey);
            if (nodeSet.isEmpty()) nodeSubscriptions.remove(nId);
        }

        Set<String> sessionSet = sessionSubscribers.get(sKey);
        if (sessionSet != null) {
            sessionSet.remove(nId);
            if (sessionSet.isEmpty()) sessionSubscribers.remove(sKey);
        }
    }

    /** Unsubscribe all sessions from a node (e.g. when node disconnects). */
    public void unsubscribeAll(String nodeId) {
        String nId = normalize(nodeId);
        Set<String> sessions = nodeSubscriptions.remove(nId);
        if (sessions == null) return;

        for (String sessionKey : sessions) {
            Set<String> sessionSet = sessionSubscribers.get(sessionKey);
            if (sessionSet != null) {
                sessionSet.remove(nId);
                if (sessionSet.isEmpty()) sessionSubscribers.remove(sessionKey);
            }
        }
    }

    // ==================== Event Sending ====================

    /**
     * Send an event to all nodes subscribed by a session.
     *
     * @param sessionKey the session that originated the event
     * @param event      event name
     * @param payload    event payload (will be serialized to JSON)
     * @param sendEvent  callback: (nodeId, payloadJSON) → send to node
     */
    public void sendToSession(String sessionKey,
                               String event,
                               Object payload,
                               BiConsumer<String, String> sendEvent) {
        String sKey = normalize(sessionKey);
        if (sKey.isEmpty() || sendEvent == null) return;

        Set<String> nodes = sessionSubscribers.get(sKey);
        if (nodes == null || nodes.isEmpty()) return;

        String payloadJSON = toPayloadJSON(payload);
        for (String nodeId : nodes) {
            try {
                sendEvent.accept(nodeId, payloadJSON);
            } catch (Exception e) {
                log.warn("Failed to send event {} to node {}: {}", event, nodeId, e.getMessage());
            }
        }
    }

    /**
     * Send an event to all nodes that have at least one subscriber.
     */
    public void sendToAllSubscribed(String event,
                                     Object payload,
                                     BiConsumer<String, String> sendEvent) {
        if (sendEvent == null) return;

        String payloadJSON = toPayloadJSON(payload);
        for (String nodeId : nodeSubscriptions.keySet()) {
            try {
                sendEvent.accept(nodeId, payloadJSON);
            } catch (Exception e) {
                log.warn("Failed to send event {} to node {}: {}", event, nodeId, e.getMessage());
            }
        }
    }

    /**
     * Send an event to all currently connected nodes (regardless of subscriptions).
     *
     * @param event          event name
     * @param payload        event payload
     * @param listConnected  supplier of currently connected node IDs
     * @param sendEvent      callback to send to a specific node
     */
    public void sendToAllConnected(String event,
                                    Object payload,
                                    Supplier<List<String>> listConnected,
                                    BiConsumer<String, String> sendEvent) {
        if (sendEvent == null || listConnected == null) return;

        String payloadJSON = toPayloadJSON(payload);
        for (String nodeId : listConnected.get()) {
            try {
                sendEvent.accept(nodeId, payloadJSON);
            } catch (Exception e) {
                log.warn("Failed to send event {} to node {}: {}", event, nodeId, e.getMessage());
            }
        }
    }

    /** Clear all subscriptions. */
    public void clear() {
        nodeSubscriptions.clear();
        sessionSubscribers.clear();
    }

    // ==================== Query ====================

    /** Get the set of sessions subscribed to a node. */
    public Set<String> getSubscribedSessions(String nodeId) {
        Set<String> sessions = nodeSubscriptions.get(normalize(nodeId));
        return sessions != null ? Collections.unmodifiableSet(sessions) : Set.of();
    }

    /** Get the set of nodes a session is subscribed to. */
    public Set<String> getSubscribedNodes(String sessionKey) {
        Set<String> nodes = sessionSubscribers.get(normalize(sessionKey));
        return nodes != null ? Collections.unmodifiableSet(nodes) : Set.of();
    }

    // ==================== Helpers ====================

    private String toPayloadJSON(Object payload) {
        if (payload == null) return null;
        try {
            return objectMapper.writeValueAsString(payload);
        } catch (Exception e) {
            log.warn("Failed to serialize node event payload: {}", e.getMessage());
            return null;
        }
    }

    private static String normalize(String value) {
        return value != null ? value.trim() : "";
    }
}

package com.openclaw.channel;

import lombok.Data;

/**
 * Location message normalization and formatting.
 * Corresponds to TypeScript's channels/location.ts.
 */
public final class ChannelLocation {

    private ChannelLocation() {
    }

    // =========================================================================
    // Types
    // =========================================================================

    public static final String SOURCE_PIN = "pin";
    public static final String SOURCE_PLACE = "place";
    public static final String SOURCE_LIVE = "live";

    @Data
    public static class NormalizedLocation {
        private double latitude;
        private double longitude;
        private Double accuracy;
        private String name;
        private String address;
        private Boolean isLive;
        private String source;
        private String caption;
    }

    @Data
    public static class LocationContext {
        private double locationLat;
        private double locationLon;
        private Double locationAccuracy;
        private String locationName;
        private String locationAddress;
        private String locationSource;
        private boolean locationIsLive;
    }

    // =========================================================================
    // Public API
    // =========================================================================

    /**
     * Format a normalized location into a human-readable text.
     */
    public static String formatLocationText(NormalizedLocation location) {
        String source = resolveSource(location);
        boolean isLive = resolveIsLive(location, source);
        String coords = formatCoords(location.getLatitude(), location.getLongitude());
        String accuracy = formatAccuracy(location.getAccuracy());
        String caption = location.getCaption() != null ? location.getCaption().trim() : null;

        String header;
        if (SOURCE_LIVE.equals(source) || isLive) {
            header = "🛰 Live location: " + coords + accuracy;
        } else if (location.getName() != null || location.getAddress() != null) {
            String label = buildLabel(location.getName(), location.getAddress());
            header = "📍 " + label + " (" + coords + accuracy + ")";
        } else {
            header = "📍 " + coords + accuracy;
        }

        return (caption != null && !caption.isEmpty()) ? header + "\n" + caption : header;
    }

    /**
     * Convert a normalized location to a template context map.
     */
    public static LocationContext toLocationContext(NormalizedLocation location) {
        String source = resolveSource(location);
        boolean isLive = resolveIsLive(location, source);
        LocationContext ctx = new LocationContext();
        ctx.setLocationLat(location.getLatitude());
        ctx.setLocationLon(location.getLongitude());
        ctx.setLocationAccuracy(location.getAccuracy());
        ctx.setLocationName(location.getName());
        ctx.setLocationAddress(location.getAddress());
        ctx.setLocationSource(source);
        ctx.setLocationIsLive(isLive);
        return ctx;
    }

    // =========================================================================
    // Internal
    // =========================================================================

    private static String resolveSource(NormalizedLocation location) {
        if (location.getSource() != null) {
            return location.getSource();
        }
        if (Boolean.TRUE.equals(location.getIsLive())) {
            return SOURCE_LIVE;
        }
        if (location.getName() != null || location.getAddress() != null) {
            return SOURCE_PLACE;
        }
        return SOURCE_PIN;
    }

    private static boolean resolveIsLive(NormalizedLocation location, String source) {
        if (location.getIsLive() != null) {
            return location.getIsLive();
        }
        return SOURCE_LIVE.equals(source);
    }

    private static String formatAccuracy(Double accuracy) {
        if (accuracy == null || !Double.isFinite(accuracy)) {
            return "";
        }
        return " ±" + Math.round(accuracy) + "m";
    }

    private static String formatCoords(double latitude, double longitude) {
        return String.format("%.6f, %.6f", latitude, longitude);
    }

    private static String buildLabel(String name, String address) {
        if (name != null && !name.isEmpty() && address != null && !address.isEmpty()) {
            return name + " — " + address;
        }
        if (name != null && !name.isEmpty()) {
            return name;
        }
        return address != null ? address : "";
    }
}

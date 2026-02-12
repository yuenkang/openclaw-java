package com.openclaw.agent.runtime;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Predicate;

/**
 * Human-friendly session slug generator.
 * Produces slugs like {@code brisk-harbor} or {@code calm-cedar-reef}.
 * Mirrors {@code agents/session-slug.ts}.
 */
public final class SessionSlug {

    private SessionSlug() {
    }

    private static final List<String> ADJECTIVES = List.of(
            "amber", "briny", "brisk", "calm", "clear", "cool", "crisp", "dawn",
            "delta", "ember", "faint", "fast", "fresh", "gentle", "glow", "good",
            "grand", "keen", "kind", "lucky", "marine", "mellow", "mild", "neat",
            "nimble", "nova", "oceanic", "plaid", "quick", "quiet", "rapid", "salty",
            "sharp", "swift", "tender", "tidal", "tidy", "tide", "vivid", "warm",
            "wild", "young");

    private static final List<String> NOUNS = List.of(
            "atlas", "basil", "bison", "bloom", "breeze", "canyon", "cedar", "claw",
            "cloud", "comet", "coral", "cove", "crest", "crustacean", "daisy", "dune",
            "ember", "falcon", "fjord", "forest", "glade", "gulf", "harbor", "haven",
            "kelp", "lagoon", "lobster", "meadow", "mist", "nudibranch", "nexus", "ocean",
            "orbit", "otter", "pine", "prairie", "reef", "ridge", "river", "rook",
            "sable", "sage", "seaslug", "shell", "shoal", "shore", "slug", "summit",
            "tidepool", "trail", "valley", "wharf", "willow", "zephyr");

    /** Create a session slug that doesn't collide with existing IDs. */
    public static String create(Predicate<String> isTaken) {
        Predicate<String> takenPredicate = isTaken != null ? isTaken : s -> false;

        // Try 2-word slugs with numeric suffixes
        for (int attempt = 0; attempt < 12; attempt++) {
            String base = createSlugBase(2);
            if (!takenPredicate.test(base))
                return base;
            for (int i = 2; i <= 12; i++) {
                String candidate = base + "-" + i;
                if (!takenPredicate.test(candidate))
                    return candidate;
            }
        }

        // Try 3-word slugs with numeric suffixes
        for (int attempt = 0; attempt < 12; attempt++) {
            String base = createSlugBase(3);
            if (!takenPredicate.test(base))
                return base;
            for (int i = 2; i <= 12; i++) {
                String candidate = base + "-" + i;
                if (!takenPredicate.test(candidate))
                    return candidate;
            }
        }

        // Ultimate fallback with random suffix
        String fallback = createSlugBase(3) + "-"
                + Long.toString(ThreadLocalRandom.current().nextLong() & 0xFFFFL, 36);
        if (takenPredicate.test(fallback)) {
            fallback = fallback + "-" + Long.toString(System.currentTimeMillis(), 36);
        }
        return fallback;
    }

    /** Create a slug without collision checking. */
    public static String create() {
        return create(null);
    }

    // --- Internal ---

    private static String createSlugBase(int words) {
        String adj = randomChoice(ADJECTIVES, "steady");
        String noun1 = randomChoice(NOUNS, "harbor");
        if (words > 2) {
            String noun2 = randomChoice(NOUNS, "reef");
            return adj + "-" + noun1 + "-" + noun2;
        }
        return adj + "-" + noun1;
    }

    private static String randomChoice(List<String> list, String fallback) {
        if (list.isEmpty())
            return fallback;
        return list.get(ThreadLocalRandom.current().nextInt(list.size()));
    }
}

package com.openclaw.gateway.methods;

import java.util.List;
import java.util.Map;

/**
 * Bridge interface for model catalog access.
 * Gateway module uses this to query available models without depending
 * on agent module directly.
 */
public interface ModelCatalog {

    /**
     * List all available models across all registered providers.
     *
     * @return list of model entries with at least { id, provider, name }
     */
    List<Map<String, Object>> listModels();

    /**
     * List all registered provider IDs.
     */
    List<String> listProviders();

    /**
     * List all model aliases.
     */
    Map<String, String> listAliases();
}

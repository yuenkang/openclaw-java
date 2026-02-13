package com.openclaw.gateway.protocol.schema;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Protocol schema registry — maps schema names to their Java class.
 * Mirrors the runtime registry from
 * {@code protocol/schema/protocol-schemas.ts}.
 *
 * <p>
 * Usage: {@code ProtocolSchemaRegistry.SCHEMAS.get("ChatEvent")} returns
 * {@code LogsChatSchemas.ChatEvent.class}.
 */
public final class ProtocolSchemaRegistry {

    private ProtocolSchemaRegistry() {
    }

    /** Unmodifiable mapping of protocol schema name → Java class. */
    public static final Map<String, Class<?>> SCHEMAS;

    static {
        Map<String, Class<?>> m = new LinkedHashMap<>();

        // frames (already in ProtocolTypes, referenced here for registry)
        m.put("AgentEvent", AgentSchemas.AgentEvent.class);
        m.put("SendParams", AgentSchemas.SendParams.class);
        m.put("PollParams", AgentSchemas.PollParams.class);
        m.put("AgentParams", AgentSchemas.AgentParams.class);
        m.put("AgentIdentityParams", AgentSchemas.AgentIdentityParams.class);
        m.put("AgentIdentityResult", AgentSchemas.AgentIdentityResult.class);
        m.put("AgentWaitParams", AgentSchemas.AgentWaitParams.class);
        m.put("WakeParams", AgentSchemas.WakeParams.class);

        // agents, models, skills
        m.put("AgentSummary", AgentsModelsSkillsSchemas.AgentSummary.class);
        m.put("AgentsFileEntry", AgentsModelsSkillsSchemas.AgentsFileEntry.class);
        m.put("AgentsFilesListParams", AgentsModelsSkillsSchemas.AgentsFilesListParams.class);
        m.put("AgentsFilesListResult", AgentsModelsSkillsSchemas.AgentsFilesListResult.class);
        m.put("AgentsFilesGetParams", AgentsModelsSkillsSchemas.AgentsFilesGetParams.class);
        m.put("AgentsFilesGetResult", AgentsModelsSkillsSchemas.AgentsFilesGetResult.class);
        m.put("AgentsFilesSetParams", AgentsModelsSkillsSchemas.AgentsFilesSetParams.class);
        m.put("AgentsFilesSetResult", AgentsModelsSkillsSchemas.AgentsFilesSetResult.class);
        m.put("AgentsListResult", AgentsModelsSkillsSchemas.AgentsListResult.class);
        m.put("ModelChoice", AgentsModelsSkillsSchemas.ModelChoice.class);
        m.put("ModelsListResult", AgentsModelsSkillsSchemas.ModelsListResult.class);
        m.put("SkillsStatusParams", AgentsModelsSkillsSchemas.SkillsStatusParams.class);
        m.put("SkillsBinsResult", AgentsModelsSkillsSchemas.SkillsBinsResult.class);
        m.put("SkillsInstallParams", AgentsModelsSkillsSchemas.SkillsInstallParams.class);
        m.put("SkillsUpdateParams", AgentsModelsSkillsSchemas.SkillsUpdateParams.class);

        // channels
        m.put("TalkModeParams", ChannelsSchemas.TalkModeParams.class);
        m.put("ChannelsStatusParams", ChannelsSchemas.ChannelsStatusParams.class);
        m.put("ChannelsStatusResult", ChannelsSchemas.ChannelsStatusResult.class);
        m.put("ChannelsLogoutParams", ChannelsSchemas.ChannelsLogoutParams.class);
        m.put("WebLoginStartParams", ChannelsSchemas.WebLoginStartParams.class);
        m.put("WebLoginWaitParams", ChannelsSchemas.WebLoginWaitParams.class);

        // config
        m.put("ConfigSetParams", ConfigSchemas.ConfigSetParams.class);
        m.put("ConfigApplyParams", ConfigSchemas.ConfigApplyParams.class);
        m.put("ConfigPatchParams", ConfigSchemas.ConfigPatchParams.class);
        m.put("ConfigSchemaResponse", ConfigSchemas.ConfigSchemaResponse.class);
        m.put("UpdateRunParams", ConfigSchemas.UpdateRunParams.class);

        // cron
        m.put("CronJob", CronSchemas.CronJob.class);
        m.put("CronListParams", CronSchemas.CronListParams.class);
        m.put("CronAddParams", CronSchemas.CronAddParams.class);
        m.put("CronUpdateParams", CronSchemas.CronUpdateParams.class);
        m.put("CronRemoveParams", CronSchemas.CronRemoveParams.class);
        m.put("CronRunParams", CronSchemas.CronRunParams.class);
        m.put("CronRunsParams", CronSchemas.CronRunsParams.class);
        m.put("CronRunLogEntry", CronSchemas.CronRunLogEntry.class);

        // devices
        m.put("DevicePairApproveParams", DevicesSchemas.DevicePairApproveParams.class);
        m.put("DevicePairRejectParams", DevicesSchemas.DevicePairRejectParams.class);
        m.put("DeviceTokenRotateParams", DevicesSchemas.DeviceTokenRotateParams.class);
        m.put("DeviceTokenRevokeParams", DevicesSchemas.DeviceTokenRevokeParams.class);
        m.put("DevicePairRequestedEvent", DevicesSchemas.DevicePairRequestedEvent.class);
        m.put("DevicePairResolvedEvent", DevicesSchemas.DevicePairResolvedEvent.class);

        // exec-approvals
        m.put("ExecApprovalsSnapshot", ExecApprovalsSchemas.ExecApprovalsSnapshot.class);
        m.put("ExecApprovalsSetParams", ExecApprovalsSchemas.ExecApprovalsSetParams.class);
        m.put("ExecApprovalsNodeGetParams", ExecApprovalsSchemas.ExecApprovalsNodeGetParams.class);
        m.put("ExecApprovalsNodeSetParams", ExecApprovalsSchemas.ExecApprovalsNodeSetParams.class);
        m.put("ExecApprovalRequestParams", ExecApprovalsSchemas.ExecApprovalRequestParams.class);
        m.put("ExecApprovalResolveParams", ExecApprovalsSchemas.ExecApprovalResolveParams.class);

        // logs-chat
        m.put("LogsTailParams", LogsChatSchemas.LogsTailParams.class);
        m.put("LogsTailResult", LogsChatSchemas.LogsTailResult.class);
        m.put("ChatHistoryParams", LogsChatSchemas.ChatHistoryParams.class);
        m.put("ChatSendParams", LogsChatSchemas.ChatSendParams.class);
        m.put("ChatAbortParams", LogsChatSchemas.ChatAbortParams.class);
        m.put("ChatInjectParams", LogsChatSchemas.ChatInjectParams.class);
        m.put("ChatEvent", LogsChatSchemas.ChatEvent.class);

        // nodes
        m.put("NodePairRequestParams", NodesSchemas.NodePairRequestParams.class);
        m.put("NodePairApproveParams", NodesSchemas.NodePairApproveParams.class);
        m.put("NodePairRejectParams", NodesSchemas.NodePairRejectParams.class);
        m.put("NodePairVerifyParams", NodesSchemas.NodePairVerifyParams.class);
        m.put("NodeRenameParams", NodesSchemas.NodeRenameParams.class);
        m.put("NodeDescribeParams", NodesSchemas.NodeDescribeParams.class);
        m.put("NodeInvokeParams", NodesSchemas.NodeInvokeParams.class);
        m.put("NodeInvokeResultParams", NodesSchemas.NodeInvokeResultParams.class);
        m.put("NodeEventParams", NodesSchemas.NodeEventParams.class);
        m.put("NodeInvokeRequestEvent", NodesSchemas.NodeInvokeRequestEvent.class);

        // sessions
        m.put("SessionsListParams", SessionsSchemas.SessionsListParams.class);
        m.put("SessionsPreviewParams", SessionsSchemas.SessionsPreviewParams.class);
        m.put("SessionsResolveParams", SessionsSchemas.SessionsResolveParams.class);
        m.put("SessionsPatchParams", SessionsSchemas.SessionsPatchParams.class);
        m.put("SessionsResetParams", SessionsSchemas.SessionsResetParams.class);
        m.put("SessionsDeleteParams", SessionsSchemas.SessionsDeleteParams.class);
        m.put("SessionsCompactParams", SessionsSchemas.SessionsCompactParams.class);
        m.put("SessionsUsageParams", SessionsSchemas.SessionsUsageParams.class);

        // snapshot
        m.put("PresenceEntry", SnapshotSchemas.PresenceEntry.class);

        // wizard
        m.put("WizardStartParams", WizardSchemas.WizardStartParams.class);
        m.put("WizardNextParams", WizardSchemas.WizardNextParams.class);
        m.put("WizardCancelParams", WizardSchemas.WizardCancelParams.class);
        m.put("WizardStatusParams", WizardSchemas.WizardStatusParams.class);
        m.put("WizardStep", WizardSchemas.WizardStep.class);
        m.put("WizardNextResult", WizardSchemas.WizardNextResult.class);
        m.put("WizardStartResult", WizardSchemas.WizardStartResult.class);
        m.put("WizardStatusResult", WizardSchemas.WizardStatusResult.class);

        SCHEMAS = Collections.unmodifiableMap(m);
    }
}

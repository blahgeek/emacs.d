import type {
	ExtensionAPI,
	ProviderConfig,
	ProviderModelConfig,
} from "@earendil-works/pi-coding-agent";
import type { Api } from "@earendil-works/pi-ai";

/**
 * Stealth internal model provider.
 *
 * Only active when the STEALTH_INTERNAL_MODEL_HOST environment variable is set
 * (a bare host or a full URL). On every startup it fetches
 * `<host>/v1/models/api.json`, which contains a provider -> models map, filters
 * the models through MODEL_WHITELIST and registers them as pi providers.
 *
 * The API key is taken from the STEALTH_INTERNAL_MODEL_APIKEY environment
 * variable (referenced via pi's "$ENV_VAR" config syntax, resolved per request).
 */

const HOST_ENV = "STEALTH_INTERNAL_MODEL_HOST";
const API_KEY_REF = "$STEALTH_INTERNAL_MODEL_APIKEY";

interface WhitelistEntry {
	/** Regex matched against the remote model id. Non-matching models are ignored. */
	model: RegExp;
	/**
	 * Remote provider "type" this entry applies to. The JSON lists the same model
	 * under several provider types; only the entry whose type matches is used.
	 */
	type: string;
	/**
	 * [provider, modelTemplate] of the original model already registered in pi.
	 * The template may reference capture groups of `model` ("$1", ...). The matched
	 * model's `compat` settings are copied onto the registered model.
	 */
	original: [string, string];
}

const MODEL_WHITELIST: WhitelistEntry[] = [
	{ model: /(gpt-.*)/, type: "openai_responses", original: ["openai", "$1"] },
	{ model: /kimi-k2.*/, type: "openai", original: ["moonshotai", "kimi-k2.7-code"] },
	{ model: /kimi-k3.*/, type: "openai", original: ["moonshotai", "kimi-k3"] },
	{ model: /coding-model-.*/, type: "openai", original: ["moonshotai", "kimi-k3"] },
	{ model: /deepseek-.*flash.*/, type: "openai", original: ["deepseek", "deepseek-v4-flash"] },
];

/** Maps remote provider "type" to a pi streaming API. Unknown types are skipped. */
const TYPE_TO_API: Record<string, Api> = {
	openai: "openai-completions",
	openai_responses: "openai-responses",
	azure_openai_responses: "azure-openai-responses",
	anthropic: "anthropic-messages",
	google: "google-generative-ai",
};

interface RemoteModel {
	id: string;
	name?: string;
	limit?: { context?: number; output?: number };
	reasoning?: boolean;
	modalities?: { input?: string[]; output?: string[] };
	support_efforts?: string[];
	default_effort?: string;
}

interface RemoteProvider {
	id: string;
	name?: string;
	api: string;
	type: string;
	models?: Record<string, RemoteModel>;
}

type Compat = ProviderModelConfig["compat"];
type CompatLookup = (provider: string, modelId: string) => Compat | undefined;

const PI_THINKING_LEVELS = ["minimal", "low", "medium", "high", "xhigh", "max"] as const;

function thinkingLevelMap(
	supportEfforts: string[] | undefined,
): ProviderModelConfig["thinkingLevelMap"] | undefined {
	if (!supportEfforts || supportEfforts.length === 0) return undefined;
	const map: Record<string, string | null> = {};
	for (const level of PI_THINKING_LEVELS) {
		map[level] = supportEfforts.includes(level) ? level : null;
	}
	return map;
}

function buildModel(rm: RemoteModel, entry: WhitelistEntry, lookupCompat?: CompatLookup): ProviderModelConfig {
	const input = (rm.modalities?.input ?? ["text"]).filter(
		(m): m is "text" | "image" => m === "text" || m === "image",
	);
	const originalModelId = rm.id.replace(entry.model, entry.original[1]);
	const compat = lookupCompat?.(entry.original[0], originalModelId);
	const levelMap = thinkingLevelMap(rm.support_efforts);
	return {
		id: rm.id,
		name: rm.name ?? rm.id,
		reasoning: rm.reasoning ?? false,
		input: input.length > 0 ? input : ["text"],
		cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
		contextWindow: rm.limit?.context ?? 128000,
		maxTokens: rm.limit?.output ?? 16384,
		...(levelMap ? { thinkingLevelMap: levelMap } : {}),
		...(compat ? { compat } : {}),
	};
}

function buildProviderConfigs(
	payload: Record<string, RemoteProvider>,
	lookupCompat?: CompatLookup,
): Array<[string, ProviderConfig]> {
	const configs: Array<[string, ProviderConfig]> = [];
	for (const [remoteId, rp] of Object.entries(payload)) {
		const api = TYPE_TO_API[rp.type];
		if (!api || !rp.models) continue;
		const models: ProviderModelConfig[] = [];
		for (const rm of Object.values(rp.models)) {
			const entry = MODEL_WHITELIST.find((w) => w.type === rp.type && w.model.test(rm.id));
			if (!entry) continue;
			models.push(buildModel(rm, entry, lookupCompat));
		}
		if (models.length === 0) continue;
		configs.push([
			remoteId,
			{
				name: rp.name ?? remoteId,
				baseUrl: rp.api,
				apiKey: API_KEY_REF,
				api,
				models,
			},
		]);
	}
	return configs;
}

export default async function (pi: ExtensionAPI) {
	const host = process.env[HOST_ENV];
	if (!host) return;

	const base = (/^https?:\/\//.test(host) ? host : `https://${host}`).replace(/\/+$/, "");
	const url = `${base}/v1/models/api.json`;

	let payload: Record<string, RemoteProvider>;
	try {
		const response = await fetch(url);
		if (!response.ok) throw new Error(`HTTP ${response.status}`);
		payload = (await response.json()) as Record<string, RemoteProvider>;
	} catch (err) {
		const message = err instanceof Error ? err.message : String(err);
		pi.on("session_start", (_event, ctx) => {
			ctx.ui.notify(`stealth-provider: failed to fetch model list: ${message}`, "warning");
		});
		return;
	}

	const registeredProviderIds: string[] = [];

	const register = (lookupCompat?: CompatLookup) =>
		buildProviderConfigs(payload, lookupCompat).map(([id, config]) => {
			pi.registerProvider(id, config);
			if (!registeredProviderIds.includes(id)) registeredProviderIds.push(id);
			return config;
		});

	// Register immediately so the models are available during startup and to
	// `pi --list-models`.
	register();

	// Re-register with `compat` copied from pi's already-registered
	// providers/models (needs the model registry, available via ctx).
	pi.on("session_start", (_event, ctx) => {
		register((provider, modelId) => {
			return ctx.modelRegistry.find(provider, modelId)?.compat;
		});
	});

	pi.registerCommand("dump-stealth-provider", {
		description: "Print the full JSON of providers/models registered by stealth-provider",
		handler: async (_args, ctx) => {
			const models = ctx.modelRegistry
				.getAll()
				.filter((m) => registeredProviderIds.includes(m.provider));
			ctx.ui.notify(JSON.stringify(models, null, 2), "info");
		},
	});
}

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
   pi.registerProvider("subhub", {
      name: "Subhub",
      baseUrl: "https://s-h.stealth.internal/v1",
      apiKey: "Q_STEALTH_API_KEY",
      api: "openai-responses",
      headers: { "User-Agent": "codex_cli_rs/0.132.0 (Linux 7.0.5; aarch64) pi-compat" },
      models: [
         {
            id: "gpt-5.5",
            name: "GPT-5.5 (Subhub)",
            reasoning: true,
            input: ["text", "image"],
            cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
            contextWindow: 1047576,
            maxTokens: 65536,
            compat: {
               sendSessionIdHeader: false,
               supportsLongCacheRetention: false,
            },
         },
      ],
   });

   pi.on("before_provider_request", (event, ctx) => {
      if (ctx.model.provider !== "subhub") return;

      const payload = event.payload as Record<string, unknown>;
      const input = payload.input as Array<Record<string, unknown>>;
      if (!input || !Array.isArray(input)) return;

      // Move system/developer message to top-level instructions
      const systemIndex = input.findIndex(
         (msg) => msg.role === "system" || msg.role === "developer",
      );
      if (systemIndex !== -1) {
         payload.instructions = (input[systemIndex] as Record<string, unknown>).content;
         input.splice(systemIndex, 1);
      }
      if (!payload.instructions) {
         payload.instructions = "You are a helpful assistant.";
      }

      // Strip parameters the subhub proxy rejects
      delete payload.prompt_cache_retention;
      delete payload.prompt_cache_key;
      delete payload.max_output_tokens;

      return payload;
   });
}

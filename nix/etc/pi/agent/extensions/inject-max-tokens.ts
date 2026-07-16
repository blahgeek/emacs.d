import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

// fixes https://github.com/earendil-works/pi/issues/5595
// coding-model-* default max tokens is very small
export default function (pi: ExtensionAPI) {
  pi.on("before_provider_request", (event, ctx) => {
    const p = event.payload as Record<string, unknown> | null;
    if (!p || typeof p !== "object") return;

    if (ctx.model?.provider === "stealth-openai" && typeof p.model === "string" && p.model.includes("-vibe")) {
      return {...p, "max_tokens": 128000};
    }
    return p;
  });
}

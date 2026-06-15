/**
 * Retry on empty assistant response.
 *
 * Some providers (observed on kh-stealth-openai gpt-5.5 via openai-responses API)
 * occasionally return a "completed" response with no content and zero usage:
 *
 *   message: {
 *     role: "assistant",
 *     content: [],
 *     usage: { input: 0, output: 0, totalTokens: 0, ... },
 *     stopReason: "stop",
 *     responseId: "resp_..."
 *   }
 *
 * pi's harness treats this as a normal stop and the agent goes idle, which
 * looks to the user like the model "silently died". This extension detects
 * that exact shape and re-triggers the turn up to MAX_RETRIES times. The
 * spurious empty assistant frames are also filtered out of the context sent
 * to the provider on subsequent calls, so they don't pollute history.
 *
 * Install (project-local): `.pi/extensions/retry-empty-response.ts`
 * Install (global):        `~/.pi/agent/extensions/retry-empty-response.ts`
 * Quick test:              `pi -e ./retry-empty-response.ts`
 *
 * After installing into an auto-discovered location, run `/reload` in pi.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const MAX_RETRIES = 10;
const RETRY_DELAY_BASE_MS = 1000;

/**
 * Number of consecutive spurious empty assistant responses in the current retry
 * chain. Do not key this by turnIndex: pi increments turnIndex for each LLM
 * call and resets it on each agent_start, so it is not a stable retry-chain id.
 */
let consecutiveEmptyRetries = 0;

function sleep(ms: number, signal?: AbortSignal): Promise<boolean> {
	if (signal?.aborted) return Promise.resolve(false);

	return new Promise((resolve) => {
		let timeout: ReturnType<typeof setTimeout> | undefined;

		const cleanup = () => {
			if (timeout !== undefined) clearTimeout(timeout);
			signal?.removeEventListener("abort", onAbort);
		};

		const onAbort = () => {
			cleanup();
			resolve(false);
		};

		timeout = setTimeout(() => {
			cleanup();
			resolve(true);
		}, ms);

		signal?.addEventListener("abort", onAbort, { once: true });
	});
}

function isSpuriousEmptyAssistant(msg: any): boolean {
	if (!msg || msg.role !== "assistant") return false;

	// Any real content (text, tool call, or non-empty thinking) disqualifies.
	const content = Array.isArray(msg.content) ? msg.content : [];
	const hasRealContent = content.some((c: any) => {
		if (!c || typeof c !== "object") return false;
		if (c.type === "toolCall") return true;
		if (c.type === "text" && typeof c.text === "string" && c.text.trim()) return true;
		if (c.type === "thinking" && typeof c.thinking === "string" && c.thinking.trim()) return true;
		return false;
	});
	if (hasRealContent) return false;

	// Server-reported usage must look "all zero" — this is the smoking gun.
	const u = msg.usage ?? {};
	const totals = [u.totalTokens, u.input, u.output, u.cacheRead, u.cacheWrite];
	const allZeroOrMissing = totals.every((v) => !v);
	if (!allZeroOrMissing) return false;

	// stopReason should be a plain "stop" — not "toolUse", "maxTokens", "error", ...
	if (msg.stopReason && msg.stopReason !== "stop" && msg.stopReason !== "end_turn") {
		return false;
	}

	return true;
}

export default function (pi: ExtensionAPI) {
	pi.on("turn_end", async (event, ctx) => {
		const msg: any = (event as any).message;

		if (!isSpuriousEmptyAssistant(msg)) {
			// Healthy turn — reset the current retry chain.
			consecutiveEmptyRetries = 0;
			return;
		}

		if (consecutiveEmptyRetries >= MAX_RETRIES) {
			ctx.ui.notify(
				`Empty response again after ${consecutiveEmptyRetries} retries — giving up.`,
				"warning",
			);
			consecutiveEmptyRetries = 0;
			return;
		}

		const attempt = ++consecutiveEmptyRetries;

		const delayMs = attempt * RETRY_DELAY_BASE_MS;
		ctx.ui.notify(
			`Empty assistant response detected — retrying (${attempt}/${MAX_RETRIES}) after ${delayMs / 1000}s`,
			"warning",
		);

		const shouldRetry = await sleep(delayMs, ctx.signal);
		if (!shouldRetry) {
			consecutiveEmptyRetries = 0;
			return;
		}

		// Agent is now idle (stopReason=stop, no tool calls). Force another
		// LLM call. `deliverAs: "followUp"` is safe regardless of state, and
		// `triggerTurn: true` is what actually re-invokes the model.
		pi.sendMessage(
			{
				customType: "retry-empty-response",
				content: "",
				display: false,
				details: { reason: "spurious-empty-assistant", attempt, delayMs },
			},
			{ triggerTurn: true, deliverAs: "followUp" },
		);
	});

	// Strip the empty assistant frames and retry marker messages from the
	// context we send to the provider. They stay in the session log for
	// forensics, but the model never sees them.
	pi.on("context", async (event, _ctx) => {
		const messages = (event as any).messages.filter((m: any) => {
			if (!m) return true;

			if (m.role === "assistant" && isSpuriousEmptyAssistant(m)) {
				return false;
			}

			if (m.role === "custom" && m.customType === "retry-empty-response") {
				return false;
			}

			return true;
		});
		return { messages } as any;
	});
}

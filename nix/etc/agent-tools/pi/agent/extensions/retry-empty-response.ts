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

const MAX_RETRIES = 2;

/** Map<turnIndex, retryCount> — reset whenever a non-empty turn lands. */
const retryCounts = new Map<number, number>();

/** Ids of assistant messages we have classified as "spurious empty". */
const emptyAssistantIds = new Set<string>();

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
			// Healthy turn — clear any retry counter for this turn index.
			retryCounts.delete(event.turnIndex);
			return;
		}

		const tries = retryCounts.get(event.turnIndex) ?? 0;
		if (tries >= MAX_RETRIES) {
			ctx.ui.notify(
				`Empty response again after ${tries} retries — giving up.`,
				"warning",
			);
			retryCounts.delete(event.turnIndex);
			return;
		}

		retryCounts.set(event.turnIndex, tries + 1);

		// Mark this empty message so the `context` hook below scrubs it from
		// the payload sent to the provider on the next call.
		if (typeof msg.id === "string") emptyAssistantIds.add(msg.id);

		ctx.ui.notify(
			`Empty assistant response detected — retrying (${tries + 1}/${MAX_RETRIES})`,
			"warning",
		);

		// Agent is now idle (stopReason=stop, no tool calls). Force another
		// LLM call. `deliverAs: "followUp"` is safe regardless of state, and
		// `triggerTurn: true` is what actually re-invokes the model.
		pi.sendMessage(
			{
				customType: "retry-empty-response",
				content: "",
				display: false,
				details: { reason: "spurious-empty-assistant", attempt: tries + 1 },
			},
			{ triggerTurn: true, deliverAs: "followUp" },
		);
	});

	// Strip the empty assistant frames from the context we send to the
	// provider. They stay in the session log for forensics, but the model
	// never sees them.
	pi.on("context", async (event, _ctx) => {
		if (emptyAssistantIds.size === 0) return;
		const messages = (event as any).messages.filter((m: any) => {
			if (!m || m.role !== "assistant") return true;
			if (typeof m.id !== "string") return true;
			return !emptyAssistantIds.has(m.id);
		});
		return { messages } as any;
	});
}

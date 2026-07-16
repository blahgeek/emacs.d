/**
 * Subagent Tool - Delegate a task to a subagent with isolated context.
 *
 * Spawns a separate `pi` process for the subagent task, giving it an
 * isolated context window. The subagent uses the parent session's current model.
 *
 * Uses JSON mode to capture structured output from the subagent.
 */

import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { AgentToolResult } from "@earendil-works/pi-agent-core";
import type { Message } from "@earendil-works/pi-ai";
import { type ExtensionAPI, getMarkdownTheme } from "@earendil-works/pi-coding-agent";
import { Box, Container, Markdown, Spacer, Text } from "@earendil-works/pi-tui";
import { Type } from "typebox";

const READONLY_TOOLS = "read,grep,find,ls";

function formatTokens(count: number): string {
	if (count < 1000) return count.toString();
	if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
	if (count < 1000000) return `${Math.round(count / 1000)}k`;
	return `${(count / 1000000).toFixed(1)}M`;
}

function formatUsageStats(
	usage: {
		input: number;
		output: number;
		cacheRead: number;
		cacheWrite: number;
		cost: number;
		contextTokens?: number;
		turns?: number;
	},
	model?: string,
): string {
	const parts: string[] = [];
	if (usage.turns) parts.push(`${usage.turns} turn${usage.turns > 1 ? "s" : ""}`);
	if (usage.input) parts.push(`↑${formatTokens(usage.input)}`);
	if (usage.output) parts.push(`↓${formatTokens(usage.output)}`);
	if (usage.cacheRead) parts.push(`R${formatTokens(usage.cacheRead)}`);
	if (usage.cacheWrite) parts.push(`W${formatTokens(usage.cacheWrite)}`);
	if (usage.cost) parts.push(`$${usage.cost.toFixed(4)}`);
	if (usage.contextTokens && usage.contextTokens > 0) {
		parts.push(`ctx:${formatTokens(usage.contextTokens)}`);
	}
	if (model) parts.push(model);
	return parts.join(" ");
}

function formatToolCall(
	toolName: string,
	args: Record<string, unknown>,
	themeFg: (color: any, text: string) => string,
): string {
	const shortenPath = (p: string) => {
		const home = os.homedir();
		return p.startsWith(home) ? `~${p.slice(home.length)}` : p;
	};

	switch (toolName) {
		case "bash": {
			const command = (args.command as string) || "...";
			const preview = command.length > 60 ? `${command.slice(0, 60)}...` : command;
			return themeFg("muted", "$ ") + themeFg("toolOutput", preview);
		}
		case "read": {
			const rawPath = (args.file_path || args.path || "...") as string;
			const filePath = shortenPath(rawPath);
			const offset = args.offset as number | undefined;
			const limit = args.limit as number | undefined;
			let text = themeFg("accent", filePath);
			if (offset !== undefined || limit !== undefined) {
				const startLine = offset ?? 1;
				const endLine = limit !== undefined ? startLine + limit - 1 : "";
				text += themeFg("warning", `:${startLine}${endLine ? `-${endLine}` : ""}`);
			}
			return themeFg("muted", "read ") + text;
		}
		case "write": {
			const rawPath = (args.file_path || args.path || "...") as string;
			const filePath = shortenPath(rawPath);
			const content = (args.content || "") as string;
			const lines = content.split("\n").length;
			let text = themeFg("muted", "write ") + themeFg("accent", filePath);
			if (lines > 1) text += themeFg("dim", ` (${lines} lines)`);
			return text;
		}
		case "edit": {
			const rawPath = (args.file_path || args.path || "...") as string;
			return themeFg("muted", "edit ") + themeFg("accent", shortenPath(rawPath));
		}
		case "ls": {
			const rawPath = (args.path || ".") as string;
			return themeFg("muted", "ls ") + themeFg("accent", shortenPath(rawPath));
		}
		case "find": {
			const pattern = (args.pattern || "*") as string;
			const rawPath = (args.path || ".") as string;
			return themeFg("muted", "find ") + themeFg("accent", pattern) + themeFg("dim", ` in ${shortenPath(rawPath)}`);
		}
		case "grep": {
			const pattern = (args.pattern || "") as string;
			const rawPath = (args.path || ".") as string;
			return (
				themeFg("muted", "grep ") +
				themeFg("accent", `/${pattern}/`) +
				themeFg("dim", ` in ${shortenPath(rawPath)}`)
			);
		}
		default: {
			const argsStr = JSON.stringify(args);
			const preview = argsStr.length > 50 ? `${argsStr.slice(0, 50)}...` : argsStr;
			return themeFg("accent", toolName) + themeFg("dim", ` ${preview}`);
		}
	}
}

interface UsageStats {
	input: number;
	output: number;
	cacheRead: number;
	cacheWrite: number;
	cost: number;
	contextTokens: number;
	turns: number;
}

interface SingleResult {
	name: string;
	prompt: string;
	readonly: boolean;
	exitCode: number;
	messages: Message[];
	stderr: string;
	usage: UsageStats;
	model?: string;
	stopReason?: string;
	errorMessage?: string;
}

interface SubagentDetails {
	result: SingleResult;
}

function getFinalOutput(messages: Message[]): string {
	for (let i = messages.length - 1; i >= 0; i--) {
		const msg = messages[i];
		if (msg.role === "assistant") {
			for (const part of msg.content) {
				if (part.type === "text") return part.text;
			}
		}
	}
	return "";
}

function isFailedResult(result: SingleResult): boolean {
	return result.exitCode !== 0 || result.stopReason === "error" || result.stopReason === "aborted";
}

function getResultOutput(result: SingleResult): string {
	if (isFailedResult(result)) {
		return result.errorMessage || result.stderr || getFinalOutput(result.messages) || "(no output)";
	}
	return getFinalOutput(result.messages) || "(no output)";
}

type DisplayItem = { type: "text"; text : string } | { type: "toolCall"; name: string; args: Record<string, any> };

function getDisplayItems(messages: Message[], includeText: boolean): DisplayItem[] {
	const items: DisplayItem[] = [];
	for (const msg of messages) {
		if (msg.role === "assistant") {
			for (const part of msg.content) {
				if (part.type === "toolCall") {
					items.push({ type: "toolCall", name: part.name, args: part.arguments });
				}
				if (includeText && part.type === "text") {
					items.push({ type: "text", text: part.text });
				}
			}
		}
	}
	return items;
}

function truncatedString(text: string, maxLen: number, maxLines: number): string {
	const lines = text.split("\n").filter(x => x.trim().length > 0).slice(0, maxLines);
	text = lines.join("\n");
	if (text.length <= maxLen) {
		return text;
	}
	return text.slice(0, maxLen) + "...";
}

function getPiInvocation(args: string[]): { command: string; args: string[] } {
	const currentScript = process.argv[1];
	const isBunVirtualScript = currentScript?.startsWith("/$bunfs/root/");
	if (currentScript && !isBunVirtualScript && fs.existsSync(currentScript)) {
		return { command: process.execPath, args: [currentScript, ...args] };
	}

	const execName = path.basename(process.execPath).toLowerCase();
	const isGenericRuntime = /^(node|bun)(\.exe)?$/.test(execName);
	if (!isGenericRuntime) {
		return { command: process.execPath, args };
	}

	return { command: "pi", args };
}

type OnUpdateCallback = (partial: AgentToolResult<SubagentDetails>) => void;

interface TaskSpec {
	name: string;
	prompt: string;
	readonly: boolean;
	cwd?: string;
}

interface ModelInfo {
	provider: string | undefined;
	id: string | undefined;
}

async function runSingleAgent(
	defaultCwd: string,
	spec: TaskSpec,
	modelInfo: ModelInfo,
	signal: AbortSignal | undefined,
	onUpdate: OnUpdateCallback | undefined,
): Promise<SingleResult> {
	const args: string[] = ["--mode", "json", "-p", "--no-session"];
	if (modelInfo.provider) args.push("--provider", modelInfo.provider);
	if (modelInfo.id) args.push("--model", modelInfo.id);
	if (spec.readonly) args.push("--tools", READONLY_TOOLS);
	args.push(spec.prompt);

	const currentResult: SingleResult = {
		name: spec.name,
		prompt: spec.prompt,
		readonly: spec.readonly,
		exitCode: -1, // -1 = still running; set to the real exit code when the process closes
		messages: [],
		stderr: "",
		usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0, contextTokens: 0, turns: 0 },
		model: modelInfo.id,
	};

	const emitUpdate = () => {
		if (onUpdate) {
			onUpdate({
				content: [{ type: "text", text: getFinalOutput(currentResult.messages) || "(running...)" }],
				details: { result: currentResult },
			});
		}
	};

	let wasAborted = false;

	const exitCode = await new Promise<number>((resolve) => {
		const invocation = getPiInvocation(args);
		const proc = spawn(invocation.command, invocation.args, {
			cwd: spec.cwd ?? defaultCwd,
			shell: false,
			stdio: ["ignore", "pipe", "pipe"],
		});
		let buffer = "";

		const processLine = (line: string) => {
			if (!line.trim()) return;
			let event: any;
			try {
				event = JSON.parse(line);
			} catch {
				return;
			}

			if (event.type === "message_end" && event.message) {
				const msg = event.message as Message;
				currentResult.messages.push(msg);

				if (msg.role === "assistant") {
					currentResult.usage.turns++;
					const usage = msg.usage;
					if (usage) {
						currentResult.usage.input += usage.input || 0;
						currentResult.usage.output += usage.output || 0;
						currentResult.usage.cacheRead += usage.cacheRead || 0;
						currentResult.usage.cacheWrite += usage.cacheWrite || 0;
						currentResult.usage.cost += usage.cost?.total || 0;
						currentResult.usage.contextTokens = usage.totalTokens || 0;
					}
					if (!currentResult.model && msg.model) currentResult.model = msg.model;
					if (msg.stopReason) currentResult.stopReason = msg.stopReason;
					if (msg.errorMessage) currentResult.errorMessage = msg.errorMessage;
				}
				emitUpdate();
			}

			if (event.type === "tool_result_end" && event.message) {
				currentResult.messages.push(event.message as Message);
				emitUpdate();
			}
		};

		proc.stdout.on("data", (data) => {
			buffer += data.toString();
			const lines = buffer.split("\n");
			buffer = lines.pop() || "";
			for (const line of lines) processLine(line);
		});

		proc.stderr.on("data", (data) => {
			currentResult.stderr += data.toString();
		});

		proc.on("close", (code) => {
			if (buffer.trim()) processLine(buffer);
			resolve(code ?? 0);
		});

		proc.on("error", () => {
			resolve(1);
		});

		if (signal) {
			const killProc = () => {
				wasAborted = true;
				proc.kill("SIGTERM");
				setTimeout(() => {
					if (!proc.killed) proc.kill("SIGKILL");
				}, 5000);
			};
			if (signal.aborted) killProc();
			else signal.addEventListener("abort", killProc, { once: true });
		}
	});

	currentResult.exitCode = exitCode;
	if (wasAborted) throw new Error("Subagent was aborted");
	return currentResult;
}

const SubagentParams = Type.Object({
	name: Type.String({
		minLength: 1,
		description: "Required. A short display label identifying this subagent (e.g. its role or focus).",
	}),
	prompt: Type.String({
		minLength: 1,
		description: "Required. The full prompt/instructions given to this subagent. Must be a non-empty string.",
	}),
	readonly: Type.Optional(
		Type.Boolean({
			description: "If true, the subagent can only inspect the workspace and cannot make changes. Default: false.",
			default: false,
		}),
	),
	cwd: Type.Optional(Type.String({ description: "Working directory for the subagent process" })),
});

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "subagent",
		label: "Subagent",
		description: [
			"Delegate a task to a subagent running in a separate `pi` process with an isolated context window.",
		].join(" "),
		parameters: SubagentParams,

		async execute(_toolCallId, params, signal, onUpdate, ctx) {
			const modelInfo: ModelInfo = { provider: ctx.model?.provider, id: ctx.model?.id };

			const spec: TaskSpec = {
				name: params.name.trim() || "subagent",
				prompt: params.prompt,
				readonly: params.readonly ?? false,
				cwd: params.cwd,
			};

			const result = await runSingleAgent(ctx.cwd, spec, modelInfo, signal, onUpdate);

			if (isFailedResult(result)) {
				return {
					content: [
						{ type: "text", text: `Subagent ${result.stopReason || "failed"}: ${getResultOutput(result)}` },
					],
					details: { result },
					isError: true,
				};
			}
			return {
				content: [{ type: "text", text: getFinalOutput(result.messages) || "(no output)" }],
				details: { result },
			};
		},

		renderCall(args, theme, context) {
			const name = args.name?.trim() || "subagent";
			const ro = args.readonly ? theme.fg("warning", " [ro]") : "";
			const container = new Container();
			container.addChild(
				new Text(theme.fg("toolTitle", theme.bold("subagent ")) + theme.fg("accent", name) + ro, 0, 0),
			);
			const prompt = args.prompt || "...";
			const shown = context.expanded ? prompt : truncatedString(prompt, 200, 3);
			const box = new Box(1, 0, (t) => theme.bg("userMessageBg", t));
			box.addChild(new Text(theme.fg("dim", shown), 0, 0));
			container.addChild(box);
			return container;
		},

		renderResult(result, { expanded }, theme, _context) {
			const details = result.details as SubagentDetails | undefined;
			const r = details?.result;
			if (!r) {
				const text = result.content[0];
				return new Text(text?.type === "text" ? `${text.text}` : "\n(no output)", 0, 0);
			}

			const mdTheme = getMarkdownTheme();
			const isRunning = r.exitCode === -1;

			const container = new Container();
			container.addChild(new Spacer(1));

			let displayItems = getDisplayItems(r.messages, /* includeText */ isRunning || !expanded);
			let skippedDisplayCount = 0;
			if (!expanded) {
				const MAX_DISPLAY_ITEMS_COUNT = 5;
				skippedDisplayCount = displayItems.length > MAX_DISPLAY_ITEMS_COUNT ? (displayItems.length - MAX_DISPLAY_ITEMS_COUNT) : 0;
				displayItems = displayItems.slice(-MAX_DISPLAY_ITEMS_COUNT);
			}

			if (skippedDisplayCount > 0) {
				container.addChild(new Text(theme.fg("muted", `... ${skippedDisplayCount} earlier items`), 0, 0));
			}
			for (const item of displayItems) {
				if (item.type === "toolCall") {
					container.addChild(
						new Text(
							theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme)),
							0,
							0,
						),
					);
				} else if (item.type === "text") {
					const text = expanded ? item.text : truncatedString(item.text, 200, 3);
					container.addChild(new Text(theme.fg("toolOutput", text), 0, 0));
				}
			}

			if (!isRunning && expanded) {
				const finalOutput = getFinalOutput(r.messages);
				if (finalOutput) {
					container.addChild(new Spacer(1));
					container.addChild(new Markdown(finalOutput.trim(), 0, 0, mdTheme));
				}
			}

			let statusText = isRunning
				? theme.fg("warning", "⏳")
				: isFailedResult(r)
				? theme.fg("error", "✗")
				: theme.fg("success", "✓");
			if (isRunning) {
				statusText += ` ${theme.fg("muted", "(running...)")}`;
			} else {
				const usageStr = formatUsageStats(r.usage, r.model);
				if (usageStr) {
					statusText += " " + theme.fg("dim", usageStr);
				}
			}
			if (!expanded) {
				statusText += `${theme.fg("muted", " (Ctrl+O to expand)")}`;
			}
			container.addChild(new Spacer(1));
			container.addChild(new Text(statusText, 0, 0));
			return container;
		},
	});
}

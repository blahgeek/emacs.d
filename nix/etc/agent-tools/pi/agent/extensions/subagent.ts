/**
 * Subagent Tool - Delegate tasks to subagents with isolated context.
 *
 * Spawns a separate `pi` process for each subagent task, giving it an
 * isolated context window. Runs one or more tasks (concurrently when more
 * than one). Each subagent uses the parent session's current model.
 *
 * Uses JSON mode to capture structured output from subagents.
 */

import { spawn } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { AgentToolResult } from "@earendil-works/pi-agent-core";
import type { Message } from "@earendil-works/pi-ai";
import { type ExtensionAPI, getMarkdownTheme } from "@earendil-works/pi-coding-agent";
import { Container, Markdown, Spacer, Text } from "@earendil-works/pi-tui";
import { Type } from "typebox";

const MAX_PARALLEL_TASKS = 8;
const MAX_CONCURRENCY = 4;
const COLLAPSED_ITEM_COUNT = 10;
const PER_TASK_OUTPUT_CAP = 50 * 1024;
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
	results: SingleResult[];
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

function truncateParallelOutput(output: string): string {
	const byteLength = Buffer.byteLength(output, "utf8");
	if (byteLength <= PER_TASK_OUTPUT_CAP) return output;

	let truncated = output.slice(0, PER_TASK_OUTPUT_CAP);
	while (Buffer.byteLength(truncated, "utf8") > PER_TASK_OUTPUT_CAP) {
		truncated = truncated.slice(0, -1);
	}
	return `${truncated}\n\n[Output truncated: ${byteLength - Buffer.byteLength(truncated, "utf8")} bytes omitted. Full output preserved in tool details.]`;
}

type DisplayItem = { type: "text"; text: string } | { type: "toolCall"; name: string; args: Record<string, any> };

function getDisplayItems(messages: Message[]): DisplayItem[] {
	const items: DisplayItem[] = [];
	for (const msg of messages) {
		if (msg.role === "assistant") {
			for (const part of msg.content) {
				if (part.type === "text") items.push({ type: "text", text: part.text });
				else if (part.type === "toolCall") items.push({ type: "toolCall", name: part.name, args: part.arguments });
			}
		}
	}
	return items;
}

async function mapWithConcurrencyLimit<TIn, TOut>(
	items: TIn[],
	concurrency: number,
	fn: (item: TIn, index: number) => Promise<TOut>,
): Promise<TOut[]> {
	if (items.length === 0) return [];
	const limit = Math.max(1, Math.min(concurrency, items.length));
	const results: TOut[] = new Array(items.length);
	let nextIndex = 0;
	const workers = new Array(limit).fill(null).map(async () => {
		while (true) {
			const current = nextIndex++;
			if (current >= items.length) return;
			results[current] = await fn(items[current], current);
		}
	});
	await Promise.all(workers);
	return results;
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
				details: { results: [currentResult] },
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

const TaskItem = Type.Object({
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
	name: Type.String({
		minLength: 1,
		description: "Required. A short display label identifying this subagent (e.g. its role or focus).",
	}),
	cwd: Type.Optional(Type.String({ description: "Working directory for the subagent process" })),
});

const SubagentParams = Type.Object({
	tasks: Type.Array(TaskItem, {
		minItems: 1,
		description:
			"Required. A non-empty array of one or more subagent tasks. " +
			"When more than one task is given, they run concurrently.",
	}),
});

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "subagent",
		label: "Subagent",
		description: [
			"Delegate one or more tasks to subagents, each running in a separate `pi` process with an isolated context window.",
		].join(" "),
		parameters: SubagentParams,

		async execute(_toolCallId, params, signal, onUpdate, ctx) {
			const modelInfo: ModelInfo = { provider: ctx.model?.provider, id: ctx.model?.id };
			const model = modelInfo.id;

			if (!params.tasks || params.tasks.length === 0) {
				return {
					content: [{ type: "text", text: "Invalid parameters. Provide at least one task in `tasks`." }],
					details: { results: [] },
				};
			}

			if (params.tasks.length > MAX_PARALLEL_TASKS) {
				return {
					content: [
						{
							type: "text",
							text: `Too many tasks (${params.tasks.length}). Max is ${MAX_PARALLEL_TASKS}.`,
						},
					],
					details: { results: [] },
				};
			}

			const specs: TaskSpec[] = params.tasks.map((t, i) => ({
				name: t.name.trim() || (params.tasks.length > 1 ? `subagent #${i + 1}` : "subagent"),
				prompt: t.prompt,
				readonly: t.readonly ?? false,
				cwd: t.cwd,
			}));

			// Track all results for streaming updates
			const allResults: SingleResult[] = specs.map((spec) => ({
				name: spec.name,
				prompt: spec.prompt,
				readonly: spec.readonly,
				exitCode: -1, // -1 = still running
				messages: [],
				stderr: "",
				usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0, contextTokens: 0, turns: 0 },
				model,
			}));

			const emitUpdate = () => {
				if (onUpdate) {
					const running = allResults.filter((r) => r.exitCode === -1).length;
					const done = allResults.filter((r) => r.exitCode !== -1).length;
					onUpdate({
						content: [{ type: "text", text: `${done}/${allResults.length} done, ${running} running...` }],
						details: { results: [...allResults] },
					});
				}
			};

			// Emit an initial update so the running state is visible immediately,
			// before any subagent produces its first event.
			emitUpdate();

			const results = await mapWithConcurrencyLimit(specs, MAX_CONCURRENCY, async (spec, index) => {
			const result = await runSingleAgent(ctx.cwd, spec, modelInfo, signal, (partial) => {
					if (partial.details?.results[0]) {
						allResults[index] = partial.details.results[0];
						emitUpdate();
					}
				});
				allResults[index] = result;
				emitUpdate();
				return result;
			});

			const successCount = results.filter((r) => !isFailedResult(r)).length;

			if (results.length === 1) {
				const r = results[0];
				if (isFailedResult(r)) {
					return {
						content: [{ type: "text", text: `Subagent ${r.stopReason || "failed"}: ${getResultOutput(r)}` }],
						details: { results },
						isError: true,
					};
				}
				return {
					content: [{ type: "text", text: getFinalOutput(r.messages) || "(no output)" }],
					details: { results },
				};
			}

			const summaries = results.map((r) => {
				const output = truncateParallelOutput(getResultOutput(r));
				const status = isFailedResult(r)
					? `failed${r.stopReason && r.stopReason !== "end" ? ` (${r.stopReason})` : ""}`
					: "completed";
				return `### [${r.name}] ${status}\n\n${output}`;
			});
			return {
				content: [
					{
						type: "text",
						text: `${successCount}/${results.length} succeeded\n\n${summaries.join("\n\n---\n\n")}`,
					},
				],
				details: { results },
			};
		},

		renderCall(args, theme, context) {
			const tasks = args.tasks ?? [];
			const labelFor = (t: (typeof tasks)[number], i: number) =>
				t.name?.trim() || (tasks.length > 1 ? `subagent #${i + 1}` : "subagent");
			let text =
				theme.fg("toolTitle", theme.bold("subagent ")) +
				theme.fg("accent", tasks.length === 1 ? "1 task" : `${tasks.length} tasks`);
			if (context.expanded) {
				// Expanded: show every task with its full prompt.
				for (let i = 0; i < tasks.length; i++) {
					const t = tasks[i];
					const ro = t.readonly ? theme.fg("warning", " [ro]") : "";
					text += `\n  ${theme.fg("accent", labelFor(t, i))}${ro}`;
					text += `\n  ${theme.fg("dim", t.prompt || "...")}`;
				}
				return new Text(text, 0, 0);
			}
			// Collapsed: short preview of the first few tasks.
			for (let i = 0; i < Math.min(tasks.length, 3); i++) {
				const t = tasks[i];
				const preview = t.prompt && t.prompt.length > 50 ? `${t.prompt.slice(0, 50)}...` : t.prompt || "...";
				const ro = t.readonly ? theme.fg("warning", " [ro]") : "";
				text += `\n  ${theme.fg("accent", labelFor(t, i))}${ro}${theme.fg("dim", ` ${preview}`)}`;
			}
			if (tasks.length > 3) text += `\n  ${theme.fg("muted", `... +${tasks.length - 3} more`)}`;
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded }, theme, _context) {
			const details = result.details as SubagentDetails | undefined;
			if (!details || details.results.length === 0) {
				const text = result.content[0];
				return new Text(text?.type === "text" ? text.text : "\n(no output)", 0, 0);
			}

			const mdTheme = getMarkdownTheme();

			const renderDisplayItems = (items: DisplayItem[], limit?: number) => {
				const toShow = limit ? items.slice(-limit) : items;
				const skipped = limit && items.length > limit ? items.length - limit : 0;
				let text = "";
				if (skipped > 0) text += theme.fg("muted", `... ${skipped} earlier items\n`);
				for (const item of toShow) {
					if (item.type === "text") {
						const preview = expanded ? item.text : item.text.split("\n").slice(0, 3).join("\n");
						text += `${theme.fg("toolOutput", preview)}\n`;
					} else {
						text += `${theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme))}\n`;
					}
				}
				return text.trimEnd();
			};

			// Unified view for one or more subagents
			const aggregateUsage = (results: SingleResult[]) => {
				const total = { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0, turns: 0 };
				for (const r of results) {
					total.input += r.usage.input;
					total.output += r.usage.output;
					total.cacheRead += r.usage.cacheRead;
					total.cacheWrite += r.usage.cacheWrite;
					total.cost += r.usage.cost;
					total.turns += r.usage.turns;
				}
				return total;
			};

			const running = details.results.filter((r) => r.exitCode === -1).length;
			const successCount = details.results.filter((r) => r.exitCode !== -1 && !isFailedResult(r)).length;
			const failCount = details.results.filter((r) => r.exitCode !== -1 && isFailedResult(r)).length;
			const isRunning = running > 0;

			if (expanded && !isRunning) {
				const container = new Container();
				for (const r of details.results) {
					const rIcon = isFailedResult(r) ? theme.fg("error", "✗") : theme.fg("success", "✓");
					const displayItems = getDisplayItems(r.messages);
					const finalOutput = getFinalOutput(r.messages);
					const roTag = r.readonly ? theme.fg("warning", " [ro]") : "";

					container.addChild(new Spacer(1));
					container.addChild(
						new Text(`${theme.fg("muted", "─── ") + theme.fg("accent", r.name)}${roTag} ${rIcon}`, 0, 0),
					);

					for (const item of displayItems) {
						if (item.type === "toolCall") {
							container.addChild(
								new Text(
									theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme)),
									0,
									0,
								),
							);
						}
					}

					if (finalOutput) {
						container.addChild(new Spacer(1));
						container.addChild(new Markdown(finalOutput.trim(), 0, 0, mdTheme));
					}

					const taskUsage = formatUsageStats(r.usage, r.model);
					if (taskUsage) container.addChild(new Text(theme.fg("dim", taskUsage), 0, 0));
				}

				const usageStr = formatUsageStats(aggregateUsage(details.results));
				if (usageStr) {
					container.addChild(new Spacer(1));
					container.addChild(new Text(theme.fg("dim", `Total: ${usageStr}`), 0, 0));
				}
				return container;
			}

			// Collapsed view (or still running)
			let text = "\n";
			for (const r of details.results) {
				const rIcon =
					r.exitCode === -1
						? theme.fg("warning", "⏳")
						: isFailedResult(r)
							? theme.fg("error", "✗")
							: theme.fg("success", "✓");
				const displayItems = getDisplayItems(r.messages);
				const roTag = r.readonly ? theme.fg("warning", " [ro]") : "";
				text += `${theme.fg("muted", "─── ")}${theme.fg("accent", r.name)}${roTag} ${rIcon}`;
				if (displayItems.length === 0)
					text += `\n${theme.fg("muted", r.exitCode === -1 ? "(running...)" : "(no output)")}`;
				else text += `\n${renderDisplayItems(displayItems, 5)}`;
				text += "\n\n";
			}
			if (!isRunning) {
				const usageStr = formatUsageStats(aggregateUsage(details.results));
				if (usageStr) text += `${theme.fg("dim", `Total: ${usageStr}`)}`;
				text += `${theme.fg("muted", " (Ctrl+O to expand)")}`;
			}
			return new Text(text, 0, 0);
		},
	});
}

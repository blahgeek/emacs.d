/**
 * Session-scoped remote endpoints. Adding an endpoint authorizes both tools;
 * there are no per-call prompts. URLs live only in custom session entries.
 * Tools register inactive at startup and activate when endpoints are added or
 * restored. Deleting endpoints does not explicitly deactivate the tools.
 *
 * /remote-shell [list | add <name> <url> | del <name>]
 * SSH requires non-interactive authentication and normal host-key verification.
 * kubectl uses the default container; copy requires tar in that container.
 * SSH copy uses modern scp's default SFTP transport (no legacy -O mode).
 * Copy uses native scp/kubectl cp behavior, including existing destinations.
 * Cancellation stops the local client, not necessarily remote descendants.
 * This is not a secrecy sandbox: command output can disclose remote identity,
 * and an agent with unrestricted local filesystem access can read session files.
 */
import { isAbsolute } from "node:path";
import {
	createBashToolDefinition,
	createLocalBashOperations,
	defineTool,
	type AgentToolResult,
	type BashOperations,
	type BashToolDetails,
	type ExtensionAPI,
	type ExtensionContext,
	type Theme,
} from "@earendil-works/pi-coding-agent";
import { Box, Container, Text, type Component } from "@earendil-works/pi-tui";
import { Type } from "typebox";

const STATE_TYPE = "remote-shell-endpoints";
const NOTICE_TYPE = "remote-shell-availability";
const NAME = /^[A-Za-z][A-Za-z0-9_-]*$/;
const USAGE = "/remote-shell [list | add <name> <endpoint-url> | del <name>]";

type Endpoint = { name: string; url: string };
type Target =
	| { kind: "ssh"; host: string; user?: string; port?: string }
	| { kind: "kubectl"; pod: string; namespace?: string; context?: string };
type Location = { path: string; endpoint?: Endpoint };

function quote(value: string): string {
	return `'${value.replace(/'/g, "'\\''")}'`;
}

function remoteBox(component: Component, theme: Theme, isError = false): Box {
	const box = new Box(1, 0, (text) => theme.bg(isError ? "toolErrorBg" : "customMessageBg", text));
	box.addChild(component);
	return box;
}

function argvCommand(argv: string[]): string {
	return argv.map(quote).join(" ");
}

function parseEndpoint(url: string): Target {
	// Parse strictly instead of URL(), which normalizes/decodes some authorities.
	const ssh = /^ssh:\/\/(?:(?<user>[A-Za-z0-9_.-]+)@)?(?<host>\[[0-9A-Fa-f:.]+\]|[A-Za-z0-9_][A-Za-z0-9_.-]*)(?::(?<port>[0-9]+))?$/.exec(url);
	if (ssh?.groups) {
		const { user, host, port } = ssh.groups;
		if (port && (+port < 1 || +port > 65535)) throw new Error("SSH port must be between 1 and 65535.");
		return { kind: "ssh", user, host, port };
	}
	if (url.startsWith("kubectl://")) {
		const parts = url.slice("kubectl://".length).split(":");
		if (parts.length >= 1 && parts.length <= 3 && parts.every((p) => p.length > 0 && !/[\s\x00-\x1f\x7f/]/.test(p))) {
			const pod = parts.at(-1)!;
			const namespace = parts.length > 1 ? parts.at(-2) : undefined;
			const context = parts.length === 3 ? parts[0] : undefined;
			const resourceName = /^[a-z0-9](?:[a-z0-9.-]*[a-z0-9])?$/;
			if (resourceName.test(pod) && (!namespace || resourceName.test(namespace))) {
				return { kind: "kubectl", pod, namespace, context };
			}
		}
	}
	throw new Error("Expected ssh://[user@]host[:port] or kubectl://[[context:]namespace:]pod-name.");
}

function kubectlArgs(target: Extract<Target, { kind: "kubectl" }>): string[] {
	return [
		...(target.context ? [`--context=${target.context}`] : []),
		...(target.namespace ? [`--namespace=${target.namespace}`] : []),
	];
}

function sshHost(target: Extract<Target, { kind: "ssh" }>, scp = false): string {
	const host = scp ? target.host : target.host.replace(/^\[|\]$/g, "");
	return `${target.user ? `${target.user}@` : ""}${host}`;
}

function execArgv(endpoint: Endpoint, command: string): string[] {
	const target = parseEndpoint(endpoint.url);
	if (target.kind === "ssh") {
		return ["ssh", "-T", "-o", "BatchMode=yes", ...(target.port ? ["-p", target.port] : []), "--", sshHost(target), command];
	}
	return ["kubectl", ...kubectlArgs(target), "exec", target.pod, "--", "sh", "-c", command];
}

function parseLocation(value: string, endpoints: Map<string, Endpoint>): Location {
	if (isAbsolute(value)) {
		if (value.includes("\0")) throw new Error("Paths must not contain NUL.");
		return { path: value };
	}
	const match = /^([A-Za-z][A-Za-z0-9_-]*):\/\/(\/[^\0]*)$/.exec(value);
	if (!match) throw new Error("Use an absolute local path or name:///absolute/remote/path.");
	const endpoint = endpoints.get(match[1]);
	if (!endpoint) throw new Error(`Unknown remote endpoint: ${match[1]}`);
	return { path: match[2], endpoint };
}

function copyArgv(endpoint: Endpoint, src: Location, dst: Location, recursive: boolean): string[] {
	const target = parseEndpoint(endpoint.url);
	const remote = (location: Location) => {
		if (!location.endpoint) return location.path;
		// SFTP scp expands remote *source* globs even with local shell quoting.
		// Escape glob syntax, not shell syntax; destination paths are literal.
		const path = target.kind === "ssh" && location === src
			? location.path.replace(/[\\*?\[\]{}]/g, "\\$&")
			: location.path;
		return `${target.kind === "ssh" ? sshHost(target, true) : target.pod}:${path}`;
	};
	if (target.kind === "ssh") {
		return ["scp", "-B", ...(target.port ? ["-P", target.port] : []), ...(recursive ? ["-r"] : []), "--", remote(src), remote(dst)];
	}
	// kubectl cp recognizes any colon as a remote spec, even in a local path.
	if ([src, dst].some((p) => !p.endpoint && p.path.includes(":"))) {
		throw new Error("kubectl copy does not support colons in local paths.");
	}
	return ["kubectl", ...kubectlArgs(target), "cp", "--", remote(src), remote(dst)];
}

export default function remoteShell(pi: ExtensionAPI) {
	let endpoints = new Map<string, Endpoint>();
	const localOps = createLocalBashOperations();

	function restore(ctx: ExtensionContext) {
		let saved: unknown;
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === STATE_TYPE) {
				saved = (entry.data as { endpoints?: unknown } | undefined)?.endpoints;
			}
		}
		endpoints = new Map();
		for (const item of Array.isArray(saved) ? saved : []) {
			if (!item || typeof item.name !== "string" || !NAME.test(item.name) || typeof item.url !== "string") continue;
			try {
				parseEndpoint(item.url);
				endpoints.set(item.name, { name: item.name, url: item.url });
			} catch { /* Ignore malformed saved definitions, never expose their URL. */ }
		}
		if (endpoints.size > 0) ensureToolsAvailable();
		updateStatus(ctx);
	}

	function updateStatus(ctx: ExtensionContext) {
		if (ctx.hasUI) ctx.ui.setStatus("remote-shell", `🌐 remote-shell: {${[...endpoints.keys()].join(", ")}}`);
	}

	pi.on("session_start", (_event, ctx) => restore(ctx));
	pi.on("session_tree", (_event, ctx) => restore(ctx));

	pi.registerCommand("remote-shell", {
		description: "Manage session remote endpoints: list, add <name> <url>, del <name>",
		getArgumentCompletions: (prefix) => {
			const values = /^(del)\s/.test(prefix)
				? [...endpoints.keys()].map((name) => `del ${name}`)
				: ["add", "list", "del"];
			return values.filter((value) => value.startsWith(prefix)).map((value) => ({ value, label: value }));
		},
		handler: async (args, ctx) => {
			const parts = args.trim().split(/\s+/).filter(Boolean);
			if (!parts.length || (parts.length === 1 && parts[0] === "list")) {
				ctx.ui.notify(endpoints.size ? [...endpoints.values()].map((e) => `${e.name}: ${e.url}`).join("\n") : "No remote endpoints defined.", "info");
				return;
			}
			const [action, name, url] = parts;
			if (!((action === "add" && parts.length === 3) || (action === "del" && parts.length === 2))) {
				ctx.ui.notify(USAGE, "error");
				return;
			}
			if (!NAME.test(name)) {
				ctx.ui.notify("Endpoint names must start with a letter and contain only letters, digits, _ or -.", "error");
				return;
			}
			if (action === "add") {
				try { parseEndpoint(url); } catch (error) {
					ctx.ui.notify((error as Error).message, "error");
					return;
				}
				endpoints.set(name, { name, url });
			} else if (!endpoints.delete(name)) {
				ctx.ui.notify(`Unknown remote endpoint: ${name}`, "error");
				return;
			}
			pi.appendEntry(STATE_TYPE, { endpoints: [...endpoints.values()] });
			if (action === "add") ensureToolsAvailable();
			updateStatus(ctx);
			const content = `Remote-shell endpoint "${name}" is now ${action === "add" ? "available" : "unavailable"}.`;
			pi.sendMessage({ customType: NOTICE_TYPE, content, display: false }, { triggerTurn: false, deliverAs: "nextTurn" });
			ctx.ui.notify(content, "info");
		},
	});

	const bash = createBashToolDefinition(process.cwd(), { exposeSessionEnvironment: false });
	const execTool = defineTool({
		...bash,
		name: "remote-shell-exec",
		renderShell: "self",
		exposure: "direct",
		defaultActive: false,
		prepareArguments: undefined,
		label: "Remote shell",
		description: "Run a command on a named remote endpoint. The remote shell may not be bash; kubectl uses sh. Each call starts a new shell.",
		promptSnippet: "Execute shell commands on a named remote endpoint",
		parameters: Type.Object({
			endpoint: Type.String({ description: "User-defined endpoint name" }),
			command: Type.String({ description: "Shell command to execute remotely" }),
			timeout: Type.Optional(Type.Number({ minimum: 0, exclusiveMinimum: 0, description: "Timeout in seconds" })),
		}),
		renderCall(args, theme, context) {
			const container = new Container();
			container.addChild(new Text(
				theme.fg("toolTitle", theme.bold("remote-shell-exec")) +
				" " + theme.fg("accent", `[${args.endpoint ?? ""}]`), 0, 0,
			));
			const mapped = { command: args.command ?? "", timeout: args.timeout };
			container.addChild(bash.renderCall!(mapped, theme, { ...context, args: mapped, lastComponent: undefined }));
			return remoteBox(container, theme);
		},
		renderResult(result: AgentToolResult<BashToolDetails | undefined>, options, theme, context) {
			const output = bash.renderResult!(result, options, theme, { ...context, args: { command: context.args.command }, lastComponent: undefined });
			return remoteBox(output, theme, context.isError);
		},
		async execute(id, params, signal, onUpdate, ctx) {
			const endpoint = endpoints.get(params.endpoint);
			if (!endpoint) throw new Error(`Unknown remote endpoint: ${params.endpoint}`);
			const operations: BashOperations = {
				exec: (command, cwd, options) => localOps.exec(argvCommand(execArgv(endpoint, command)), cwd, options),
			};
			return createBashToolDefinition(ctx.cwd, { operations, exposeSessionEnvironment: false })
				.execute(id, { command: params.command, timeout: params.timeout }, signal, onUpdate, ctx);
		},
	});

	const copyTool = defineTool({
		...bash,
		name: "remote-shell-copy",
		renderShell: "self",
		exposure: "direct",
		defaultActive: false,
		prepareArguments: undefined,
		label: "Remote copy",
		description: "Copy between an absolute local path and name:///absolute/remote/path. Exactly one side must be remote. Existing destinations follow native scp/kubectl cp behavior. kubectl requires tar and copies directories recursively automatically.",
		promptSnippet: "Copy files or directories to/from a named remote endpoint",
		parameters: Type.Object({
			src: Type.String({ description: "Absolute local path or name:///absolute/remote/path" }),
			dst: Type.String({ description: "Destination, using the same path syntax" }),
			is_recursive: Type.Optional(Type.Boolean({ default: false, description: "Copy a directory recursively (scp -r)" })),
		}),
		renderCall(args, theme, context) {
			const direction = /^[A-Za-z][A-Za-z0-9_-]*:\/\//.test(args.src ?? "") ? "download" : "upload";
			// The builtin result renderer shares this clock with the call renderer.
			if (context.executionStarted && context.state.startedAt === undefined) {
				context.state.startedAt = Date.now();
				context.state.endedAt = undefined;
			}
			const text = new Text(
				theme.fg("toolTitle", theme.bold("remote-shell-copy")) +
				" " + theme.fg("accent", `[${direction}]`) +
				"\n" + theme.fg("muted", "src: ") + (args.src ?? "...") +
				"\n" + theme.fg("muted", "dst: ") + (args.dst ?? "..."),
				0, 0,
			);
			return remoteBox(text, theme);
		},
		renderResult(result: AgentToolResult<BashToolDetails | undefined>, options, theme, context) {
			const output = bash.renderResult!(result, options, theme, { ...context, args: { command: "" }, lastComponent: undefined });
			return remoteBox(output, theme, context.isError);
		},
		async execute(id, params, signal, onUpdate, ctx) {
			const src = parseLocation(params.src, endpoints);
			const dst = parseLocation(params.dst, endpoints);
			if (Boolean(src.endpoint) === Boolean(dst.endpoint)) throw new Error("Exactly one of src and dst must be remote.");
			const endpoint = (src.endpoint ?? dst.endpoint)!;
			const recursive = params.is_recursive ?? false;
			const transfer = copyArgv(endpoint, src, dst, recursive);
			const operations: BashOperations = {
				exec: (_command, cwd, options) => localOps.exec(argvCommand(transfer), cwd, options),
			};
			return createBashToolDefinition(ctx.cwd, { operations, exposeSessionEnvironment: false })
				.execute(id, { command: `copy ${params.src} ${params.dst}` }, signal, onUpdate, ctx);
		},
	});

	pi.registerTool(execTool);
	pi.registerTool(copyTool);

	function ensureToolsAvailable() {
		// Tree navigation restores the transcript's active loadout independently
		// of registration. Re-registering an unchanged direct tool does not
		// reactivate it, so explicitly restore any missing names.
		const active = pi.getActiveTools();
		const missing = [execTool.name, copyTool.name].filter((name) => !active.includes(name));
		if (missing.length > 0) pi.setActiveTools([...active, ...missing]);
	}
}

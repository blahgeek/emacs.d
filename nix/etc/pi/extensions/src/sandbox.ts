/**
 * Sandbox Extension
 *
 * Per-session filesystem allowlist enforced via:
 * 1. A `tool_call` hook that blocks read/edit/write/grep/find/ls when the
 *    target path is not covered by the allowlist.
 * 2. An overridden `bash` tool that runs every command over SSH against a
 *    persistent dropbear server inside a single bwrap sandbox, bind-mounting
 *    only allowlisted paths writable (`--bind-try`) or read-only
 *    (`--ro-bind-try`) on top of BWRAP_BASE_ARGS. Because all commands share
 *    one sandbox, state such as mounts and background processes persists
 *    between commands.
 *
 * The SSH server is started on session start and restarted whenever the
 * allowlist changes or the session tree is navigated (both reset all sandbox
 * state; a notice is shown when this happens). It is stopped on session
 * shutdown, and `--die-with-parent` covers pi exiting unexpectedly.
 *
 * Commands:
 * - `/sandbox`            - show server state and current allowlist
 * - `/sandbox rw <path>`  - allow read-write access to a file or directory
 * - `/sandbox ro <path>`  - allow read-only access to a file or directory
 * - `/sandbox clear`      - clear the allowlist
 * - `/sandbox restart`    - restart the sandbox SSH server
 *
 * CLI flags (repeatable, process-scoped, not persisted in the session):
 * - `--sandbox-rw <path>` / `--sandbox-rw=<path>`
 * - `--sandbox-ro <path>` / `--sandbox-ro=<path>`
 *
 * pi's own arg parser is last-wins for repeated flags, so these are parsed
 * from process.argv directly (scanning stops at a standalone `--`).
 *
 * The allowlist is stored in the session as a custom entry
 * (`sandbox-allowlist`), so it is restored when the session is
 * loaded/resumed. Custom entries do NOT participate in LLM context; they
 * are only rendered in the TUI (see registerEntryRenderer below).
 *
 * DEFAULT_ALLOW_ENTRIES (below) are always in effect on top of the session
 * allowlist and are not persisted.
 *
 * Other extensions can run commands inside the same sandbox via the
 * exported buildSandboxedCommand():
 *   import { buildSandboxedCommand } from "./sandbox.ts";
 *
 * Note: pi loads every extension with a FRESH jiti instance
 * (moduleCache: false in the extension loader), so each extension importing
 * this file gets its own copy of all module-level state. The only things
 * shared between those copies are process-wide: globalThis and process.env.
 * The SSH server is a process-wide resource and therefore lives on
 * globalThis (keyed by Symbol.for, see SandboxServerManager below); the allowlist
 * is session-scoped and lives inside the factory closure.
 */

import { type ChildProcess, execFile, spawn } from "node:child_process";
import { once } from "node:events";
import { type Dirent, readlinkSync } from "node:fs";
import { chmod, copyFile, mkdtemp, readdir, realpath, rm, stat } from "node:fs/promises";
import { type AddressInfo, createConnection, createServer } from "node:net";
import { homedir, tmpdir } from "node:os";
import { basename, dirname, resolve, sep, join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";
import { promisify } from "node:util";
import {
	type BashOperations,
	type ExtensionAPI,
	type ExtensionContext,
	createBashToolDefinition,
	createLocalBashOperations,
} from "@earendil-works/pi-coding-agent";
import { Box, Text } from "@earendil-works/pi-tui";

// ---------------------------------------------------------------------------
// Configuration (stateless, shared across instances)
// ---------------------------------------------------------------------------

/**
 * Base bwrap arguments applied to the sandbox SSH server. Allowlist binds are
 * appended after these. Modify as needed.
 */
const BWRAP_BASE_ARGS: string[] = [
	"--die-with-parent",
	"--unshare-user", "--uid", "0", "--gid", "0", "--cap-add", "ALL",
	"--unshare-pid",

	"--dev", "/dev",
	"--dev-bind", "/dev/fuse", "/dev/fuse",
	"--dev-bind", "/dev/net/tun", "/dev/net/tun",
	"--proc", "/proc",
	"--die-with-parent",

	"--tmpfs", "/home",
	"--tmpfs", "/root",
];

type AllowMode = "rw" | "ro";

interface AllowEntry {
	path: string; // absolute, normalized
	mode: AllowMode;
}

// /etc/resolv.conf is usually a symlink (e.g. to /etc/systemd or a nix store
// path); bind the real target so DNS works inside the sandbox. The link target
// may be relative (resolved against /etc); fall back to the plain path where
// it is not a symlink.
const RESOLVCONF_REALPATH = (() => {
	try {
		return resolve("/etc", readlinkSync("/etc/resolv.conf"));
	} catch {
		return "/etc/resolv.conf";
	}
})();

/**
 * Default allow entries, always in effect. These are NOT persisted in the
 * session; session entries with the same path override them.
 */
const DEFAULT_ALLOW_ENTRIES: AllowEntry[] = [
	{ path: "/usr", mode: "ro" },
	{ path: "/etc", mode: "ro" },
	{ path: RESOLVCONF_REALPATH, mode: "ro" },
	{ path: "/bin", mode: "ro" },
	{ path: "/sbin", mode: "ro" },
	{ path: "/lib", mode: "ro" },
	{ path: "/lib64", mode: "ro" },
	{ path: "/nix", mode: "ro" },
	{ path: "/var/nix", mode: "ro" },
	{ path: "/pi", mode: "rw" },

	{ path: "/tmp", mode: "rw" },
	{ path: "/var/run/docker.sock", mode: "rw" },

	{ path: `${homedir()}/Code`, mode: "ro" },
	{ path: `${homedir()}/work`, mode: "ro" },
	{ path: `${homedir()}/tmp`, mode: "rw" },

	{ path: `${homedir()}/.cache`, mode: "rw" },
	{ path: `${homedir()}/.local/share/uv`, mode: "rw" },
	{ path: `${homedir()}/.local/share/agents`, mode: "rw" },
	{ path: `${homedir()}/.npm`, mode: "rw" },
	{ path: `${homedir()}/.npm-packages`, mode: "rw" },
	{ path: `${homedir()}/.cargo`, mode: "rw" },
	{ path: `${homedir()}/.rustup`, mode: "rw" },
	{ path: `${homedir()}/.gyro`, mode: "rw" },
	{ path: `${homedir()}/.docker`, mode: "rw" },
	{ path: `${homedir()}/.lark-cli`, mode: "rw" },
	{ path: `${homedir()}/.local/share/lark-cli`, mode: "rw" },

	{ path: `${homedir()}/.local/state`, mode: "ro" },
	{ path: `${homedir()}/.local/bin`, mode: "ro" },
	{ path: `${homedir()}/.nix-profile`, mode: "ro" },
	{ path: `${homedir()}/.nix-defexpr`, mode: "ro" },
	{ path: `${homedir()}/.nix-channels`, mode: "ro" },
];

/**
 * Subdirectories that are bind-mounted read-only on top of any rw entry
 * (e.g. to keep VCS metadata out of the agent's reach).
 */
const PROTECTED_SUBDIRS = [".jj"];

const CUSTOM_TYPE = "sandbox-allowlist";

// ---------------------------------------------------------------------------
// Pure helpers (stateless)
// ---------------------------------------------------------------------------

function normalizePath(p: string, cwd: string): string {
	let expanded = p.trim();
	if (expanded === "~" || expanded.startsWith(`~${sep}`) || expanded.startsWith("~/")) {
		expanded = homedir() + expanded.slice(1);
	}
	return resolve(cwd, expanded);
}

function shellQuote(s: string): string {
	return "'" + s.replace(/'/g, "'\\''") + "'";
}

/** True if target equals entryPath or lives underneath it. */
function pathCovers(entryPath: string, target: string): boolean {
	return (
		target === entryPath ||
		target.startsWith(entryPath.endsWith(sep) ? entryPath : entryPath + sep)
	);
}

/**
 * Resolve symlinks in p, even when (parts of) p do not exist yet, by
 * resolving the nearest existing ancestor and re-appending the remainder.
 *
 * The file tools run in the unsandboxed pi process, so the tool_call gate
 * must check the resolved path - otherwise a symlink inside an allowed
 * directory could point anywhere on the host filesystem.
 */
async function resolveExistingPath(p: string): Promise<string> {
	try {
		return await realpath(p);
	} catch {
		const parent = dirname(p);
		if (parent === p) return p;
		return join(await resolveExistingPath(parent), basename(p));
	}
}

/**
 * Collect all --sandbox-rw / --sandbox-ro occurrences from argv, in order.
 * Supports both `--flag value` and `--flag=value` forms; stops at `--`.
 * Needed because pi.getFlag() is last-wins for repeated flags (see the
 * flagEntries initialization in the extension factory).
 */
function parseSandboxFlags(argv: string[]): AllowEntry[] {
	const entries: AllowEntry[] = [];
	for (let i = 0; i < argv.length; i++) {
		const arg = argv[i];
		if (arg === "--") break;
		const match = arg.match(/^--sandbox-(rw|ro)(?:=(.*))?$/s);
		if (!match) continue;
		let value = match[2];
		if (value === undefined) {
			const next = argv[i + 1];
			if (next !== undefined && !next.startsWith("-") && !next.startsWith("@")) {
				value = next;
				i++;
			}
		}
		if (value !== undefined && value !== "") {
			entries.push({ path: value, mode: match[1] as AllowMode });
		}
	}
	return entries;
}

const MAX_COMPLETIONS = 100;

/**
 * Complete a typed path prefix against the filesystem.
 *
 * The command argument is raw text (it never goes through a shell), so paths
 * containing spaces or other special characters are inserted verbatim - no
 * quoting or escaping is needed (or wanted). The typed directory prefix is
 * preserved in the results (e.g. "~/Doc" completes to "~/Documents/").
 */
async function completePathArg(pathPrefix: string, cwd: string): Promise<string[]> {
	const slashIdx = pathPrefix.lastIndexOf(sep);
	const typedDir = slashIdx >= 0 ? pathPrefix.slice(0, slashIdx + 1) : "";
	const base = slashIdx >= 0 ? pathPrefix.slice(slashIdx + 1) : pathPrefix;

	const fsDir = typedDir === "" ? cwd : normalizePath(typedDir, cwd);

	let entries: Dirent[];
	try {
		entries = await readdir(fsDir, { withFileTypes: true });
	} catch {
		return [];
	}

	// Like bash: dotfiles are only offered once the prefix starts with a dot.
	const showHidden = base.startsWith(".");
	return entries
		.filter((e) => (showHidden || !e.name.startsWith(".")) && e.name.startsWith(base))
		.sort(
			(a, b) =>
				Number(b.isDirectory()) - Number(a.isDirectory()) || a.name.localeCompare(b.name),
		)
		.slice(0, MAX_COMPLETIONS)
		.map((e) => `${typedDir}${e.name}${e.isDirectory() ? sep : ""}`);
}

// ---------------------------------------------------------------------------
// Sandboxed SSH server (dropbear inside bwrap)
// ---------------------------------------------------------------------------

const execFileAsync = promisify(execFile);

/** Mount point for the runtime dir inside the sandbox. */
const DROPBEAR_DIR = "/.dropbear";
const HOSTKEY_FILENAME = "ed25519.key";
const CLIENT_KEY_FILENAME = "ed25519.openssh.key";
const SERVER_START_TIMEOUT_MS = 5000;

function findAvailablePort(): Promise<number> {
	return new Promise((resolvePort) => {
		const server = createServer();
		server.once("listening", () => {
			const { port } = server.address() as AddressInfo;
			server.close(() => resolvePort(port));
		});
		server.listen(0, "127.0.0.1");
	});
}

/** Probe a TCP port; resolves false instead of rejecting on refusal. */
function probePort(port: number): Promise<boolean> {
	return new Promise((resolveProbe) => {
		const socket = createConnection({ port, host: "127.0.0.1" });
		socket.once("connect", () => {
			socket.destroy();
			resolveProbe(true);
		});
		socket.once("error", () => {
			socket.destroy();
			resolveProbe(false);
		});
	});
}

/**
 * A persistent SSH (dropbear) server running inside one bwrap sandbox.
 * All bash commands are executed over SSH against this server, so sandbox
 * state persists between commands.
 */
class SSHServer {
	private constructor(
		private readonly proc: ChildProcess,
		private readonly runtimeDir: string,
		readonly port: number,
	) {}

	ready(): boolean {
		return this.proc.exitCode === null && this.proc.signalCode === null;
	}

	async stop(): Promise<void> {
		if (this.ready()) {
			this.proc.kill();
			await once(this.proc, "close");
		}
		await rm(this.runtimeDir, { force: true, recursive: true });
	}

	/**
	 * bwrap arguments for the sandbox: BWRAP_BASE_ARGS plus a bind per
	 * allowlist entry, sorted by path so that parent directories are always
	 * bound before nested entries (a bind mounts over whatever was mounted
	 * at that path).
	 */
	private static async buildBwrapArgs(entries: AllowEntry[], cwd: string): Promise<string[]> {
		const args = [...BWRAP_BASE_ARGS];
		const sorted = [...entries].sort((a, b) => (a.path < b.path ? -1 : a.path > b.path ? 1 : 0));
		for (const entry of sorted) {
			if (entry.mode === "rw") {
				args.push("--bind-try", entry.path, entry.path);
				// --ro-bind-try tolerates a missing source (ENOENT), but bwrap
				// aborts entirely when a path component is not a directory
				// (ENOTDIR). So only add the protected-subdir binds when the entry
				// itself is a directory (or missing, which -try skips).
				let isDir = true;
				try {
					isDir = (await stat(entry.path)).isDirectory();
				} catch {}
				if (isDir) {
					for (const sub of PROTECTED_SUBDIRS) {
						args.push("--ro-bind-try", join(entry.path, sub), join(entry.path, sub));
					}
				}
			} else {
				args.push("--ro-bind-try", entry.path, entry.path);
			}
		}
		args.push("--chdir", cwd);
		return args;
	}

	/**
	 * Local command line that connects to the server and runs payload as the
	 * remote command (interpreted by the remote login shell).
	 */
	buildCommand(payload: string): string {
		const args = [
			"ssh",
			"-q",
			"-F", "/dev/null", // ignore user/host ssh config
			"-o", "BatchMode=yes",
			"-o", "StrictHostKeyChecking=no",
			"-o", "UserKnownHostsFile=/dev/null",
			"-o", "IdentitiesOnly=yes",
			"-i", join(this.runtimeDir, CLIENT_KEY_FILENAME),
			"-p", String(this.port),
			"root@127.0.0.1",
		];
		return `exec ${[...args, payload].map(shellQuote).join(" ")}`;
	}

	static async create(entries: AllowEntry[], cwd: string): Promise<SSHServer> {
		const bwrapArgs = await SSHServer.buildBwrapArgs(entries, cwd);
		const runtimeDir = await mkdtemp(join(tmpdir(), "pi-sandbox-dropbear-"));
		try {
			// Throwaway keypair; the private key doubles as the server host key.
			const hostKey = join(runtimeDir, HOSTKEY_FILENAME);
			const clientKey = join(runtimeDir, CLIENT_KEY_FILENAME);
			await execFileAsync("dropbearkey", ["-t", "ed25519", "-f", hostKey]);
			await execFileAsync("dropbearconvert", ["dropbear", "openssh", hostKey, clientKey]);
			await copyFile(`${hostKey}.pub`, join(runtimeDir, "authorized_keys"));
			await chmod(runtimeDir, 0o700);
			await chmod(join(runtimeDir, "authorized_keys"), 0o600);

			const port = await findAvailablePort();
			const proc = spawn(
				"bwrap",
				[
					...bwrapArgs,
					"--bind", runtimeDir, DROPBEAR_DIR,
					"dropbear",
					"-r", `${DROPBEAR_DIR}/${HOSTKEY_FILENAME}`, // host key
					"-F", // foreground
					"-E", // log to stderr
					// NB: no "-e": the remote environment comes exclusively from
					// each command's payload (see buildRemotePayload), not from
					// the server's startup-time env.
					"-s", // pubkey auth only
					"-j", "-k", // disable local/remote port forwarding
					"-p", `127.0.0.1:${port}`, // listen on loopback only
					"-D", DROPBEAR_DIR, // authorized_keys directory
					"-K", "3", // keepalive seconds
				],
				{ stdio: ["ignore", "ignore", "pipe"] },
			);
			// Keep a stderr tail for startup diagnostics.
			let stderrTail = "";
			proc.stderr?.on("data", (chunk: Buffer) => {
				stderrTail = (stderrTail + chunk.toString()).slice(-2048);
			});
			await once(proc, "spawn");

			// Wait until the server accepts connections.
			const deadline = Date.now() + SERVER_START_TIMEOUT_MS;
			while (Date.now() < deadline) {
				if (proc.exitCode !== null) {
					throw new Error(
						`dropbear exited with code ${proc.exitCode}: ${stderrTail.trim()}`,
					);
				}
				if (await probePort(port)) {
					return new SSHServer(proc, runtimeDir, port);
				}
				await sleep(100);
			}
			proc.kill();
			throw new Error(
				`dropbear did not start listening within ${SERVER_START_TIMEOUT_MS}ms: ${stderrTail.trim()}`,
			);
		} catch (err) {
			await rm(runtimeDir, { force: true, recursive: true });
			throw err;
		}
	}
}

// ---------------------------------------------------------------------------
// Sandbox server state (shared via globalThis, see note in the file header)
// ---------------------------------------------------------------------------

const SERVER_STATE_KEY = Symbol.for("pi-sandbox-ssh-server");

/**
 * Owns the sandbox SSH server process and serializes its lifecycle
 * (start/stop/restart) so concurrent triggers cannot race.
 *
 * NOTE: one instance is shared by all jiti module copies via globalThis,
 * so methods must only touch instance fields, parameters and stateless
 * helpers - never one module copy's closure/module-level mutable state.
 */
class SandboxServerManager {
	server: SSHServer | null = null;
	/** Serializes start/stop so concurrent triggers cannot race. */
	private lifecycle: Promise<void> = Promise.resolve();

	/** Stop and forget the current server. Only call via `enqueue`. */
	private async stopLocked(): Promise<void> {
		const old = this.server;
		this.server = null;
		if (old) {
			await old.stop().catch(() => {});
		}
	}

	/** Run an operation after all previously queued lifecycle operations. */
	private enqueue(operation: () => Promise<void>): Promise<void> {
		const task = this.lifecycle.then(operation);
		this.lifecycle = task.catch(() => {});
		return task;
	}

	/** Restart the server with the given allowlist. Notifies the user either way. */
	restart(entries: AllowEntry[], cwd: string, ctx: ExtensionContext, reason: string): Promise<void> {
		return this.enqueue(async () => {
			await this.stopLocked();
			try {
				const server = await SSHServer.create(entries, cwd);
				this.server = server;
				if (ctx.hasUI) {
					ctx.ui.notify(
						`🔒 Sandbox SSH server restarted (${reason}) on 127.0.0.1:${server.port}. Sandbox state (mounts, background processes) was reset.`,
						"info",
					);
				}
			} catch (err) {
				const message = err instanceof Error ? err.message : String(err);
				if (ctx.hasUI) {
					ctx.ui.notify(`Sandbox SSH server failed to start: ${message}`, "error");
				}
			}
		});
	}

	/** Stop the server. */
	stop(): Promise<void> {
		return this.enqueue(() => this.stopLocked());
	}

	/** The current server, once all pending lifecycle operations have settled. */
	async settled(): Promise<SSHServer | null> {
		await this.lifecycle;
		return this.server && this.server.ready() ? this.server : null;
	}
}

// The first module instance to evaluate this creates the manager; all other
// instances (this extension, extensions importing this file) reuse it.
const serverManager: SandboxServerManager = ((
	globalThis as Record<symbol, SandboxServerManager | undefined>
)[SERVER_STATE_KEY] ??= new SandboxServerManager());

/** Variables that must not be forwarded to the remote shell. */
const SKIPPED_ENV_VARS = new Set(["PWD", "OLDPWD", "SHLVL", "_"]);

/**
 * Script interpreted by the remote login shell for each command: set the
 * environment (dropbear is started without -e, so the payload is the
 * authoritative env source), then run the command from the requested cwd.
 */
function buildRemotePayload(command: string, cwd: string, env: NodeJS.ProcessEnv): string {
	const lines: string[] = [];
	for (const [key, value] of Object.entries(env)) {
		if (value === undefined) continue;
		if (SKIPPED_ENV_VARS.has(key) || key.startsWith("BASH_FUNC_")) continue;
		if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(key)) continue;
		lines.push(`export ${key}=${shellQuote(value)}`);
	}
	lines.push(`cd ${shellQuote(cwd)} || exit 1`);
	lines.push(command);
	return lines.join("\n");
}

/**
 * Build a POSIX shell command line that runs `command` (a raw command
 * string, or an argv array which is quoted) inside the sandbox SSH server,
 * in `cwd` and with `env` forwarded. Execute it via a shell (e.g. spawn
 * with shell: true); the remote command's exit code is preserved.
 *
 * Returns null when the sandbox server is not running (sandbox extension
 * inactive or startup failed). Exported for other extensions, which share
 * the server via globalThis (see the file header), e.g.
 * `import { buildSandboxedCommand } from "./sandbox.ts"`.
 */
export async function buildSandboxedCommand(
	command: string | string[],
	cwd: string,
	env: NodeJS.ProcessEnv = process.env,
): Promise<string | null> {
	const server = await serverManager.settled();
	if (!server) return null;
	const commandLine = Array.isArray(command) ? command.map(shellQuote).join(" ") : command;
	return server.buildCommand(buildRemotePayload(commandLine, cwd, env));
}

// ---------------------------------------------------------------------------
// Extension
// ---------------------------------------------------------------------------

export default function (pi: ExtensionAPI) {
	// Skip inside subagent pi processes (see subagent.ts)
	if (process.env.PI_INSIDE_SUBAGENT) return;

	// Session-scoped state. Lives in the factory closure so each extension
	// instance gets its own copy even when pi reuses the cached module.
	let allowlist: AllowEntry[] = [];

	const localCwd = process.cwd();

	// Session working directory, updated on session_start. Always allowed rw.
	let sessionCwd = localCwd;

	// Entries from --sandbox-rw / --sandbox-ro CLI flags: process-scoped, not
	// persisted. Session entries (/sandbox ...) override them per path.
	//
	// Values are parsed from process.argv instead of pi.getFlag() because pi's
	// CLI parser stores extension flags in a Map keyed by flag name (last-wins),
	// so `pi --sandbox-rw /a --sandbox-rw /b` would only expose /b. We need every
	// occurrence, hence the custom scan. The flags are still registered with
	// pi.registerFlag() below so they show up in --help.
	const flagEntries: AllowEntry[] = parseSandboxFlags(process.argv).map((e) => ({
		path: normalizePath(e.path, localCwd),
		mode: e.mode,
	}));

	/**
	 * Effective entries, lowest to highest precedence:
	 * built-in defaults (including the session cwd as rw) < CLI flag entries
	 * < session entries. Entries with the same path override lower layers, so
	 * the cwd default can be downgraded to ro via a flag or /sandbox command.
	 */
	function effectiveAllowlist(): AllowEntry[] {
		const sessionPaths = new Set(allowlist.map((e) => e.path));
		const flagPaths = new Set(flagEntries.map((e) => e.path));
		const defaults: AllowEntry[] = [
			...DEFAULT_ALLOW_ENTRIES,
			{ path: sessionCwd, mode: "rw" },
		];
		return [
			...defaults.filter((e) => !sessionPaths.has(e.path) && !flagPaths.has(e.path)),
			...flagEntries.filter((e) => !sessionPaths.has(e.path)),
			...allowlist,
		];
	}

	/** Longest-prefix match: returns the most specific entry covering target. */
	function findCoveringEntry(target: string): AllowEntry | undefined {
		let best: AllowEntry | undefined;
		for (const entry of effectiveAllowlist()) {
			if (pathCovers(entry.path, target)) {
				if (!best || entry.path.length > best.path.length) {
					best = entry;
				}
			}
		}
		return best;
	}

	function renderAllowlistEntries(list: AllowEntry[]): string {
		if (list.length === 0) {
			return "Sandbox allowlist is empty.";
		}
		const lines = list.map((e) => `  ${e.mode} ${e.path}`);
		return `Sandbox allowlist:\n${lines.join("\n")}`;
	}

	function renderStatus(): string {
		const server = serverManager.server;
		const serverLine =
			server && server.ready() ? `running on 127.0.0.1:${server.port}` : "not running";
		const defaults = [
			...DEFAULT_ALLOW_ENTRIES.map((e) => `  ${e.mode} ${e.path}`),
			`  rw ${sessionCwd} (session cwd)`,
		].join("\n");
		const flags =
			flagEntries.length > 0
				? `\n\nFrom CLI flags (not stored in session):\n${flagEntries.map((e) => `  ${e.mode} ${e.path}`).join("\n")}`
				: "";
		return `SSH server: ${serverLine}\n\nBuilt-in defaults (not stored in session):\n${defaults}${flags}\n\n${renderAllowlistEntries(allowlist)}`;
	}

	// -- Sandboxed bash ---------------------------------------------------------
	// All commands run over SSH against the persistent sandbox server, so
	// sandbox state (mounts, background processes) persists between commands.
	// pi's built-in local shell backend handles spawn / kill-tree / timeout /
	// output streaming for the ssh client process; killing it closes the
	// channel, which makes dropbear kill the remote command.

	const localBashOps = createLocalBashOperations();

	const sandboxedBashOps: BashOperations = {
		exec: async (command, cwd, options) => {
			// The server is (re)started on session events, never here; this
			// only waits for any in-flight lifecycle operation to settle.
			const sandboxed = await buildSandboxedCommand(command, cwd, options.env);
			if (!sandboxed) {
				throw new Error(
					"Sandbox SSH server is not running. Check earlier notifications for startup errors, or run /sandbox restart.",
				);
			}
			return localBashOps.exec(sandboxed, cwd, options);
		},
	};

	const sandboxedBash = createBashToolDefinition(localCwd, { operations: sandboxedBashOps });

	function updateStatus(ctx: ExtensionContext) {
		if (!ctx.hasUI) return;
		const rw = allowlist.filter((e) => e.mode === "rw").length;
		const ro = allowlist.length - rw;
		ctx.ui.setStatus("sandbox", `🔒 sandbox: ${rw} rw, ${ro} ro`);
	}

	/**
	 * Persist the current allowlist into the session as a custom entry.
	 * Custom entries do NOT participate in LLM context (unlike custom
	 * messages created with pi.sendMessage); they are TUI-only.
	 */
	function persistAllowlist() {
		pi.appendEntry(CUSTOM_TYPE, { allowlist: allowlist.map((e) => ({ ...e })) });
	}

	// Render persisted allowlist entries in the TUI chat transcript.
	pi.registerEntryRenderer(CUSTOM_TYPE, (entry, _options, theme) => {
		const list = (entry.data as { allowlist?: AllowEntry[] } | undefined)?.allowlist ?? [];
		const box = new Box(1, 0, (text) => theme.bg("customMessageBg", text));
		box.addChild(new Text(theme.fg("dim", renderAllowlistEntries(list))));
		return box;
	});

	// -- Restore allowlist from session history --------------------------------

	/** Rebuild in-memory state from the current branch's persisted entries. */
	function restoreFromBranch(ctx: ExtensionContext) {
		sessionCwd = ctx.cwd;
		allowlist = [];
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type !== "custom") continue;
			const e = entry as { customType?: string; data?: { allowlist?: AllowEntry[] } };
			if (e.customType !== CUSTOM_TYPE) continue;
			const saved = e.data?.allowlist;
			if (Array.isArray(saved)) {
				allowlist = saved.filter(
					(item) =>
						item &&
						typeof item.path === "string" &&
						(item.mode === "rw" || item.mode === "ro"),
				);
			}
		}
		updateStatus(ctx);
	}

	// The server restarts on every session start / tree navigation, resetting
	// all sandbox state (mounts, background processes, ...).
	pi.on("session_start", async (_event, ctx) => {
		restoreFromBranch(ctx);
		await serverManager.restart(effectiveAllowlist(), sessionCwd, ctx, "session started");
	});
	pi.on("session_tree", async (_event, ctx) => {
		restoreFromBranch(ctx);
		await serverManager.restart(effectiveAllowlist(), sessionCwd, ctx, "session branch changed");
	});
	pi.on("session_shutdown", async () => {
		await serverManager.stop();
	});

	// -- Block file tools outside the allowlist --------------------------------

	const READ_TOOLS = new Set(["read", "grep", "find", "ls"]);
	const WRITE_TOOLS = new Set(["edit", "write"]);

	pi.on("tool_call", async (event, ctx) => {
		const toolName = event.toolName;
		const needsWrite = WRITE_TOOLS.has(toolName);
		if (!needsWrite && !READ_TOOLS.has(toolName)) return undefined;

		const input = event.input as { path?: string };
		// Check the symlink-resolved path: the file tools run unsandboxed, so
		// a symlink inside an allowed directory must not grant access to its
		// target outside the allowlist.
		const target = await resolveExistingPath(normalizePath(input.path ?? ".", ctx.cwd));
		const entry = findCoveringEntry(target);

		if (entry && (!needsWrite || entry.mode === "rw")) {
			return undefined; // allowed
		}

		const reason = entry
			? `Sandbox: "${target}" is read-only for this session.`
			: `Sandbox: "${target}" is not allowed to access for this session.`;
		return { block: true, reason };
	});

	// -- CLI flags (registered for --help; values are read from process.argv) --

	pi.registerFlag("sandbox-rw", {
		description: "Allow read-write access to a path (repeatable)",
		type: "string",
	});
	pi.registerFlag("sandbox-ro", {
		description: "Allow read-only access to a path (repeatable)",
		type: "string",
	});

	// -- Override bash with sandboxed execution --------------------------------

	pi.registerTool({
		...sandboxedBash,
		// label: "bash (sandbox)",
		// promptGuidelines: [
		// 	...(sandboxedBash.promptGuidelines ?? []),
		// 	"Each bash command runs inside a persistent sandbox",
		// ],
	});

	// -- /sandbox command -------------------------------------------------------

	pi.registerCommand("sandbox", {
		description: "Manage sandbox allowlist: /sandbox [rw|ro <path> | clear | restart]",
		getArgumentCompletions: async (prefix) => {
			// Completing the subcommand itself (no whitespace typed yet).
			if (!/\s/.test(prefix)) {
				const subs = ["rw", "ro", "clear", "restart"].filter((s) => s.startsWith(prefix));
				return subs.length > 0 ? subs.map((s) => ({ value: s, label: s })) : null;
			}

			// Completing the path after "rw " / "ro ". Completion values replace
			// the whole argument text, so they must include the subcommand.
			const match = prefix.match(/^(rw|ro)\s+(.*)$/s);
			if (!match) return null;

			const completions = await completePathArg(match[2], localCwd);
			if (completions.length === 0) return null;
			return completions.map((p) => ({ value: `${match[1]} ${p}`, label: p }));
		},
		handler: async (args, ctx) => {
			const trimmed = args.trim();

			if (!trimmed) {
				ctx.ui.notify(renderStatus(), "info");
				return;
			}

			if (trimmed === "clear") {
				allowlist = [];
				persistAllowlist();
				updateStatus(ctx);
				ctx.ui.notify("Sandbox allowlist cleared", "info");
				await serverManager.restart(effectiveAllowlist(), sessionCwd, ctx, "allowlist updated");
				return;
			}

			if (trimmed === "restart") {
				await serverManager.restart(effectiveAllowlist(), sessionCwd, ctx, "manual restart");
				return;
			}

			const match = trimmed.match(/^(rw|ro)\s+(.+)$/s);
			if (!match) {
				ctx.ui.notify("Usage: /sandbox [rw|ro <path> | clear | restart]", "error");
				return;
			}

			const mode = match[1] as AllowMode;
			const target = normalizePath(match[2], ctx.cwd);

			allowlist = [...allowlist.filter((e) => e.path !== target), { path: target, mode }];
			persistAllowlist();
			updateStatus(ctx);
			ctx.ui.notify(`Sandbox: added ${mode} ${target}`, "info");
			await serverManager.restart(effectiveAllowlist(), sessionCwd, ctx, "allowlist updated");
		},
	});
}

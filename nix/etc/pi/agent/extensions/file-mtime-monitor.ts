import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { createHash } from "node:crypto";
import { readFile, stat } from "node:fs/promises";
import { isAbsolute, relative, resolve } from "node:path";

const CUSTOM_TYPE = "file-mtime-monitor-v2";
const REMINDER_MESSAGE_TYPE = `${CUSTOM_TYPE}-reminder`;

// Files larger than this are never tracked (hashing them is too expensive).
const MAX_FILE_SIZE_BYTES = 1024 * 1024;
// Only the most recently read files are tracked.
const MAX_TRACKED_FILES = 32;

type FileSnapshot =
	| {
			exists: true;
			mtimeIso: string;
			size: number;
			sha1: string;
	  }
	| {
			exists: false;
	  };

type State = {
	fileSnapshots: Record<string, FileSnapshot>;
};

type ReminderDetails = {
	displayPaths: string[];
};

function displayPathFor(cwd: string, absolutePath: string): string {
	const rel = relative(cwd, absolutePath);
	if (!rel || rel === "") return ".";
	if (rel === ".." || rel.startsWith("../") || isAbsolute(rel)) return absolutePath;
	return rel;
}

/**
 * Snapshot a file's mtime and content hash.
 * Returns undefined when the file exists but is too large to track.
 */
async function calculateSnapshot(absolutePath: string): Promise<FileSnapshot | undefined> {
	let stats;
	try {
		stats = await stat(absolutePath);
	} catch {
		return { exists: false };
	}

	if (!stats.isFile() || stats.size > MAX_FILE_SIZE_BYTES) {
		return undefined;
	}

	try {
		const content = await readFile(absolutePath);
		return {
			exists: true,
			mtimeIso: stats.mtime.toISOString(),
			size: stats.size,
			sha1: createHash("sha1").update(content).digest("hex"),
		};
	} catch {
		return { exists: false };
	}
}

function describeSnapshot(snapshot: FileSnapshot): string {
	if (!snapshot.exists) return "missing";
	return `mtime ${snapshot.mtimeIso} (${snapshot.size} bytes)`;
}

export default function (pi: ExtensionAPI) {
	let fileSnapshots: Record<string, FileSnapshot> = {};
	const pendingReads = new Set<string>();

	pi.registerMessageRenderer(REMINDER_MESSAGE_TYPE, (message, _options, theme) => {
		const details = message.details as ReminderDetails | undefined;
		const paths = details?.displayPaths ?? [];
		const text = `[file mtime changed reminder] ${paths.join(", ")}`;
		return new Text(text, 0, 0);
	});

	function rebuildFromBranch(ctx: { sessionManager: { getBranch(): any[] } }) {
		fileSnapshots = {};
		pendingReads.clear();

		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === CUSTOM_TYPE) {
				fileSnapshots = (entry.data as State).fileSnapshots ?? {};
			}
		}
	}

	function saveState(newFileSnapshots: Record<string, FileSnapshot>) {
		fileSnapshots = newFileSnapshots;
		pi.appendEntry(CUSTOM_TYPE, { fileSnapshots: newFileSnapshots } satisfies State);
	}

	pi.on("session_start", async (_event, ctx) => {
		rebuildFromBranch(ctx);
	});

	pi.on("session_tree", async (_event, ctx) => {
		rebuildFromBranch(ctx);
	});

	pi.on("tool_result", async (event, ctx) => {
		if (event.toolName === "read" || event.toolName === "write" || event.toolName === "edit") {
			const input = event.input as { path?: unknown } | undefined;
			const inputPath = input && typeof input.path === "string" && input.path.length > 0 ? input.path : undefined;
			if (inputPath) {
				const absPath = resolve(ctx.cwd, inputPath);
				// Refresh recency: re-insert so the newest reads end up last.
				pendingReads.delete(absPath);
				pendingReads.add(absPath);
			}
		}
	});

	pi.on("agent_settled", async () => {
		let filepaths: Array<string> = [];
		for (const path of pendingReads) {
			filepaths.push(path);
		}
		if (filepaths.length > MAX_TRACKED_FILES) {
			filepaths = filepaths.slice(filepaths.length - MAX_TRACKED_FILES);
		}
		pendingReads.clear();

		const newFileSnapshots: Record<string, FileSnapshot> = {};
		for (const path of filepaths) {
			const snapshot = await calculateSnapshot(path);
			if (snapshot !== undefined) {
				newFileSnapshots[path] = snapshot;
			}
		}
		saveState(newFileSnapshots);
	});

	pi.on("before_agent_start", async (_event, ctx) => {
		const changed: Array<{ path: string, previous: FileSnapshot; current: FileSnapshot }> = [];
		for (const [path, previous] of Object.entries(fileSnapshots)) {
			const current = await calculateSnapshot(path);
			if (current === undefined) {
				continue;
			}

			if (previous.exists != current.exists || (previous.exists && current.exists && previous.sha1 != current.sha1)) {
				changed.push({path, previous, current});
			}
		}

		saveState({});

		if (changed.length === 0) {
			return;
		}

		const reminderText = [
			"[file change monitor] FYI: the following file(s) changed on disk since the agent last finished:",
			...changed.map(({ path, previous, current }) => {
				return `- ${path}: was ${describeSnapshot(previous)}, now ${describeSnapshot(current)}`;
			}),
			"Please re-read affected files before relying on previously observed contents.",
		].join("\n");

		return {
			message: {
				customType: REMINDER_MESSAGE_TYPE,
				content: reminderText,
				display: true,
				details: {
					displayPaths: changed.map(({ path }) => displayPathFor(ctx.cwd, path)),
				} satisfies ReminderDetails,
			},
		};
	});
}

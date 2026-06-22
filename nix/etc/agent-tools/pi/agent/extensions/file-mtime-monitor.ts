import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { realpath, stat } from "node:fs/promises";
import { isAbsolute, relative, resolve } from "node:path";

const CUSTOM_TYPE = "file-mtime-monitor";
const REMINDER_MESSAGE_TYPE = "file-mtime-monitor-reminder";

type Snapshot =
	| {
			exists: true;
			mtimeMs: number;
			mtimeIso: string;
			size: number;
	  }
	| {
			exists: false;
	  };

type FileRecord = Snapshot & {
	path: string;
	displayPath: string;
};

type StateEntryData = {
	records: FileRecord[];
};

type ReminderDetails = {
	displayPaths: string[];
};

function stripAtPrefix(path: string): string {
	return path.startsWith("@") ? path.slice(1) : path;
}

function displayPathFor(cwd: string, absolutePath: string): string {
	const rel = relative(cwd, absolutePath);
	if (!rel || rel === "") return ".";
	if (rel === ".." || rel.startsWith("../") || isAbsolute(rel)) return absolutePath;
	return rel;
}

async function canonicalizePath(cwd: string, inputPath: string): Promise<string> {
	const cleaned = stripAtPrefix(inputPath);
	const absolutePath = resolve(cwd, cleaned);

	try {
		return await realpath(absolutePath);
	} catch {
		// For deleted or not-yet-created paths, canonicalize the nearest existing
		// parent best-effort by keeping the resolved absolute target path.
		return absolutePath;
	}
}

async function snapshotPath(absolutePath: string): Promise<Snapshot> {
	try {
		const stats = await stat(absolutePath);
		return {
			exists: true,
			mtimeMs: stats.mtimeMs,
			mtimeIso: stats.mtime.toISOString(),
			size: stats.size,
		};
	} catch {
		return { exists: false };
	}
}

function snapshotChanged(previous: FileRecord, current: Snapshot): boolean {
	if (!previous.exists && !current.exists) return false;
	if (previous.exists !== current.exists) return true;
	if (previous.exists && current.exists) return previous.mtimeMs !== current.mtimeMs;
	return false;
}

function describeSnapshot(snapshot: Snapshot): string {
	if (!snapshot.exists) return "missing";
	return `${snapshot.mtimeIso} (${snapshot.size} bytes)`;
}

function isTrackedTool(toolName: string): toolName is "read" | "write" | "edit" {
	return toolName === "read" || toolName === "write" || toolName === "edit";
}

function getToolPath(input: unknown): string | undefined {
	if (!input || typeof input !== "object") return undefined;
	const path = (input as { path?: unknown }).path;
	return typeof path === "string" && path.length > 0 ? path : undefined;
}

export default function (pi: ExtensionAPI) {
	const records = new Map<string, FileRecord>();

	pi.registerMessageRenderer(REMINDER_MESSAGE_TYPE, (message, _options, theme) => {
		const details = message.details as ReminderDetails | undefined;
		const paths = details?.displayPaths ?? [];
		const text = `[file mtime changed reminder] ${paths.join(", ")}`;
		return new Text(text, 0, 0);
	});

	function rebuildFromBranch(ctx: { sessionManager: { getBranch(): any[] } }) {
		records.clear();

		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type !== "custom" || entry.customType !== CUSTOM_TYPE) continue;

			const data = entry.data as StateEntryData | undefined;
			if (!data) continue;

			for (const record of data.records ?? []) {
				if (record?.path) records.set(record.path, record);
			}
		}
	}

	function saveRecords(updatedRecords: FileRecord[]) {
		for (const record of updatedRecords) records.set(record.path, record);
		pi.appendEntry(CUSTOM_TYPE, { records: updatedRecords } satisfies StateEntryData);
	}

	pi.on("session_start", async (_event, ctx) => {
		rebuildFromBranch(ctx);
	});

	pi.on("session_tree", async (_event, ctx) => {
		rebuildFromBranch(ctx);
	});

	pi.on("tool_result", async (event, ctx) => {
		if (!isTrackedTool(event.toolName)) return;

		const inputPath = getToolPath(event.input);
		if (!inputPath) return;

		const absolutePath = await canonicalizePath(ctx.cwd, inputPath);
		const snapshot = await snapshotPath(absolutePath);

		saveRecords([
			{
				...snapshot,
				path: absolutePath,
				displayPath: displayPathFor(ctx.cwd, absolutePath),
			},
		]);
	});

	pi.on("before_agent_start", async () => {
		if (records.size === 0) return;

		const changed: Array<{ previous: FileRecord; current: FileRecord }> = [];

		for (const previous of records.values()) {
			const currentSnapshot = await snapshotPath(previous.path);
			if (!snapshotChanged(previous, currentSnapshot)) continue;

			changed.push({
				previous,
				current: {
					...currentSnapshot,
					path: previous.path,
					displayPath: previous.displayPath,
				},
			});
		}

		if (changed.length === 0) return;

		const reminderText = [
			"[file change monitor] FYI: the following file(s) changed on disk since this session last read, wrote, or edited them:",
			...changed.map(({ previous, current }) => {
				return `- ${current.path}: was ${describeSnapshot(previous)}, now ${describeSnapshot(current)}`;
			}),
			"Please re-read affected files before relying on previously observed contents.",
		].join("\n");

		// Update after creating the reminder so the same change is not reported again.
		saveRecords(changed.map(({ current }) => current));

		return {
			message: {
				customType: REMINDER_MESSAGE_TYPE,
				content: reminderText,
				display: true,
				details: { displayPaths: changed.map(({ current }) => current.displayPath) } satisfies ReminderDetails,
			},
		};
	});
}

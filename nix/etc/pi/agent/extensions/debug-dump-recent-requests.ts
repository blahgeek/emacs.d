import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { writeFileSync } from "node:fs";
import { randomUUID } from "node:crypto";

const MAX_ENTRIES = 8;

const deepCopy = <T>(value: T): T => {
  try {
    return structuredClone(value);
  } catch {
    // fallback for non-cloneable values
    return JSON.parse(JSON.stringify(value)) as T;
  }
};

interface DumpEntry {
  t: string;
  request?: unknown;
  response?: {
    status?: number;
    headers?: unknown;
  };
}

export default function (pi: ExtensionAPI) {
  // Ring buffer of recent request/response entries (as separate entries)
  const recent: DumpEntry[] = [];

  const push = (entry: DumpEntry) => {
    recent.push(entry);
    // Trim oldest entries, keeping at most MAX_ENTRIES entries
    while (recent.length > MAX_ENTRIES) recent.shift();
  };

  pi.registerCommand("debug-dump-recent-requests", {
    description:
      `Dump the last ${MAX_ENTRIES} provider request/response entries to a unique /tmp/*.jsonl file`,
    handler: async (_args, ctx) => {
      if (recent.length === 0) {
        ctx.ui.notify("No requests recorded yet", "info");
        return;
      }
      const stamp = new Date().toISOString().replace(/[:.]/g, "-");
      const dumpFile = `/tmp/pi-requests-${stamp}-${randomUUID().slice(0, 8)}.jsonl`;
      try {
        writeFileSync(
          dumpFile,
          recent.map((entry) => JSON.stringify(entry)).join("\n") + "\n",
        );
        ctx.ui.notify(
          `Dumped ${recent.length} entr${recent.length === 1 ? "y" : "ies"} → ${dumpFile}`,
          "info",
        );
      } catch (err) {
        ctx.ui.notify(`Dump failed: ${String(err)}`, "error");
      }
    },
  });

  pi.on("before_provider_request", (event) => {
    try {
      push({
        t: new Date().toISOString(),
        request: deepCopy(event.payload),
      });
    } catch {
      // ignore to avoid disrupting the agent loop
    }
  });

  pi.on("after_provider_response", (event) => {
    try {
      push({
        t: new Date().toISOString(),
        response: {
          status: event.status,
          headers: deepCopy(event.headers),
        },
      });
    } catch {
      // ignore
    }
  });
}

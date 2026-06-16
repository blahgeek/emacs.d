import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { appendFileSync } from "node:fs";
import { randomUUID } from "node:crypto";

export default function (pi: ExtensionAPI) {
  let enabled = false;
  let dumpFile = "";

  pi.registerCommand("debug-toggle-request-dump", {
    description:
      "Toggle dumping provider requests/responses to a unique /tmp/*.jsonl file",
    handler: async (_args, ctx) => {
      enabled = !enabled;

      if (enabled) {
        const stamp = new Date().toISOString().replace(/[:.]/g, "-");
        dumpFile = `/tmp/pi-requests-${stamp}-${randomUUID().slice(0, 8)}.jsonl`;
        ctx.ui.notify(`Request dump ON → ${dumpFile}`, "info");
        ctx.ui.setStatus("req-dump", `dumping → ${dumpFile}`);
      } else {
        ctx.ui.notify("Request dump OFF", "info");
        ctx.ui.setStatus("req-dump", "");
      }
    },
  });

  pi.on("before_provider_request", (event) => {
    if (!enabled) return;
    try {
      appendFileSync(
        dumpFile,
        JSON.stringify({
          t: new Date().toISOString(),
          dir: "request",
          payload: event.payload,
        }) + "\n",
      );
    } catch {
      // ignore write errors to avoid disrupting the agent loop
    }
  });

  pi.on("after_provider_response", (event) => {
    if (!enabled) return;
    try {
      appendFileSync(
        dumpFile,
        JSON.stringify({
          t: new Date().toISOString(),
          dir: "response",
          status: event.status,
          headers: event.headers,
        }) + "\n",
      );
    } catch {
      // ignore
    }
  });
}

// Mostly for working around the scrollback missing bug in ghostel

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { TUI } from "@earendil-works/pi-tui";

export default function (pi: ExtensionAPI) {
  let tui: TUI | undefined;

  pi.on("session_start", (_e, ctx) => {
    if (ctx.mode !== "tui") return;
    // Invisible widget just to capture the TUI handle
    ctx.ui.setWidget("redraw-handle", (t) => {
      tui = t;
      return { render: () => [], invalidate() {} };
    });
  });

  const redraw = () => {
    tui?.invalidate();         // clear component caches
    tui?.requestRender(true);  // force full repaint
  };

  pi.registerCommand("redraw", {
    description: "Force full TUI redraw",
    handler: async () => redraw(),
  });

  pi.registerShortcut("ctrl+shift+l", {
    description: "Force full TUI redraw",
    handler: async () => redraw(),
  });
}

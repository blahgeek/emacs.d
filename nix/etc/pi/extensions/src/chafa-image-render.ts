import { spawnSync } from "node:child_process";

import { ImageContent } from "@earendil-works/pi-ai";
import { ToolRenderers, type ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Component, Container, Spacer, truncateToWidth } from "@earendil-works/pi-tui";


function chafaRender(imageData: string, width: number): string[] {
  let args = ["--format", "symbols", `--size=${width}x1000`];
  if (process.env.TERM_CELL_WIDTH && process.env.TERM_CELL_HEIGHT) {
    args.push(`--font-ratio=${process.env.TERM_CELL_WIDTH}/${process.env.TERM_CELL_HEIGHT}`);
  }
  const result = spawnSync("chafa", [...args, "-"], {
    input: Buffer.from(imageData, "base64"),
  });
  if (result.error || result.status !== 0) {
    return [
      truncateToWidth(result.error?.message ?? `chafa exited with code ${result.status}`, width),
    ];
  }
  return result.stdout.toString().split("\n");
}

class ChafaImage implements Component {
  private imageData: string;
  private renderedWidth?: number;
  private renderedLines?: string[];

  constructor(img: ImageContent) {
    this.imageData = img.data;
  }

  render(width: number): string[] {
    if (this.renderedWidth === width && this.renderedLines !== undefined) {
      return this.renderedLines;
    }
    this.renderedLines = chafaRender(this.imageData, width);
    this.renderedWidth = width;
    return this.renderedLines;
  }

  invalidate(): void {
    this.renderedLines = undefined;
    this.renderedWidth = undefined;
  }
}

export default function (pi: ExtensionAPI) {

  // disable default image rendering
  process.env.PI_IMAGE_PROTOCOL = "none";

  pi.registerToolRenderer((toolName: string, next: () => ToolRenderers | undefined) => {
    const base = next();
    const baseRender = base?.renderResult;
    if (base === undefined || baseRender === undefined) {
      return base;
    }
    return {
      ...base,
      renderResult(result, options, theme, ctx): Component {
        const baseRendered = baseRender(result, options, theme, ctx);
        const imageBlocks = result.content.filter(c => c.type === "image");
        if (imageBlocks.length === 0) {
          return baseRendered;
        }

        let container = new Container();
        if (baseRendered instanceof Container) {
          container = baseRendered;
        } else {
          container.addChild(baseRendered);
        }

        for (const img of imageBlocks) {
          if (img.data) {
            container.addChild(new Spacer(1));
            container.addChild(new ChafaImage(img));
          }
        }
        return container;
      },
    };
  });
}


#!/usr/bin/env node

// 从pi默认的model json基础上修改

const nixPath = process.argv[2];
if (!nixPath) {
  console.error("Usage: node dump-models.mjs <nix-package-path>");
  console.error("Example: node dump-models.mjs /nix/store/vhs75wr9gcmfvhz11shgbywk6njv8wjz-pi-coding-agent-0.64.0");
  process.exit(1);
}

const modelsPath = `${nixPath}/lib/node_modules/pi-monorepo/node_modules/@earendil-works/pi-ai/dist/models.js`;
const { getProviders, getModels } = await import(modelsPath);

const getModel = (provider, model) => {
  const models = getModels(provider);
  for (const m of models) {
    if (m.id === model) {
      // m.id = `my-${m.id}`;
      delete m.baseUrl;
      delete m.provider;
      return m;
    }
  }
  return undefined;
};

// ======

// for reference
let _ref = {};
for (const provider of getProviders()) {
  _ref[provider] = getModels(provider);
}


const output = {
  _ref: _ref,
  providers: {
    "q-stealth-anthropic": {
      baseUrl: "https://o.a.stealth.internal/raw/vibe",
      api: "anthropic-messages",
      apiKey: "Q_STEALTH_API_KEY",
      models: [
        getModel("anthropic", "claude-opus-4-6"),
        getModel("anthropic", "claude-opus-4-7"),
        getModel("anthropic", "claude-sonnet-4-6"),
      ],
    },
    "q-stealth-openai": {
      baseUrl: "https://o.a.stealth.internal/raw/vibe/v1",
      api: "openai-responses",
      apiKey: "Q_STEALTH_API_KEY",
      models: [
        getModel("openai", "gpt-5.4"),
        getModel("openai", "gpt-5.5"),
      ],
    },
    "kh-stealth-anthropic": {
      baseUrl: "https://f-t.stealth.internal",
      api: "anthropic-messages",
      apiKey: "KH_STEALTH_API_KEY",
      models: [
        getModel("anthropic", "claude-opus-4-6"),
        getModel("anthropic", "claude-opus-4-7"),
        getModel("anthropic", "claude-sonnet-4-6"),
      ],
    },
    "kh-stealth-openai": {
      baseUrl: "https://f-t.stealth.internal/v1",
      api: "openai-completions",
      apiKey: "KH_STEALTH_API_KEY",
      models: [
        getModel("openai", "gpt-5.5"),  // uses openai-responses
        {
          ...getModel("moonshotai", "kimi-k2.6"),
          id: "kimi-k2.6-highspeed",
        },
      ],
    },
  },
};


console.log(JSON.stringify(output, null, 2));

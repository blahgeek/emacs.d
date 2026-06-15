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
    // {STEALTH_INTERNAL_MODEL_HOST} will be replaced while running pi
    "stealth-anthropic": {
      baseUrl: "https://{STEALTH_INTERNAL_MODEL_HOST}",
      api: "anthropic-messages",
      apiKey: "$STEALTH_INTERNAL_MODEL_APIKEY",
      models: [
        getModel("anthropic", "claude-opus-4-6"),
        getModel("anthropic", "claude-opus-4-7"),
        getModel("anthropic", "claude-opus-4-8"),
        getModel("anthropic", "claude-sonnet-4-6"),
      ],
    },
    "stealth-openai": {
      baseUrl: "https://{STEALTH_INTERNAL_MODEL_HOST}/v1",
      api: "openai-completions",
      apiKey: "$STEALTH_INTERNAL_MODEL_APIKEY",
      models: [
        getModel("openai", "gpt-5.5"),  // uses openai-responses
        {
          id: "coding-model-okapi-0615-vibe",
          name: "Coding Model 0615",
          reasoning: true,
          input: ["text", "image"],
          contextWindow: 1048576,
          maxTokens: 1048576,
          compat: {
            supportsStore: false,
            supportsDeveloperRole: false,
            supportsReasoningEffort: false,
            maxTokensField: "max_tokens",
            supportsStrictMode: false
          },
        },
      ],
    },
  },
};


console.log(JSON.stringify(output, null, 2));

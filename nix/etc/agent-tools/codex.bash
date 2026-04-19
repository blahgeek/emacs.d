mkdir -p ~/.codex

sandbox_extra_args=(
    --yolo
    --config 'model_providers.custom.name="custom"'
    --config 'model_providers.custom.env_key="CODEX_API_KEY"'
    --config 'model_provider="custom"'
)

if [[ -n "$INSIDE_STEALTH_INTERNAL" ]]; then
    echo "Inside stealth.internal, using q-stealth..."
    sandbox_required_apikeys=("CODEX_API_KEY:api.stealth.internal")
    sandbox_extra_args+=(
        --config 'model_providers.custom.base_url="https://o.a.stealth.internal/v1"'
        --config 'model="stupid/gpt-5.3-codex"'
    )
else
    echo "Using OpenRouter with api_key openrouter-codex-cli"
    sandbox_required_apikeys=("CODEX_API_KEY:openrouter-codex-cli")
    sandbox_extra_args+=(
        --config 'model_providers.custom.base_url="https://openrouter.ai/api/v1"'
        --config 'model="openai/gpt-5.3-codex"'
    )
fi

sandbox_rw_files=(
    "$HOME/.codex"
    "$SKILLS_DIR:$HOME/.agents/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.codex/AGENTS.md"
)

mkdir -p ~/.codex

sandbox_extra_args=(
    --yolo
)

if [[ -n "$INSIDE_STEALTH_INTERNAL" ]]; then
    echo "Inside stealth.internal, using kh-stealth..."
    sandbox_required_apikeys=("CODEX_API_KEY:f-t.stealth.internal")
    sandbox_extra_args+=(
        --config 'model_provider="custom"'
        --config 'model_providers.custom.name="custom"'
        --config 'model_providers.custom.env_key="CODEX_API_KEY"'
        --config 'model_providers.custom.base_url="https://f-t.stealth.internal/v1"'
        --config 'model_providers.custom.wire_api="responses"'
        --config 'model="gpt-5.5"'
    )
fi

sandbox_rw_files=(
    "$HOME/.codex"
    "$SKILLS_DIR:$HOME/.agents/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.codex/AGENTS.md"
)

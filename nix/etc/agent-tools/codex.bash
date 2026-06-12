mkdir -p ~/.codex

sandbox_extra_args=(
    --yolo
)

if [[ -n "${STEALTH_INTERNAL_MODEL_HOST}" ]]; then
    echo "Using custom model host ${STEALTH_INTERNAL_MODEL_HOST}..."
    sandbox_required_apikeys=("CODEX_API_KEY:${STEALTH_INTERNAL_MODEL_HOST}")
    sandbox_extra_args+=(
        --config 'model_provider="custom"'
        --config 'model_providers.custom.name="custom"'
        --config 'model_providers.custom.env_key="CODEX_API_KEY"'
        --config 'model_providers.custom.base_url="https://'${STEALTH_INTERNAL_MODEL_HOST}'/v1"'
        --config 'model_providers.custom.wire_api="responses"'
        --config 'model="gpt-5.5"'
    )
fi

sandbox_rw_files=(
    "$HOME/.codex"
    "$SKILLS_DIR:$HOME/.agents/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.codex/AGENTS.md"
)

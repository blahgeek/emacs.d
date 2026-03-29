mkdir -p ~/.codex

extra_args=(
    --config 'model_providers.custom.name="custom"'
    --config 'model_providers.custom.env_key="CODEX_API_KEY"'
    --config 'model_provider="custom"'
)

if [[ -n "$INSIDE_MSH_TEAM" ]]; then
    echo "Inside msh team, using Qianxun..."
    export CODEX_API_KEY="$(emacs-get-gptel-api-key api.msh.team)"
    extra_args+=(
        --config 'model_providers.custom.base_url="https://openai.app.msh.team/v1"'
        # 5.3 is not ready yet??
        --config 'model="stupid/gpt-5.2-codex"'
    )
else
    export CODEX_API_KEY="$(emacs-get-gptel-api-key openrouter-codex-cli)"
    echo "Using OpenRouter with api_key openrouter-codex-cli"
    extra_args+=(
        --config 'model_providers.custom.base_url="https://openrouter.ai/api/v1"'
        --config 'model="openai/gpt-5.3-codex"'
    )
fi

sandbox_rw_files=("$HOME/.codex")
sandbox_extra_args=(--yolo)

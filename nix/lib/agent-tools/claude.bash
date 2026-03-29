if [ ! -f ~/.claude.json ]; then
    echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
fi

if [[ -n "$INSIDE_MSH_TEAM" ]]; then
    echo "Inside msh team, using Qianxun..."
    export ANTHROPIC_BASE_URL=https://openai.app.msh.team/raw/vibe/
    API_KEY="$(emacs-get-gptel-api-key api.msh.team)"
else
    echo "Using OpenRouter with api_key $OPENROUTER_API_KEY"
    export ANTHROPIC_BASE_URL="https://openrouter.ai/api"
    API_KEY="$(emacs-get-gptel-api-key openrouter-claude-code)"
fi

export ANTHROPIC_AUTH_TOKEN="$API_KEY"
export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty

sandbox_rw_files=("$HOME/.claude.json" "$HOME/.claude")
sandbox_extra_args=(--dangerously-skip-permissions)

extra_args=(
    --add-dir ~/.agents
)

if [ ! -f ~/.claude.json ]; then
    echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
fi

if [[ -n "$INSIDE_STEALTH_INTERNAL" ]]; then
    echo "Inside stealth.internal, using q-stealth..."
    export ANTHROPIC_BASE_URL=https://o.a.stealth.internal/raw/vibe/
    API_KEY="$(emacs-get-gptel-api-key api.stealth.internal)"
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

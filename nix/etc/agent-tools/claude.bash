
if [[ -n "$INSIDE_STEALTH_INTERNAL" ]]; then
    if [ ! -f ~/.claude.json ]; then
        echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
    fi
    echo "Inside stealth.internal, using q-stealth..."
    export ANTHROPIC_BASE_URL=https://o.a.stealth.internal/raw/vibe/
    API_KEY="$(emacs-get-gptel-api-key api.stealth.internal)"

    export ANTHROPIC_AUTH_TOKEN="$API_KEY"
    export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty
fi

sandbox_rw_files=("$HOME/.claude.json" "$HOME/.claude")
sandbox_extra_args=(--dangerously-skip-permissions)
sandbox_skills_dir="$HOME/.claude/skills"
sandbox_global_instruction_file="$HOME/.claude/CLAUDE.md"

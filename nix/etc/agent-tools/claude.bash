
if [[ -n "$INSIDE_MSH_TEAM" ]]; then
    if [ ! -f ~/.claude.json ]; then
        echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
    fi
    echo "Inside msh team, using Qianxun..."
    export ANTHROPIC_BASE_URL=https://openai.app.msh.team/raw/vibe/
    API_KEY="$(emacs-get-gptel-api-key api.msh.team)"

    export ANTHROPIC_AUTH_TOKEN="$API_KEY"
    export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty
fi

sandbox_rw_files=("$HOME/.claude.json" "$HOME/.claude")
sandbox_extra_args=(--dangerously-skip-permissions)
sandbox_skills_dir="$HOME/.claude/skills"

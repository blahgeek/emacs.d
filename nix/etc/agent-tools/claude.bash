
if [[ -n "$INSIDE_MSH_TEAM" ]]; then
    if [ ! -f ~/.claude.json ]; then
        echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
    fi
    echo "Inside msh team, using Qianxun..."
    export ANTHROPIC_BASE_URL=https://openai.app.msh.team/raw/vibe/
    export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty
    sandbox_required_apikeys=("ANTHROPIC_AUTH_TOKEN:api.msh.team")
fi

sandbox_rw_files=(
    "$HOME/.claude.json"
    "$HOME/.claude"
    "$SKILLS_DIR:$HOME/.claude/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.claude/CLAUDE.md"
)
sandbox_extra_args=(--dangerously-skip-permissions)

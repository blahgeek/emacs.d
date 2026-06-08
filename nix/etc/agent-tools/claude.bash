
if [[ -n "$INSIDE_STEALTH_INTERNAL" ]]; then
    if [ ! -f ~/.claude.json ]; then
        echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
    fi
    echo "Inside stealth.internal, using kh-stealth..."
    export ANTHROPIC_BASE_URL=https://f-t.stealth.internal
    export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty
    sandbox_required_apikeys=("ANTHROPIC_AUTH_TOKEN:f-t.stealth.internal")
fi

sandbox_rw_files=(
    "$HOME/.claude.json"
    "$HOME/.claude"
    "$SKILLS_DIR:$HOME/.claude/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.claude/CLAUDE.md"
)
sandbox_extra_args=(--dangerously-skip-permissions)

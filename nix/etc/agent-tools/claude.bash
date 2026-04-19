
if [[ -n "$INSIDE_STEALTH_INTERNAL" ]]; then
    if [ ! -f ~/.claude.json ]; then
        echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
    fi
    echo "Inside stealth.internal, using q-stealth..."
    export ANTHROPIC_BASE_URL=https://o.a.stealth.internal/raw/vibe/
    export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty
    sandbox_required_apikeys=("ANTHROPIC_AUTH_TOKEN:api.stealth.internal")
fi

sandbox_rw_files=(
    "$HOME/.claude.json"
    "$HOME/.claude"
    "$SKILLS_DIR:$HOME/.claude/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.claude/CLAUDE.md"
)
sandbox_extra_args=(--dangerously-skip-permissions)

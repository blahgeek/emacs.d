
if [[ -n "${STEALTH_INTERNAL_MODEL_HOST}" ]]; then
    if [ ! -f ~/.claude.json ]; then
        echo '{ "hasCompletedOnboarding": true }' > ~/.claude.json
    fi
    echo "Using custom model host ${STEALTH_INTERNAL_MODEL_HOST}..."
    export ANTHROPIC_BASE_URL=https://${STEALTH_INTERNAL_MODEL_HOST}
    export ANTHROPIC_API_KEY="" # Important: Must be explicitly empty
    sandbox_required_apikeys=("ANTHROPIC_AUTH_TOKEN:${STEALTH_INTERNAL_MODEL_HOST}")
fi

sandbox_rw_files=(
    "$HOME/.claude.json"
    "$HOME/.claude"
    "$SKILLS_DIR:$HOME/.claude/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.claude/CLAUDE.md"
)
sandbox_extra_args=(--dangerously-skip-permissions)

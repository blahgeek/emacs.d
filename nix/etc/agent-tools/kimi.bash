mkdir -p ~/.kimi-code

# in terminal emacs, it would open w3m by default
# also we do not want to expose /opt/orbstack-guest/bin/open in sandbox
# let's disable browser feature
export BROWSER=true

sandbox_rw_files=(
    "$HOME/.kimi-code"
    "$SKILLS_DIR:$HOME/.agents/skills"
    "$SCRIPT_DIR/agents.md:$HOME/.kimi-code/AGENTS.md"
)

sandbox_extra_args+=(
    --yolo
)

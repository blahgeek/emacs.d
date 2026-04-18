mkdir -p ~/.kimi

# in terminal emacs, it would open w3m by default
# also we do not want to expose /opt/orbstack-guest/bin/open in sandbox
# let's disable browser feature
export BROWSER=true

sandbox_rw_files=(
    "$HOME/.kimi"
    "$SKILLS_DIR:$HOME/.agents/skills"
)

# agents.md is defined in agent.yaml using relative path
sandbox_extra_args+=(
    --yolo
    --agent-file "$SCRIPT_DIR/kimi/agent.yaml"
)

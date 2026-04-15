mkdir -p ~/.kimi

# in terminal emacs, it would open w3m by default
# also we do not want to expose /opt/orbstack-guest/bin/open in sandbox
# let's disable browser feature
export BROWSER=true

sandbox_rw_files=("$HOME/.kimi")
sandbox_skills_dir="$HOME/.agents/skills"
# not required. defined in agent.yaml using relative path
sandbox_global_instruction_file="/tmp/_notused_.md"

sandbox_extra_args+=(
    --yolo
    --agent-file "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/kimi/agent.yaml"
)

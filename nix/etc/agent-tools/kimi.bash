mkdir -p ~/.kimi

# in terminal emacs, it would open w3m by default
# also we do not want to expose /opt/orbstack-guest/bin/open in sandbox
# let's disable browser feature
export BROWSER=true

sandbox_rw_files=("$HOME/.kimi")
sandbox_extra_args=(--yolo)
sandbox_skills_dir="$HOME/.agents/skills"
sandbox_global_instruction_file="$HOME/.agents/AGENTS.md"

_tmp_agent_yaml_file=$(mktemp /tmp/kimi.custom-agent.XXXXXX.yaml)
cat <<EOF > $_tmp_agent_yaml_file
version: 1
agent:
  extend: default  # 继承默认 Agent
  system_prompt_path: $sandbox_global_instruction_file
EOF
sandbox_extra_args+=(--agent-file $_tmp_agent_yaml_file)

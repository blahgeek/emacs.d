_script_dir="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"

mkdir -p ~/.pi/sessions

# put settings in /pi/agent instead of ~/.pi/agent, to prevent home path in system prompt
export PI_CODING_AGENT_DIR=/pi/agent

sandbox_rw_files=(
    "$HOME/.pi"
    "$_script_dir/pi/agent/settings.json:/pi/agent/settings.json"
    "$_MODELS_JSON:/pi/agent/models.json"
)
unset MODELS_JSON

sandbox_skills_dir="/pi/agent/skills"
sandbox_global_instruction_file="/pi/agent/AGENTS.md"

# session dir should still be in ~/.pi
sandbox_extra_args=(
    --session-dir ~/.pi/sessions
)

{
    read -r KIMI_API_KEY
    read -r QIANXUN_API_KEY
} < <(emacs-get-gptel-api-key code.kimi.com api.msh.team)
[[ $KIMI_API_KEY != null ]] && export KIMI_API_KEY
[[ $QIANXUN_API_KEY != null ]] && export QIANXUN_API_KEY

if [[ -n "$INSIDE_MSH_TEAM" ]]; then
    sandbox_extra_args+=(
        --provider qianxun-anthropic
        --model claude-opus-4-6
    )
else
    sandbox_extra_args+=(
        --provider kimi-coding
        --model k2p5
    )
fi


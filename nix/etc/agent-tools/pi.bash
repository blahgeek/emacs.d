# ~/.pi 不会被真正的用到，因为PI_CODING_AGENT_DIR整个指定了
# 但是有几个目录和文件会特别的指过去或者bind过去
# 为了避免歧义，用~/.pi_sandbox目录

mkdir -p ~/.pi_sandbox/sessions

if [[ ! -f ~/.pi_sandbox/auth.json ]]; then
    echo '{}' > ~/.pi_sandbox/auth.json
fi

# put settings in /pi/agent instead of ~/.pi/agent, to prevent home path in system prompt
export PI_CODING_AGENT_DIR=/pi/agent

sandbox_rw_files=(
    "$HOME/.pi/sessions"
    "$_MODELS_JSON:/pi/agent/models.json"
    "$SCRIPT_DIR/pi/agent/settings.json:/pi/agent/settings.json"
    "$SKILLS_DIR:/pi/agent/skills"
    "$SCRIPT_DIR/agents.md:/pi/agent/AGENTS.md"
    "$HOME/.pi_sandbox/auth.json:/pi/agent/auth.json"
)
unset MODELS_JSON

# session dir should still be in ~/.pi
sandbox_extra_args=(
    # disable update check
    --offline
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


# ~/.pi 不会被真正的用到，因为PI_CODING_AGENT_DIR整个指定了
# 但是有几个目录和文件会特别的指过去或者bind过去
# 为了避免歧义，用~/.pi_sandbox目录

mkdir -p ~/.pi_sandbox/sessions

if [[ ! -f ~/.pi_sandbox/auth.json ]]; then
    echo '{}' > ~/.pi_sandbox/auth.json
fi

# put settings in /pi/agent instead of ~/.pi/agent, to prevent home path in system prompt
export PI_CODING_AGENT_DIR=/pi/agent
sandbox_extra_args=()

TRANSLATED_MODELS_JSON="/tmp/pi-models-$(sha1sum $_MODELS_JSON | cut -d' ' -f1)-translated.json"

if [[ -n "$STEALTH_INTERNAL_MODEL_HOST" ]]; then
    sandbox_required_apikeys=(
        "STEALTH_INTERNAL_MODEL_APIKEY:${STEALTH_INTERNAL_MODEL_HOST}"
    )
    sandbox_extra_args+=(
        --provider stealth-openai
        --model gpt-5.5
    )
    cat "$_MODELS_JSON" | sed "s/{STEALTH_INTERNAL_MODEL_HOST}/${STEALTH_INTERNAL_MODEL_HOST}/g" \
                              > "$TRANSLATED_MODELS_JSON"
else
    sandbox_required_apikeys=(
        "KIMI_API_KEY:code.kimi.com"
    )
    sandbox_extra_args+=(
        --provider openai-codex
        --model gpt-5.5
    )
    cat "$_MODELS_JSON" | sed "s/{STEALTH_INTERNAL_MODEL_HOST}/not-set.stealth.internal/g" \
                              > "$TRANSLATED_MODELS_JSON"
fi
unset _MODELS_JSON

sandbox_rw_files=(
    "$TRANSLATED_MODELS_JSON:/pi/agent/models.json"
    "$SCRIPT_DIR/pi/agent/settings.json:/pi/agent/settings.json"
    "$SCRIPT_DIR/pi/agent/keybindings.json:/pi/agent/keybindings.json"
    "$SCRIPT_DIR/pi/agent/themes:/pi/agent/themes"
    "$SCRIPT_DIR/pi/agent/extensions:/pi/agent/extensions"
    "$SKILLS_DIR:/pi/agent/skills"
    "$SCRIPT_DIR/agents.md:/pi/agent/AGENTS.md"
    "$HOME/.pi_sandbox/sessions:/pi/agent/sessions"
    "$HOME/.pi_sandbox/auth.json:/pi/agent/auth.json"
)

sandbox_extra_args+=(
    # disable update check
    --offline
)

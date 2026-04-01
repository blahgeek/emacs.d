#!/bin/bash

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

# we want "/run git diff" without pager
export GIT_PAGER=cat
# export GIT_CONFIG_GLOBAL=${git.gitconfig}

MOONSHOT_API_BASE="https://api.moonshot.cn/v1/"
MOONSHOT_API_KEY="$(emacs-get-gptel-api-key api.moonshot.cn)"
if [ -n "$INSIDE_MSH_TEAM" ]; then
    MOONSHOT_API_BASE="https://api.msh.team/v1/"
    MOONSHOT_API_KEY="$(emacs-get-gptel-api-key api.msh.team)"
fi
OPENROUTER_API_KEY="$(emacs-get-gptel-api-key openrouter.ai)"

export MOONSHOT_API_BASE MOONSHOT_API_KEY OPENROUTER_API_KEY
exec aider \
     --config=$SCRIPT_DIR/aider.conf.yml \
     --model-metadata-file $SCRIPT_DIR/model.metadata.json \
     --model-settings-file $SCRIPT_DIR/model.settings.yml \
     "$@"

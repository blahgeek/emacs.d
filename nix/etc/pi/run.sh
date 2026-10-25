#!/bin/bash

set +o nounset

# assert PI_PACKAGE_DIR & PI_CODING_AGENT_DIR exists as directories
[ ! -d "$PI_PACKAGE_DIR" ] && { echo "PI_PACKAGE_DIR is not a directory: $PI_PACKAGE_DIR"; exit 1; }
[ ! -d "$PI_CODING_AGENT_DIR" ] && { echo "PI_CODING_AGENT_DIR is not a directory: $PI_CODING_AGENT_DIR"; exit 1; }

function export_apikeys() {
    _apikey_vars=()
    _apikey_domains=()
    for _spec in "$@"; do
        _apikey_vars+=("${_spec%%:*}")
        _apikey_domains+=("${_spec#*:}")
    done
    mapfile -t _apikey_values < <(emacs-auth-source-get.py "${_apikey_domains[@]}")
    for _i in "${!_apikey_vars[@]}"; do
        _val="${_apikey_values[$_i]:-}"
        if [[ -n "$_val" && "$_val" != "null" ]]; then
            export "${_apikey_vars[$_i]}=$_val"
        fi
    done
    unset _apikey_vars _apikey_domains _apikey_values _spec _i _val
}

if [[ -v PI_REQUIRED_APIKEYS ]]; then
    # shellcheck disable=SC2086
    export_apikeys $PI_REQUIRED_APIKEYS
    unset PI_REQUIRED_APIKEYS
fi

# shellcheck disable=SC1090
[ -f ~/.profile.agents ] && source ~/.profile.agents

_local_config_dirs=(sessions)
_local_config_jsons=(auth.json settings.json trust.json mcp.json mcp-auth.json)
for x in "${_local_config_dirs[@]}"; do
    [ ! -d ~/.pi_sandbox/"$x" ] && mkdir -p ~/.pi_sandbox/"$x"
done
for x in "${_local_config_jsons[@]}"; do
    [ ! -f ~/.pi_sandbox/"$x" ] && echo '{}' > ~/.pi_sandbox/"$x"
done

_args=()
case "$1" in
    # do not add extra args for subcommands
    install|remove|uninstall|update|list|config|auth|mcp)
        _args=("$@")
        ;;
    *)
        if [[ -v RUN_PI_EXTRA_ARGS ]]; then
            # shellcheck disable=SC2206
            _args=($RUN_PI_EXTRA_ARGS)
            unset RUN_PI_EXTRA_ARGS
        fi
        _args+=(--tui-mode regular "$@")
        ;;
esac

# /etc/ssh/ssh_config.d : fix `Bad owner or permissions on /etc/ssh/ssh_config.d/10-orbstack.conf`

# shellcheck disable=SC2046
exec bwrap \
     $(for x in /*; do printf -- '--dev-bind %s %s ' "$x" "$x"; done) \
     --overlay-src "$PI_CODING_AGENT_DIR" \
     --tmp-overlay /pi/agent \
     $(for x in "${_local_config_dirs[@]}" "${_local_config_jsons[@]}"; do
         printf -- '--bind %s %s ' ~/.pi_sandbox/"$x" /pi/agent/"$x"
     done) \
     --ro-bind "$PI_PACKAGE_DIR" /pi/src \
     --tmpfs /pi-private \
     --tmpfs /etc/ssh/ssh_config.d \
     --setenv PI_CODING_AGENT_DIR /pi/agent \
     --setenv PI_PACKAGE_DIR /pi/src \
     --setenv PI_OFFLINE true \
     pi "${_args[@]}"

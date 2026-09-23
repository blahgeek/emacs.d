#!/usr/bin/env bash

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script should be sourced, not executed."
    exit 1
fi

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

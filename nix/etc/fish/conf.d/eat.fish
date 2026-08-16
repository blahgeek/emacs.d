# EAT integration: keep the host Emacs' default-directory in sync with the shell

string match -qr '(^|,)eat$' -- "$INSIDE_EMACS"; or return 0

function __eat_sync_cwd --on-variable PWD --on-event fish_prompt
    _emacs_term_cmd set-cwd $PWD
end

# EAT integration: keep the host Emacs' default-directory in sync with the
# shell (ported from xonshconf/emacs.py set_pwd + pre_prompt).
# This file is in conf.d/ because event handlers in functions/ are not
# autoloaded; conf.d snippets are sourced at startup.
function __eat_sync_cwd --on-variable PWD --on-event fish_prompt
    _eat_term_cmd set-cwd $PWD
end

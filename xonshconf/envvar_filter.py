import os

from xonsh.built_ins import XSH

# Do not include those starting with XONSH_ or XONTRIBS_
_ENVVARS_BLACKLIST = {
    # my own
    "INSIDE_EMACS",
    "EMACS_DISPLAY_GRAPHIC_P",

    "XONSHRC",
    "XONSHRC_DIR",
    "RAISE_SUBPROC_ERROR",
    "THREAD_SUBPROCS",
    "AUTO_CONTINUE",
    "COMMANDS_CACHE_SAVE_INTERMEDIATE",
    "ENABLE_COMMANDS_CACHE",
    "AUTO_CD",
    "AUTO_PUSHD",
    "COMPLETE_DOTS",
    "DIRSTACK_SIZE",
    "PUSHD_MINUS",
    "PUSHD_SILENT",
    "DOTGLOB",
    "EXPAND_ENV_VARS",
    "FOREIGN_ALIASES_OVERRIDE",
    "FOREIGN_ALIASES_SUPPRESS_SKIP_MESSAGE",
    "GLOB_SORTED",
    "BOTTOM_TOOLBAR",
    "COLOR_INPUT",
    "COLOR_RESULTS",
    "DYNAMIC_CWD_ELISION_CHAR",
    "DYNAMIC_CWD_WIDTH",
    "IGNOREEOF",
    "INDENT",
    "MULTILINE_PROMPT",
    "MULTILINE_PROMPT_POS",
    "MULTILINE_PROMPT_PRE",
    "PRETTY_PRINT_RESULTS",
    "PROMPT",
    "PROMPT_FIELDS",
    "PROMPT_REFRESH_INTERVAL",
    "PROMPT_TOKENS_FORMATTER",
    "RIGHT_PROMPT",
    "SHELL_TYPE",
    "SUGGEST_COMMANDS",
    "SUGGEST_MAX_NUM",
    "SUGGEST_THRESHOLD",
    "SUPPRESS_BRANCH_TIMEOUT_MESSAGE",
    "TITLE",
    "UPDATE_PROMPT_ON_KEYPRESS",
    "VC_BRANCH_TIMEOUT",
    "VC_GIT_INCLUDE_UNTRACKED",
    "VC_HG_SHOW_BRANCH",
    "AUTO_SUGGEST",
    "AUTO_SUGGEST_IN_COMPLETIONS",
    "MOUSE_SUPPORT",
    "PROMPT_TOOLKIT_COLOR_DEPTH",
    "PTK_STYLE_OVERRIDES",
    "ASYNC_INVALIDATE_INTERVAL",
    "ASYNC_PROMPT_THREAD_WORKERS",
    "ENABLE_ASYNC_PROMPT",
    "HISTCONTROL",
    "ALIAS_COMPLETIONS_OPTIONS_BY_DEFAULT",
    "ALIAS_COMPLETIONS_OPTIONS_LONGEST",
    "BASH_COMPLETIONS",
    "CASE_SENSITIVE_COMPLETIONS",
    "CMD_COMPLETIONS_SHOW_DESC",
    "COMPLETIONS_BRACKETS",
    "COMPLETION_QUERY_LIMIT",
    "FUZZY_PATH_COMPLETION",
    "SUBSEQUENCE_PATH_COMPLETION",
    "COMPLETIONS_CONFIRM",
    "COMPLETIONS_DISPLAY",
    "COMPLETIONS_MENU_ROWS",
    "COMPLETION_IN_THREAD",
    "COMPLETION_MODE",
    "UPDATE_COMPLETIONS_ON_KEYPRESS",
    "LAST_RETURN_ERROR",
    "OLDPWD",
}

@events.on_pre_spec_run
def _on_run(spec, **kwargs):
    if spec.cmd and spec.cmd[0] == 'xonsh':
        return

    # see xonsh/procs/specs.py
    # overrides prep_env_subproc
    def _prepare_env(kwargs):
        with XSH.env.swap(spec.env) as env:
            denv = env.detype()
        denv = {
            k: v for k, v in denv.items()
            if not (k.startswith('XONSH_') or k.startswith('XONTRIBS_') or k in _ENVVARS_BLACKLIST)
        }
        kwargs["env"] = denv

    spec.prep_env_subproc = _prepare_env

# Ported from xonshconf/env.xsh $PROMPT:
#   {env_name}{CYAN}{hostname} {YELLOW}{cwd}{git_prompt: ({})}{RED}[code]{RESET}>
function fish_prompt
    set -l last_status $status

    # {env_name}: only python venv; conda was never handled here either
    if set -q VIRTUAL_ENV
        echo -n '('(path basename -- $VIRTUAL_ENV)') '
    end

    echo -n (set_color cyan)(prompt_hostname)' '
    echo -n (set_color yellow)(prompt_pwd)(set_color normal)

    set -l gp (__git_prompt)
    if test -n "$gp"
        echo -n ' ('$gp')'(set_color normal)
    end

    if test $last_status -ne 0
        echo -n (set_color red)'['$last_status']'(set_color normal)
    end

    echo -n '> '
end

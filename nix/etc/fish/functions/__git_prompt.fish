# Ported from xonshconf/git_prompt.py: git/jj status for the prompt.
# Uses gitstatus.sh (fast bash-git-prompt, copied from xonshconf/), with jj-specific display.
# NOTE: the xonsh version had a 10s timeout around gitstatus.sh; fish has no
# builtin timeout for command substitutions, so that is dropped.
function __git_prompt
    # locate gitstatus.sh relative to this file (functions/ -> config dir),
    # so the whole config directory can live anywhere
    set -l script (path dirname (path dirname (status filename)))/gitstatus.sh
    test -x $script; or return 1

    set -l extra_args '--no-optional-locks -c gc.auto=0 -c maintenance.auto=false'
    if test -f ~/.gitconfig_fsmonitor
        set extra_args "$extra_args -c include.path=$HOME/.gitconfig_fsmonitor"
    end

    set -l out (
        env __GIT_PROMPT_SHOW_UNTRACKED_FILES=normal __GIT_EXTRA_ARGS="$extra_args" \
            $script 2>/dev/null
    )
    or return 1 # not a git repo

    if test (count $out) -ne 10
        echo -n GIT_ERROR
        return
    end

    set -l branch $out[1]
    set -l remote $out[2]
    # $out[3] (upstream) is unused, same as the xonsh version
    set -l staged $out[4]
    set -l conflicts $out[5]
    set -l changed $out[6]
    set -l untracked $out[7]
    set -l stashed $out[8]
    set -l clean $out[9]
    set -l is_jj $out[10]

    set -l normal (set_color normal)
    set -l result

    if test "$is_jj" = 1; and type -q jj
        set -l jjcmd jj log --ignore-working-copy --no-graph --color never

        # one-letter markers (one per line), colorized below
        set -l tokens ($jjcmd -r @ -T '
            separate("\n",
              "JJ",
              if(conflict, "C"),
              if(empty, "E"),
              if(description.len() == 0, "N"),
              if(divergent, "D"),
              if(hidden, "H"))
        ')
        set -l ahead (string length --visible ($jjcmd -T '"x"' -r 'trunk()..@ ~ empty()' 2>/dev/null | string collect -a))
        set -l behind (string length --visible ($jjcmd -T '"x"' -r '@..trunk()' 2>/dev/null | string collect -a))

        set -a result 'JJ:'
        set -e tokens[1] # drop the "JJ" marker
        for t in $tokens
            switch $t
                case C
                    set -a result (set_color red)'✖'$normal
                case E
                    set -a result (set_color cyan)'∅'$normal
                case N
                    set -a result (set_color yellow)'-'$normal
                case D
                    set -a result (set_color red)'(divergent)'$normal
                case H
                    set -a result (set_color red)'(hidden)'$normal
            end
        end
        test "$ahead" -gt 0; and set -a result '↑'$ahead
        test "$behind" -gt 0; and set -a result '↓'$behind
        # "untracked" is also considered as changed for jj
        # (https://github.com/jj-vcs/jj/discussions/7406)
        set -l changes (math $changed + $untracked)
        test $changes -gt 0; and set -a result (set_color blue)'✚'$changes$normal

        string join '' $result
        return
    end

    # plain git repo
    if contains -- $remote . _NO_REMOTE_TRACKING_
        set remote ''
    else
        set remote (string replace -a -- _AHEAD_ '↑' $remote | string replace -a -- _BEHIND_ '↓')
    end

    set -a result (set_color --bold magenta)$branch$normal
    test -n "$remote"; and set -a result $remote
    set -a result '|'
    test "$staged" != 0; and set -a result (set_color red)'●'$staged$normal
    test "$conflicts" != 0; and set -a result (set_color red)'✖'$conflicts$normal
    test "$changed" != 0; and set -a result (set_color blue)'✚'$changed$normal
    test "$untracked" != 0; and set -a result '…'$untracked
    test "$stashed" != 0; and set -a result '⚑'$stashed
    test "$clean" = 1; and set -a result (set_color --bold green)'✔'$normal

    string join '' $result
end

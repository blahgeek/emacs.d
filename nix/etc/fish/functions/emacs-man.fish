# Show a man page in the host Emacs (xonshconf/emacs.py emacs-man)
function emacs-man
    if test (count $argv) -eq 0
        echo "Supports -a, -l, -k.
Args are passed to emacs `man' function directly.
See emacs documentation for details." >&2
        return 1
    end
    for arg in $argv
        if string match -q -- '-*' $arg; and not contains -- $arg -a -l -k
            echo "Arg \"$arg\" not supported" >&2
            return 1
        end
    end
    _eat_term_cmd man (string join ' ' $argv)
end

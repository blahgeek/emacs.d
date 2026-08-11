# Send a command to the host Emacs through EAT's OSC 51 message channel
# (see xonshconf/emacs.py term_cmd and eat-message-handler-alist in init.el).
# Protocol: OSC "51;e;M;" b64(arg1) ";" b64(arg2) ... ST
# Returns 1 without doing anything when not inside EAT.
function _eat_term_cmd
    string match -qr '(^|,)eat$' -- "$INSIDE_EMACS"; or return 1

    set -l encoded
    for arg in $argv
        set -a encoded (printf %s "$arg" | base64 | tr -d '\n')
    end
    set -l content '51;e;M;'(string join ';' $encoded)

    if set -q TMUX
        # tmux passthrough with doubled ESC
        printf '\x1bPtmux;\x1b\x1b]%s\a\x1b\\' "$content"
    else if string match -q 'screen*' -- "$TERM"
        printf '\x1bP\x1b]%s\a\x1b\\' "$content"
    else
        printf '\x1b]%s\x1b\\' "$content"
    end
end

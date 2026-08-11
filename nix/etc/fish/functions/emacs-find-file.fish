# Open a file in the host Emacs (xonshconf/emacs.py find-file)
function emacs-find-file
    if test (count $argv) -ne 1
        echo 'usage: emacs-find-file FILE' >&2
        return 1
    end
    echo "Finding file $argv[1]" >&2
    _eat_term_cmd find-file $argv[1]
end

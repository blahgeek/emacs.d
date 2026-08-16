# Run rg, results shown in the host Emacs (xonshconf/emacs.py emacs-rg)
function emacs-rg
    _emacs_term_cmd rg-run-raw (string join ' ' (string escape -- $argv)) $PWD
end

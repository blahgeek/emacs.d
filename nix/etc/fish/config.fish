# fish configuration
# Migrated from xonsh: ~/.emacs.d/xonsh_rc.xsh + ~/.emacs.d/xonshconf/*
# Prompt, greeting and helper functions live in functions/.

# ---- Environment (xonshconf/env.xsh) ----

set fish_function_path (status dirname)/functions $fish_function_path

if test (uname) = Darwin
    set -gx LC_ALL en_US.UTF-8
    set -gx LANG en_US.UTF-8
end

if status is-interactive
    # ---- Abbreviations (xonshconf/env.xsh abbrevs) ----
    # NOTE: the xonsh abbrevs could also expand after "sudo " (allow_sudo);
    # fish abbreviations cannot do that, intentionally dropped.
    abbr -a --position command du 'du -h'
    abbr -a --position command df 'df -h'
    abbr -a --position command ll 'ls -alh'
    abbr -a --position command sxiv 'sxiv -a' # autoplay gif
    abbr -a --position command ssh sshrc
    abbr -a --position command mosh moshrc

    # ---- Aliases ----
    alias x 'dtrx -r -n' # usually use "x" as temp var

    if test (uname) = Darwin
        # coreutils ls from macports: supports colors in EAT,
        # the builtin macOS ls cannot load EAT terminfo
        alias ls 'gls --color=auto'
    end

    # pbcopy/pbpaste fallback when the system does not provide them.
    # (The xonsh config also had emacs-pbcopy/emacs-pbpaste aliases for
    # non-graphical emacs, but those commands were never defined -- dropped.)
    if not type -q pbcopy; and type -q xclip
        alias pbpaste 'xclip -selection clipboard -o'
        alias pbcopy 'xclip -selection clipboard'
    end

    if string match -qr '(^|,)(eat|ghostel)$' -- "$INSIDE_EMACS"
        abbr -a --position command vi emacs-find-file
        abbr -a --position command vim emacs-find-file
        abbr -a --position command gits emacs-magit-status
        abbr -a --position command rg emacs-rg
        abbr -a --position command ag emacs-rg
        abbr -a --position command man emacs-man
    end

    # ---- Keybindings (xonshconf/keybindings.py) ----
    # Open the command line in $EDITOR with C-x / C-x C-x.
    # NOTE: this overrides fish 4's default C-x (copy to clipboard).
    bind \cx edit_command_buffer
    bind \cx\cx edit_command_buffer
    # NOTE: C-w (backward-kill-path-component) is already fish's default,
    # nothing to port.

    # https://github.com/fish-shell/fish-shell/issues/11327
    bind ctrl-c cancel-commandline
end

for i in (status dirname)/conf.d/*.fish
    source $i
end

if test -n "$EMACS_GHOSTEL_PATH" -a -f "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
    source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
end

# ---- Intentionally not migrated ----
# - $AUTO_CD:                     builtin in fish
# - $XONSH_HISTORY_* / $HISTCONTROL: fish history behaves differently, defaults are fine
# - $XONSH_COLOR_STYLE etc.:      fish colors via fish_color_* (conf.d/fish_frozen_theme.fish)
# - xontrib autojump:             no equivalent configured; consider zoxide if needed
# - $TITLE:                       fish has a builtin fish_title (command + cwd)
# - $BASH_COMPLETIONS:            fish uses its own completion system
# - completions/async-prompt tuning: xonsh/prompt_toolkit specific
# - xonshconf/envvar_filter.py:   works around xonsh env leaking into subprocesses, not needed
# - vterm/ghostel emacs integration: dropped on purpose, EAT only

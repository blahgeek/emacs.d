# Syntax/UI colors, self-maintained.
# Mirrors xonsh's default color style (XONSH_BASE_STYLE in xonsh/pyghooks.py)
# plus the two $XONSH_STYLE_OVERRIDES from xonshconf/env.xsh.
# ANSI color names only (no truecolor): the terminal palette itself is
# solarized, so colors follow when the terminal switches light/dark.
#
# xonsh -> fish name mapping:
#   ansibrightblack -> brblack   ansigray -> white   ansiwhite -> brwhite
#   ansibrightX     -> brX       others keep their canonical names

fish_config theme choose solarized --color-theme=light

# set -g fish_color_normal normal
set -g fish_color_command green --bold          # Name.Builtin (env.xsh override: ansigreen bold)
set -g fish_color_function green --bold         # Name.Builtin (env.xsh override: ansigreen bold)
set -g fish_color_builtin green --bold          # Name.Builtin (env.xsh override: ansigreen bold)
set -g fish_color_keyword green --bold          # Keyword: bold ansigreen
set -g fish_color_quote yellow                  # Literal.String: ansiyellow
# set -g fish_color_escape yellow --bold          # Literal.String.Escape: bold ansiyellow
# set -g fish_color_redirection brblack           # Operator: ansibrightblack
# set -g fish_color_end brblack                   # Operator
# set -g fish_color_operator brblack              # Operator
# set -g fish_color_param normal                  # unquoted args: default text in xonsh
# set -g fish_color_option normal
# set -g fish_color_comment cyan                  # Comment: ansicyan
# set -g fish_color_error brred                   # Error: ansibrightred
# set -g fish_color_autosuggestion brblack        # PTK.AutoSuggestion: ansibrightblack
# set -g fish_color_cancel brblack                # PTK.Aborting: ansibrightblack
# set -g fish_color_status red

# # no direct xonsh counterpart, kept from the previous theme
# set -g fish_color_history_current --bold
# set -g fish_color_valid_path --underline=single
# set -g fish_color_search_match bryellow --bold --background=white
# set -g fish_color_selection white --bold --background=brblack

# # completion pager: kept from the previous fish solarized theme (no xonsh counterpart)
# set -g fish_pager_color_completion green
# set -g fish_pager_color_description yellow
# set -g fish_pager_color_prefix cyan --underline=single
# set -g fish_pager_color_progress brwhite --bold --background=cyan
# set -g fish_pager_color_selected_background --background=white

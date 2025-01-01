import os
import sys
import shutil
from prompt_toolkit import __version__ as PROMPT_TOOLKIT_VERSION
from xonsh import __version__ as XONSH_VERSION
from xonsh.tools import register_custom_style
from xonshconf.utils import register_alias, inside_emacs, smart_cwd, make_cmd_abbrev
from xonshconf.git_prompt import git_prompt

def _version_tuple(s):
    return tuple(int(x) for x in s.split('.')[:3])

_has_builtin_last_return_code = _version_tuple(XONSH_VERSION) >= (0, 12, 5)

xontrib load abbrevs autojump
if not _has_builtin_last_return_code:
    xontrib load prompt_ret_code

# Use default style because it uses ansi colors, instead of fixed RGB values;
# which allows us to easily switch themes in terminal (emacs)
$XONSH_COLOR_STYLE = "default"
# reference: `xonfig colors`,
# `from xonsh.built_ins import XSH`, `XSH.shell.shell.get_prompt_style().style_rules`
$XONSH_STYLE_OVERRIDES = {
    'Token.Name.Builtin': 'ansigreen bold',  # commands. default is "ansigreen"
}

# https://github.com/xonsh/xonsh/issues/5179
if _version_tuple(PROMPT_TOOLKIT_VERSION) >= (3, 0, 39):
    $XONSH_STYLE_OVERRIDES['Token.PTK.CompletionMenu.Completion.Current'] = \
        "ansibrightblack bg:ansiwhite"  # it's actually reversed

if $TERM == 'xterm-mono':
    $PROMPT_TOOLKIT_COLOR_DEPTH = 'DEPTH_1_BIT'

$PROMPT_FIELDS['smart_cwd'] = smart_cwd
$PROMPT_FIELDS['git_prompt'] = git_prompt
$PROMPT = ('{env_name}'
           '{CYAN}{hostname} {YELLOW}{smart_cwd}'
           '{RESET}{git_prompt: ({})}{RESET}'
           '{RED}' +
           ('{last_return_code_if_nonzero:[{}]}'
            if _has_builtin_last_return_code
            else '{ret_code:{}}') +
           '{RESET}'
           '>{RESET} ')

# emacs (except vterm) cannot handle this
if inside_emacs() in ('vterm', ''):
    $TITLE = '{current_job:{} | }xonsh'
else:
    $TITLE = ''

# XONSH related ENVS
$XONSH_HISTORY_BACKEND = 'sqlite'
$AUTO_CD = True
$COMPLETIONS_BRACKETS = False
$COMPLETIONS_MENU_ROWS = 10
$XONSH_HISTORY_MATCH_ANYWHERE = True
$XONSH_HISTORY_SIZE = (1024 * 1024, 'commands')
$HISTCONTROL = 'erasedups'
$LS_COLORS = None
$ENABLE_ASYNC_PROMPT = True
$ASYNC_PROMPT_THREAD_WORKERS = 2
$ASYNC_INVALIDATE_INTERVAL = 0.01

# locale
if sys.platform.startswith('darwin'):
    $LC_ALL = "en_US.UTF-8"
    $LANG = "en_US.UTF-8"

# ABBREVS and ALIASES

if inside_emacs() in ('vterm', 'eat'):
    abbrevs['vi'] = make_cmd_abbrev('emacs-find-file')
    abbrevs['vim'] = make_cmd_abbrev('emacs-find-file')
    abbrevs['gits'] = make_cmd_abbrev('emacs-magit-status')
    abbrevs['rg'] = make_cmd_abbrev('emacs-rg')
    abbrevs['ag'] = make_cmd_abbrev('emacs-rg')
else:
    abbrevs['vi'] = make_cmd_abbrev('nvim', allow_sudo = True)
    abbrevs['vim'] = make_cmd_abbrev('nvim', allow_sudo = True)

abbrevs['du'] = make_cmd_abbrev('du -h', allow_sudo = True)
abbrevs['df'] = make_cmd_abbrev('df -h', allow_sudo = True)
aliases['x'] = 'dtrx -r -n'  # usually use "x" as temp var
abbrevs['ll'] = make_cmd_abbrev('ls -alh', allow_sudo = True)
abbrevs['sxiv'] = make_cmd_abbrev('sxiv -a', allow_sudo = True)  # autoplay gif
abbrevs['ssh'] = make_cmd_abbrev('sshrc')
abbrevs['mosh'] = make_cmd_abbrev('moshrc')

if sys.platform.startswith('darwin'):
    # use coreutils from macports, to support color in EAT terminal (the builtin "ls" does not support loading EAT terminfo)
    aliases['ls'] = 'gls --color=auto'

if shutil.which('xclip'):
    aliases['pbpaste'] = 'xclip -selection clipboard -o'
    aliases['pbcopy'] = 'xclip -selection clipboard'

if shutil.which('doggo'):
    abbrevs['dig'] = 'doggo'

@register_alias()
def gopath_here():
    '''Add PWD to GOPATH'''
    $GOPATH.append(os.getcwd())
    print('GOPATH:', $GOPATH)

@register_alias()
def mkcd(args):
    if len(args) != 1:
        return -1
    os.makedirs(args[0], exist_ok=True)
    cd @(args[0])

# ENVS
$SSHHOME = $XONSH_CONFIG_DIR + '/sshrc'
$GOPATH = $HOME + '/Code/GO'
$PARALLEL_SHELL = '/bin/sh'
if not ${...}.get('EDITOR'):
    $EDITOR = 'nvim'

if not inside_emacs():
    $MANPAGER = "nvim -c 'set ft=man' -"
elif ${...}.get('MANPAGER'):
    del $MANPAGER

# https://bugs.launchpad.net/libvterm/+bug/1994966
$GREP_COLORS = 'ne'

for _path in (p'~/.npm/bin',
              p'~/.cargo/bin',
              p'~/.rvm/bin',
              p'$GOPATH/bin',
              p'/opt/local/bin',
              p'/opt/local/sbin',
              p'/usr/local/bin',
              p'/usr/local/sbin',
              p'~/Library/Android/sdk/platform-tools',
              p'~/Library/Android/sdk/ndk-bundle',
              p'/usr/local/opt/ruby/bin',
              p'~/.local/bin',
              p'~/.config/xonsh/bin',
              p'~/.npm-packages/bin'):
    if _path.is_dir():
        $PATH.insert(0, _path)

os.environ['PATH'] = ':'.join($PATH)

# GPG
if shutil.which('gpgconf'):
    $[gpgconf --launch gpg-agent]
    $SSH_AUTH_SOCK = $(gpgconf --list-dirs agent-ssh-socket).strip()
    # nix-installed git would use a nix-installed ssh, which cannot work with gpg
    $GIT_SSH_COMMAND = "/usr/bin/ssh"

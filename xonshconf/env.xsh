import os
from xonsh.tools import register_custom_style
from xonshconf.utils import register_alias, inside_emacs, smart_cwd, make_cmd_abbrev
from xonshconf.git_prompt import git_prompt

xontrib load abbrevs autojump prompt_ret_code

# For some reason, this is different than simply setting to default
register_custom_style("mystyle", {}, base="default")
$XONSH_COLOR_STYLE="mystyle"

$PROMPT_FIELDS['smart_cwd'] = smart_cwd
$PROMPT_FIELDS['git_prompt'] = git_prompt
$PROMPT = ('{env_name}'
           '{CYAN}{hostname} {YELLOW}{smart_cwd}'
           '{RESET}{git_prompt: ({})}{RESET}'
           '{RED}{ret_code:{}}{RESET}'
           '>{RESET} ')

# emacs (except vterm) cannot handle this
if inside_emacs() in ('vterm', ''):
    $TITLE = '{current_job:{} | }{smart_cwd} | xonsh'
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

# ABBREVS and ALIASES

if inside_emacs() == 'vterm':
    abbrevs['vi'] = make_cmd_abbrev('emacs-find-file')
    abbrevs['vim'] = make_cmd_abbrev('emacs-find-file')
    abbrevs['gits'] = make_cmd_abbrev('emacs-magit-status')
else:
    abbrevs['vi'] = make_cmd_abbrev('nvim', allow_sudo = True)
    abbrevs['vim'] = make_cmd_abbrev('nvim', allow_sudo = True)

abbrevs['du'] = make_cmd_abbrev('du -h', allow_sudo = True)
abbrevs['df'] = make_cmd_abbrev('df -h', allow_sudo = True)
aliases['x'] = 'dtrx -r -n'  # usually use "x" as temp var
abbrevs['ll'] = make_cmd_abbrev('ls -alh', allow_sudo = True)
abbrevs['sxiv'] = make_cmd_abbrev('sxiv -a', allow_sudo = True)  # autoplay gif
abbrevs['ssh'] = make_cmd_abbrev('sshrc')

if !(which xclip):
    aliases['pbpaste'] = 'xclip -selection clipboard -o'
    aliases['pbcopy'] = 'xclip -selection clipboard'

@register_alias()
def gopath_here():
    '''Add PWD to GOPATH'''
    $GOPATH.append(os.getcwd())
    print(f'GOPATH: {$GOPATH}')

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

for _path in (gp`~/.npm/bin`,
              gp`$GOPATH/bin`,
              gp`/opt/local/bin`,
              gp`/opt/local/sbin`,
              gp`/usr/local/bin`,
              gp`/usr/local/sbin`,
              gp`~/Library/Android/sdk/platform-tools`,
              gp`~/Library/Android/sdk/ndk-bundle`,
              gp`/usr/local/opt/ruby/bin`,
              gp`~/.local/bin`,
              gp`~/.config/xonsh/bin`):
    if _path and _path[0].is_dir():
        $PATH.insert(0, _path[0])

os.environ['PATH'] = ':'.join($PATH)

# GPG
if !(which gpgconf):
    $[gpgconf --launch gpg-agent]
    $SSH_AUTH_SOCK = $(gpgconf --list-dirs agent-ssh-socket).strip()
    # nix-installed git would use a nix-installed ssh, which cannot work with gpg
    $GIT_SSH_COMMAND = "/usr/bin/ssh"

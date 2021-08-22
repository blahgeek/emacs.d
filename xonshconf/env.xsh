import os
from xonsh.tools import register_custom_style
from xonshconf.utils import register_alias, inside_emacs, smart_cwd
from xonshconf.git_prompt import git_prompt

xontrib load abbrevs autojump

# For some reason, this is different than simply setting to default
register_custom_style("mystyle", {}, base="default")
$XONSH_COLOR_STYLE="mystyle"

def _prompt_last_status():
    if __xonsh__.history.rtns:
        last_ret = __xonsh__.history.rtns[-1]
        if last_ret != 0:
            return str(last_ret)

$PROMPT_FIELDS['smart_cwd'] = smart_cwd
$PROMPT_FIELDS['git_prompt'] = git_prompt
$PROMPT_FIELDS['last_status'] = _prompt_last_status
$PROMPT = ('{env_name}'
           '{CYAN}{hostname} {YELLOW}{smart_cwd}'
           '{RESET}{git_prompt: ({})}{RESET}'
           '{RED}{last_status:[{}]}{RESET}'
           '>{RESET} ')

# emacs (except vterm) cannot handle this
if inside_emacs() in ('vterm', ''):
    $TITLE = '{current_job:{} | }{smart_cwd} | xonsh'
else:
    $TITLE = ''

# XONSH related ENVS
$UPDATE_OS_ENVIRON = True
$AUTO_CD = True
$COMPLETIONS_BRACKETS = False
$COMPLETIONS_MENU_ROWS = 10

# ABBREVS and ALIASES

if inside_emacs() == 'vterm':
    abbrevs['vi'] = 'emacs-find-file'
    abbrevs['vim'] = 'emacs-find-file'
    abbrevs['gits'] = 'emacs-magit-status'
else:
    abbrevs['vi'] = 'nvim'
    abbrevs['vim'] = 'nvim'

abbrevs['du'] = 'du -h'
abbrevs['df'] = 'df -h'
aliases['x'] = 'dtrx -r -n'  # usually use "x" as temp var
abbrevs['ll'] = 'ls -alh'
abbrevs['sxiv'] = 'sxiv -a'  # autoplay gif
abbrevs['ssh'] = 'sshrc'

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
if not os.environ.get('EDITOR'):
    $EDITOR = 'nvim'

if not inside_emacs():
    $MANPAGER = "nvim -c 'set ft=man' -"
elif os.environ.get('MANPAGER'):
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


# GPG
if !(which gpgconf):
    $[gpgconf --launch gpg-agent]
    $SSH_AUTH_SOCK = $(gpgconf --list-dirs agent-ssh-socket)

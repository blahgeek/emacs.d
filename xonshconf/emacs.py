#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import base64
import json
import shlex
import subprocess

from xonsh.tools import unthreadable
from xonshconf.utils import register_alias


def vterm_printf(content):
    if os.environ.get('TMUX'):
        print(f'\x1bPtmux;\x1b\x1b]{content}%s\x07\x1b\\')
    elif os.environ.get('TERM', '').startswith('screen'):
        print(f'\x1bP\x1b]{content}\x07\x1b\\')
    else:
        print(f'\x1b]{content}\x1b\\')

def vterm_cmd(*args):
    # see emacs init.el my/vterm-eval-base64-json
    encoded_cmd = base64.b64encode(json.dumps(args).encode()).decode()
    vterm_printf(f'51;Eeval-base64-json {encoded_cmd}')

@unthreadable   # required for input(), otherwise we cannot cancel it
@register_alias('emacs-find-file')
def find_file(args):
    assert len(args) == 1
    filename = args[0]

    print(f'Finding file {filename}', file=sys.stderr)
    vterm_cmd('find-file', filename)

@register_alias('man')
def emacs_man(args):
    assert len(args) == 1
    path = subprocess.run(['man', '-w', args[0]],
                          stdout=subprocess.PIPE,
                          universal_newlines=True,
                          check=True).stdout.strip()
    vterm_cmd('woman-find-file', path)

@register_alias('emacs-magit-status')
def magit_status():
    vterm_cmd('magit-status')

@register_alias('emacs-rg')
def emacs_rg(args):
    vterm_cmd('rg-run-raw', shlex.join(args), os.getcwd())

@events.on_chdir
def set_pwd(olddir, newdir, *args, **kwargs):
    vterm_cmd('set-pwd', newdir)

@events.on_postcommand
def set_pwd_after_ssh(cmd: str, *args, **kwargs):
    cmd_args = cmd.split()
    if not ('ssh' in cmd_args or 'sshrc' in cmd_args):
        return
    vterm_cmd('set-pwd', os.getcwd())

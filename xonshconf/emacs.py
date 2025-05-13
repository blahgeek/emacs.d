#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import base64
import json
import re
import shlex
import subprocess
import platform

from xonsh.tools import unthreadable
from xonshconf.utils import register_alias, inside_emacs


def _str_base64(s):
    return base64.b64encode(s.encode()).decode()

def _term_escape(content):
    if os.environ.get('TMUX'):
        return f'\x1bPtmux;\x1b\x1b]{content}%s\x07\x1b\\'
    elif os.environ.get('TERM', '').startswith('screen'):
        return f'\x1bP\x1b]{content}\x07\x1b\\'
    else:
        return f'\x1b]{content}\x1b\\'

def term_printf(content):
    print(_term_escape(content), end='')

def term_cmd(*args):
    if inside_emacs() == 'vterm':
        # see emacs init.el my/vterm-eval-base64-json
        encoded_cmd = _str_base64(json.dumps(args))
        term_printf(f'51;Eeval-base64-json {encoded_cmd}')
    elif inside_emacs() == 'eat':
        term_printf('51;e;M;' + ';'.join(_str_base64(x) for x in args))
    else:
        raise RuntimeError('term_cmd not supported in current terminal')

def eat_set_cwd(path: str):
    term_printf('51;e;A;{};{}'.format(_str_base64(platform.node()), _str_base64(path)))

@unthreadable   # required for input(), otherwise we cannot cancel it
@register_alias('emacs-find-file')
def find_file(args):
    assert len(args) == 1
    filename = args[0]

    print(f'Finding file {filename}', file=sys.stderr)
    term_cmd('find-file', filename)

@register_alias('emacs-man')
def emacs_man(args):
    if not args:
        print('Supports -a, -l, -k.\n'
              'Args are passed to emacs `man\' function directly.\n'
              'See emacs documentation for details.',
              file=sys.stderr)
        return

    valid_args = []
    for arg in args:
        if arg.startswith('-') and arg not in ('-a', '-l', '-k'):
            print(f'Arg "{arg}" not supported', file=sys.stderr)
            return
        valid_args.append(arg)

    term_cmd('man', ' '.join(valid_args))

@register_alias('emacs-magit-status')
def magit_status():
    term_cmd('magit-status')

@register_alias('emacs-rg')
def emacs_rg(args):
    term_cmd('rg-run-raw', shlex.join(args), os.getcwd())

@events.on_chdir
def set_pwd(olddir, newdir, *args, **kwargs):
    if inside_emacs() == 'vterm':
        term_cmd('set-pwd', newdir)
    else:
        eat_set_cwd(os.getcwd())

@events.on_pre_prompt
def pre_prompt(*args, **kwargs):
    if inside_emacs() == 'vterm':
        term_cmd('set-pwd', os.getcwd())
    elif inside_emacs() == 'eat':
        term_printf('51;e;J')
        eat_set_cwd(os.getcwd())
        term_printf('51;e;B')  # see below

@events.on_post_prompt
def post_prompt():
    # NOTE: this event is actually fired after a command is read (not after the prompt is printed)
    # the result is that EAT will update prompt annotation with one-command delay.
    # Ideally it should be put into $PROMPT (together with 51;e;B),
    # but xonsh would refresh the $PROMPT multiple times and confuses EAT.
    if inside_emacs() == 'eat':
        term_printf('51;e;C')

@events.on_postcommand
def postcommand(cmd, rtn, out, ts):
    if inside_emacs() == 'eat':
        term_printf(f'51;e;H;{rtn}')

@events.on_precommand
def precommand(cmd: str):
    if inside_emacs() == 'eat':
        term_printf('51;e;F;' + _str_base64(cmd))
        term_printf('51;e;G')

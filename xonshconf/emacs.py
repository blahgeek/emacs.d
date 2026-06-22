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

def _term_escape(content, *, bell_end=False):
    if os.environ.get('TMUX'):
        return f'\x1bPtmux;\x1b\x1b]{content}%s\x07\x1b\\'
    elif os.environ.get('TERM', '').startswith('screen'):
        return f'\x1bP\x1b]{content}\x07\x1b\\'
    else:
        return f'\x1b]{content}' + ('\a' if bell_end else '\x1b\\')

def term_printf(content, *, bell_end=False):
    print(_term_escape(content, bell_end=bell_end), end='')

def term_cmd(*args):
    if inside_emacs() == 'vterm':
        # see emacs init.el my/vterm-eval-base64-json
        encoded_cmd = _str_base64(json.dumps(args))
        term_printf(f'51;Eeval-base64-json {encoded_cmd}')
    elif inside_emacs() == 'eat':
        term_printf('51;e;M;' + ';'.join(_str_base64(x) for x in args))
    elif inside_emacs() == 'ghostel':
        # see my/ghostel-eval-b64-cmd
        encoded_cmd = _str_base64(json.dumps(args))
        term_printf(f'52;e;"eval-b64-cmd" "{encoded_cmd}" ')
    else:
        raise RuntimeError('term_cmd not supported in current terminal')

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
    term_cmd('set-cwd', newdir)

@events.on_pre_prompt
def pre_prompt(*args, **kwargs):
    term_cmd('set-cwd', os.getcwd())
    if inside_emacs() == 'eat':
        term_printf('51;e;J')
        term_printf('51;e;B')  # see below
    elif inside_emacs() == 'ghostel':
        term_printf('133;A', bell_end=True)

@events.on_post_prompt
def post_prompt():
    # NOTE: this event is actually fired after a command is read (not after the prompt is printed)
    # the result is that EAT will update prompt annotation with one-command delay.
    # Ideally it should be put into $PROMPT (together with 51;e;B),
    # but xonsh would refresh the $PROMPT multiple times and confuses EAT.
    if inside_emacs() == 'eat':
        term_printf('51;e;C')
    elif inside_emacs() == 'ghostel':
        term_printf('113;B', bell_end=True)

@events.on_postcommand
def postcommand(cmd, rtn, out, ts):
    if inside_emacs() == 'eat':
        term_printf(f'51;e;H;{rtn}')
    elif inside_emacs() == 'ghostel':
        term_printf(f'133;D;{rtn}', bell_end=True)

@events.on_precommand
def precommand(cmd: str):
    if inside_emacs() == 'eat':
        term_printf('51;e;F;' + _str_base64(cmd))
        term_printf('51;e;G')
    elif inside_emacs() == 'ghostel':
        term_printf('133;C', bell_end=True)

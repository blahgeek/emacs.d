#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys

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
    vterm_printf('51;E' + ' '.join(x.replace('\\', '\\\\').replace('"', '\\"')
                                   for x in args))

@unthreadable   # required for input(), otherwise we cannot cancel it
@register_alias('emacs-find-file')
def find_file(args):
    assert len(args) == 1
    filename = args[0]

    if os.path.exists(filename):
        filename = os.path.abspath(filename)
        if not os.access(filename, os.R_OK):
            filename = f'/sudo::{filename}'
            print(f'File is not readable, try opening {filename}. Continue? ',
                  file=sys.stderr, end='')
            input()

    print(f'Finding file {filename}', file=sys.stderr)
    vterm_cmd('find-file', filename)

@register_alias('man')
def emacs_man(args):
    assert len(args) == 1
    vterm_cmd('man', args[0])

@register_alias('emacs-magit-status')
def magit_status():
    vterm_cmd('magit-status')

@events.on_chdir
def set_pwd(olddir, newdir, *args, **kwargs):
    vterm_cmd('set-pwd', newdir)

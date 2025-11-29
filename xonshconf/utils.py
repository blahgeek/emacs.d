#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import functools


def register_alias(name: str = None):
    '''Register function as alias, with optional name.'''
    def _register(f):
        alias_name = name if name else f.__name__
        aliases[alias_name] = f
        return f
    return _register


def make_cmd_abbrev(expand: str, allow_sudo = False):
    '''Return a function suitable for abbrevs value, that only expands for command (first word)'''
    def _fn(buffer, word):
        if buffer.text.startswith(word) or \
           (allow_sudo and buffer.text.startswith('sudo ' + word)):
            return expand
        return word
    return _fn


@functools.cache
def inside_emacs():
    '''Is inside emacs. Return "vterm", "eat", "" or None'''
    s = os.environ.get('INSIDE_EMACS', '')
    return s.split(',')[-1]


@functools.cache
def emacs_display_graphic_p():
    return bool(os.environ.get('EMACS_DISPLAY_GRAPHIC_P', ''))


def smart_cwd():
    '''Return cwd as short string, like short_cwd, but disable shorting for certain components'''
    def _shorten(s):
        end = len(s)
        if end > 0:
            for i, c in enumerate(s):
                if c not in ('.', '_'):
                    end = i + 1
                    break
        return s[:end]

    cwd = os.getcwd()
    home = os.environ.get('HOME', '')
    if home and cwd.startswith(home):
        cwd = '~' + cwd[len(home):]
    components = cwd.split('/')
    for i in range(len(components) - 1):
        if not components[i]:
            continue
        if components[i][-1].isdigit():  # do not shorten component ending with digit ("pony_4")
            continue
        if i + 1 < len(components) and components[i+1] == '.sub-repos':
            continue
        components[i] = _shorten(components[i])

    return '/'.join(components)

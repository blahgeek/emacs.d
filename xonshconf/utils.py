#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os


def register_alias(name: str = None):
    '''Register function as alias, with optional name.'''
    def _register(f):
        alias_name = name if name else f.__name__
        aliases[alias_name] = f
        return f
    return _register


def inside_emacs():
    '''Is inside emacs'''
    return os.environ.get('INSIDE_EMACS', '')


def smart_cwd():
    '''Return cwd as short string, like short_cwd, but do not strip digits-ending components'''
    cwd = os.getcwd()
    home = os.environ.get('HOME', '')
    if home and cwd.startswith(home):
        cwd = '~' + cwd[len(home):]
    components = cwd.split('/')
    for n in range(len(components) - 1):
        component = components[n]
        idx = len(component)
        if idx > 0 and not component[-1].isdigit():
            for i, c in enumerate(component):
                if c not in ('.', '_'):
                    idx = i + 1
                    break
        components[n] = component[:idx]
    return '/'.join(components)

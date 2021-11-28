#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re

from prompt_toolkit.keys import Keys
from prompt_toolkit.key_binding.key_processor import KeyPressEvent


# find_start_of_previous_word with **reverse** the string and then match
_BACKWARD_KILL_PATH_COMPONENT_PATTERN = re.compile(r'''\s*[/=.,:]*[^/=.,:\s]*''')

@events.on_ptk_create
def custom_keybindings(bindings, **kw):

    bindings.remove(Keys.ControlX, Keys.ControlE)
    bindings.remove(Keys.ControlX, Keys.ControlX)
    bindings.remove(Keys.ControlX, Keys.ControlC)

    @bindings.add(Keys.ControlX)
    @bindings.add(Keys.ControlX, Keys.ControlX)
    def open_editor(event):
        """Open current buffer in editor"""
        event.current_buffer.open_in_editor(event.cli)

    @bindings.add(Keys.ControlW)
    def backward_kill_path_component(event: KeyPressEvent):
        """Fish-like 'Ctrl-W', see https://fishshell.com/docs/current/cmds/bind.html
        Mostly copied from prompt_toolkit.key_binding.bindings.named_commands.unix_word_rubout.
        Also remove 'event.app.clipboard.set_text(deleted)"""
        buff = event.current_buffer
        pos = buff.document.find_start_of_previous_word(
            count=event.arg,
            pattern=_BACKWARD_KILL_PATH_COMPONENT_PATTERN)

        if pos is None:
            # Nothing found? delete until the start of the document.  (The
            # input starts with whitespace and no words were found before the
            # cursor.)
            pos = -buff.cursor_position

        if pos:
            buff.delete_before_cursor(count=-pos)
        else:
            # Nothing to delete. Bell.
            event.app.output.bell()

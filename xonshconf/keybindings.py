#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from prompt_toolkit.keys import Keys


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

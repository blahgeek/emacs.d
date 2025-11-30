# Do not put code in this file so that the namespace is not polluted

# import some common modules for easy access
import os
import sys
import json
import time

import xonshconf.env
import xonshconf.emacs
import xonshconf.keybindings
import xonshconf.greeting
import xonshconf.envvar_filter

# https://github.com/prompt-toolkit/python-prompt-toolkit/issues/1696
import warnings
warnings.filterwarnings('ignore', 'There is no current event loop', DeprecationWarning, 'prompt_toolkit.eventloop.utils')

xonshconf.greeting.greeting()

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import time
import subprocess

def greeting():
    for cmd in ('/usr/bin/archey3', '/usr/bin/archey'):
        if os.path.exists(cmd):
            subprocess.run([cmd])
            return
    uname = os.uname()
    print()
    print(f'Machine:  {uname.sysname} {uname.release} {uname.nodename}')
    print(f'Date:     {time.ctime()}')
    print(f'Uptime:   ' + subprocess.check_output(['uptime'], universal_newlines=True))

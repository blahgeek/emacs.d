#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import time
import subprocess

def greeting():
    uname = os.uname()
    print()
    print(f'Machine:  {uname.sysname} {uname.release} {uname.nodename}')
    print(f'Date:     {time.ctime()}')
    print(f'Uptime:   ' + subprocess.check_output(['uptime'], universal_newlines=True).strip())
    print()

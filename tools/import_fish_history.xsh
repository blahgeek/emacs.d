import time

for line in $(fish -c history).split('\n'):
    line = line.strip()
    if not line:
        continue
    __xonsh__.history.append({'inp': line, 'rtn': 0})
__xonsh__.history.flush()

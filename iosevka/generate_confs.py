#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import shutil
import pathlib


LIGATURES = '''<-- <--- <<- <- -> ->> --> --->
<== <=== <<= <= => =>> ==> ===> >= >>=
<-> <--> <---> <----> <=> <==> <===> <====> -------->
<~~ <~ ~> ~~> :: ::: == != === !==
:= :- :+ <* <*> *> <| <|> |> +: -: =:
++ +++ <!-- <!--- <***>
'''.split()

CODEPOINT_OFFSET = 0xE100


def generate_build_plan(cwd: pathlib.Path):
    output_filepath = cwd / 'build-plans.toml'
    shutil.copy(cwd / 'build-plans.head.toml', output_filepath)
    with output_filepath.open('a') as f:
        for i, s in enumerate(LIGATURES):
            codepoint = i + CODEPOINT_OFFSET
            f.write(f'''
  [[buildPlans.iosevka-blah-mono.compatibility-ligatures]]
  unicode = {codepoint}  # 0x{codepoint:02X}
  featureTag = 'CLIK'
  sequence = '{s}'\n''')


def generate_elisp_data(cwd: pathlib.Path):
    with (cwd / 'ligature.el').open('w') as f:
        f.write('(\n')
        for i, s in enumerate(LIGATURES):
            codepoint = i + CODEPOINT_OFFSET
            f.write(f'("{s}" . #X{codepoint:02x})\n')
        f.write(')\n')


if __name__ == '__main__':
    cwd = pathlib.Path(__file__).resolve().parent
    generate_build_plan(cwd)
    generate_elisp_data(cwd)

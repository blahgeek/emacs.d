#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import os
import getpass
import pathlib
import subprocess


# From https://github.com/magicmonty/bash-git-prompt 2.7.1
# This is faster than fish_prompt.fish and more informative than xonsh's
_GITSTATUS_SH_BINARY = pathlib.Path(__file__).parent.parent / 'bin' / 'gitstatus.sh'
# this would make gitstatus.sh faster (default=all)
_ENVS = {
    '__GIT_PROMPT_SHOW_UNTRACKED_FILES': 'normal',
}
_TIMEOUT = 10
_USER_PREFIX = getpass.getuser() + '_'
_GITCONFIG_FSMONITOR = pathlib.Path.home() / '.gitconfig_fsmonitor'

_PROMPT_AHEAD = "↑"
_PROMPT_BEHIND = "↓"
_PROMPT_SEPARATOR = "|"
_PROMPT_BRANCH = "{BOLD_PURPLE}"
_PROMPT_STAGED = "{RED}●"
_PROMPT_CONFLICTS = "{RED}✖"
_PROMPT_CHANGED = "{BLUE}✚"
_PROMPT_REMOTE = ""
_PROMPT_UNTRACKED = "…"
_PROMPT_STASHED = "⚑"
_PROMPT_CLEAN = "{BOLD_GREEN}✔"
_PROMPT_RESET = '{RESET}'

def git_prompt():
    git_extra_args = ['--no-optional-locks', '-c', 'gc.auto=0', '-c', 'maintenance.auto=false']
    if _GITCONFIG_FSMONITOR.exists():
        git_extra_args += ['-c', f'include.path={_GITCONFIG_FSMONITOR}']

    env = os.environ.copy()  # os.environ will not be updated by xonsh
    env.update(_ENVS)
    env['__GIT_EXTRA_ARGS'] = ' '.join(git_extra_args)

    try:
        gitstatus_result = subprocess.check_output([_GITSTATUS_SH_BINARY],
                                                   stderr=subprocess.DEVNULL,
                                                   timeout=_TIMEOUT,
                                                   universal_newlines=True,
                                                   env=env)
    except subprocess.TimeoutExpired:
        return 'GIT_TIMEOUT'
    except subprocess.CalledProcessError:
        return 'GIT_ERROR'

    gitstatus_result = gitstatus_result.strip().split('\n')
    if len(gitstatus_result) != 9:
        return None

    (git_branch, git_remote, git_upstream, git_staged, git_conflicts,
     git_changed, git_untracked, git_stashed, git_clean) = gitstatus_result

    # change yikai_branch_name to branch_name. for pony
    if git_branch.startswith(_USER_PREFIX):
        git_branch = git_branch[len(_USER_PREFIX):]

    if git_remote in ('.', '_NO_REMOTE_TRACKING_'):
        git_remote = ''
    else:
        git_remote = git_remote\
            .replace('_AHEAD_', _PROMPT_AHEAD) \
            .replace('_BEHIND_', _PROMPT_BEHIND)

    result = _PROMPT_BRANCH + git_branch + _PROMPT_RESET
    if git_remote:
        result += _PROMPT_REMOTE + git_remote + _PROMPT_RESET
    result += _PROMPT_SEPARATOR
    if git_staged != '0':
        result += _PROMPT_STAGED + git_staged + _PROMPT_RESET
    if git_conflicts != '0':
        result += _PROMPT_CONFLICTS + git_conflicts + _PROMPT_RESET
    if git_changed != '0':
        result += _PROMPT_CHANGED + git_changed + _PROMPT_RESET
    if git_untracked != '0':
        result += _PROMPT_UNTRACKED + git_untracked + _PROMPT_RESET
    if git_stashed != '0':
        result += _PROMPT_STASHED + git_stashed + _PROMPT_RESET
    if git_clean == '1':
        result += _PROMPT_CLEAN + _PROMPT_RESET

    return result

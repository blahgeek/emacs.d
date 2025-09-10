#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import os
import getpass
from pathlib import Path
import subprocess


# From https://github.com/magicmonty/bash-git-prompt 2.7.1
# This is faster than fish_prompt.fish and more informative than xonsh's
_GITSTATUS_SH_BINARY = Path(__file__).parent.parent / 'bin' / 'gitstatus.sh'
# this would make gitstatus.sh faster (default=all)
_ENVS = {
    '__GIT_PROMPT_SHOW_UNTRACKED_FILES': 'normal',
}
_TIMEOUT = 10
_GITCONFIG_FSMONITOR = Path.home() / '.gitconfig_fsmonitor'

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
_PROMPT_EMPTY = '{CYAN}∅'

_JJ_LOG_CMD = ['jj', 'log', '--ignore-working-copy', '--no-graph', '--color', 'never']

def _jj_count_revs(revs):
    return subprocess.check_output(
        [*_JJ_LOG_CMD, '-T', '"x"', '-r', revs],
        text=True,
    ).count('x')

def _jj_specific_prompt():
    ahead_trunk = _jj_count_revs('trunk()..@ ~ empty()')
    behind_trunk = _jj_count_revs('@..trunk()')

    status = subprocess.check_output(
        [*_JJ_LOG_CMD, '-r', '@', '-T', f'''
        separate(
          "",
          "JJ:",
          if(conflict, "{_PROMPT_CONFLICTS}"),
          if(empty, "{_PROMPT_EMPTY}"),
          if(description.len() == 0, "{{YELLOW}}-"),
          if(divergent, "{{RED}}(divergent)"),
          if(hidden, "{{RED}}(hidden)"),
          ""
        )
        '''],
        text=True,
    )
    status += '{RESET}'
    if ahead_trunk > 0:
        status += f'{_PROMPT_AHEAD}{ahead_trunk}'
    if behind_trunk > 0:
        status += f'{_PROMPT_BEHIND}{behind_trunk}'
    return status

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
    if len(gitstatus_result) != 10:
        return 'GIT_ERROR'

    (git_branch, git_remote, git_upstream, git_staged, git_conflicts,
     git_changed, git_untracked, git_stashed, git_clean, is_jj) = gitstatus_result

    if int(is_jj):
        result = _jj_specific_prompt()
        # reuse "git diff" for number of changed files
        # https://github.com/jj-vcs/jj/discussions/7406
        # difference: "untracked" is also considered as changed for jj
        changes = int(git_changed) + int(git_untracked)
        if changes > 0:
            result += _PROMPT_CHANGED + str(changes) + _PROMPT_RESET
        return result

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

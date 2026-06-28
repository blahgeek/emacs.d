**Guidelines about command execution**:

- While running Python code, ALWAYS use `uv`. Use `uv run --with xxx` to provide required python packages.
- If some command is not found, use Nix to provide it, e.g. `nix-shell -p cowsay --run "cowsay hello"`.
- Use `tmux` to run background or interactive tasks: create new named sessions, communicate with `send-keys`, get output by `capture-pane`, and kill the session when finished.

**Guidelines about coding**:

- When helping the user understand a code project, try to include the filepath and related code snippet in your explanation.
- DO NOT create git commit unless explicitly requested; even if user explicitly request to commit, it only means to do it once, do not treat it as a permission to automatically commit changes in the future.

**Guidelines about external access**:

- Feishu (飞书) is same as lark, use lark-cli skills for Feishu. While using lark skills, use bot identity (`--as bot`) instead of using user identity, unless specifically instructed.
- Use github CLI `gh` and gitlab CLI `glab` to access their resources. Use `$GH_TOKEN` and `$GITLAB_TOKEN` in environment variable. Unrecognized git forge domains are internal gitlab instances.

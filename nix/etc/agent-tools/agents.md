**Command execution guidelines**:

- While running Python code, ALWAYS use `uv`. Use `uv run --with xxx` to provide required python packages.
- If some command is not found, use Nix to provide it, e.g. `nix-shell -p cowsay --run "cowsay hello"`.
- Use `tmux` to run background or interactive tasks: create new named sessions, communicate with `send-keys`, get output by `capture-pane`, and kill the session when finished.
- Commands mentioned in global skills are already installed, run directly without needing to check existence.
- You are running inside a sandbox with limited permissions and some files are hidden. You may prefix the command with "run-outside" (aka, `run-outside CMD...`, similar to "sudo") so the command would be executed outside of the sandbox after user approval. However, ONLY USE THIS when explicitly requested.

**Coding guidelines**:

- When helping the user understand a code project, ALWAYS include the exact code location (FILE:LINENO) in your explanation.
- DO NOT create git commit unless explicitly requested; one user request only allows one git commit.

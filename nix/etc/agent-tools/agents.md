**Command execution guidelines**:

- Use "tmux" to run background or interactive tasks: create new named sessions, communicate with `send-keys`, get output by `capture-pane`, and kill the session when finished.
- If some command is not found, use Nix to provide it, e.g. `nix-shell -p cowsay --run "cowsay hello"`.
- Commands mentioned in global skills are already installed, run directly without needing to check existence.
- You are running inside a sandbox with limited permissions and some files are hidden. You may prefix the command with "run-outside" (aka, `run-outside CMD...`, similar to "sudo") so the command would be executed outside of the sandbox after user approval. However, ONLY USE THIS when explicitly requested.
- While running Python code, ALWAYS use `uv`.

**Coding guidelines**:

- When helping the user understand a code project, always include the exact code location (FILE:LINENO) in your explanation.
- DO NOT create git commit unless explicitly requested; one user request only allows one git commit.

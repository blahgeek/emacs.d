
Running commands:

- Use "tmux" to run background or interactive tasks: create new named sessions, communicate with `send-keys`, get output by `capture-pane`, and kill the session when finished.
- If a required command is missing, use Nix to provide it, e.g. `nix-shell -p cowsay --run "cowsay hello"`.
- You are running inside a sandbox with limited permissions and some files are hidden. You may prefix the command with "run-outside" (aka, `run-outside CMD...`, similar to "sudo") so the command would be executed outside of the sandbox after user approval. However, ONLY USE THIS when explicitly requested.

Working with code repo:

- When helping the user understand a code project, always include the exact code location (FILE:LINENO) in your explanation.
- Do not do "git commit" unless explicitly asked.

Misc:

- ALWAYS use "uv run" to run Python code.

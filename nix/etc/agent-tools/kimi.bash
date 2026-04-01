mkdir -p ~/.kimi

# in terminal emacs, it would open w3m by default
# also we do not want to expose /opt/orbstack-guest/bin/open in sandbox
# let's disable browser feature
export BROWSER=true

sandbox_rw_files=("$HOME/.kimi")
sandbox_extra_args=(--yolo)

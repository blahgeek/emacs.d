#!/usr/bin/env bash
# netns-exp.sh — create a network namespace with slirp4netns user-mode networking
#                inside a user namespace (unprivileged, no host impact)
# Usage:
#   netns-exp.sh setup              create netns + slirp4netns + resolv.conf, print state dir and holder PID
#   netns-exp.sh run <PID> CMD...   run a command inside the netns (same as nsenter -t PID -n -m CMD...)
#   netns-exp.sh teardown <PID>     clean up (kill slirp and holder, remove state dir)
set -euo pipefail

find_slirp() {
  if command -v slirp4netns >/dev/null 2>&1; then
    command -v slirp4netns
    return 0
  fi
  echo "ERROR: slirp4netns not found in PATH. Please install it first." >&2
  return 1
}

state_dir_for() { echo "/tmp/netns-exp.$1"; }

cmd_setup() {
  local slirp
  slirp=$(find_slirp)

  # holder process: fresh netns + private mount ns (the latter isolates the resolv.conf bind-mount)
  unshare --net --mount sleep 86400 &
  local pid=$!
  local dir
  dir=$(state_dir_for "$pid")
  mkdir -p "$dir"
  echo "$pid" > "$dir/holder.pid"

  # --configure:    automatically configure tap0 in the netns (10.0.2.100/24, default route via 10.0.2.2)
  # --enable-ipv6:  required. The upstream network is often IPv6-only outbound (DNS returns only AAAA
  #                 for many domains); without it e.g. "curl google.com" fails at connect time.
  "$slirp" --configure --mtu=1500 --enable-ipv6 "$pid" tap0 > "$dir/slirp.log" 2>&1 &
  echo $! > "$dir/slirp.pid"

  # wait for tap0's IPv4 address to be configured
  local i
  for i in $(seq 1 50); do
    if nsenter -t "$pid" -n ip -4 addr show dev tap0 2>/dev/null | grep -q 'inet '; then
      break
    fi
    sleep 0.1
  done
  if ! nsenter -t "$pid" -n ip -4 addr show dev tap0 2>/dev/null | grep -q 'inet '; then
    echo "ERROR: tap0 was not configured successfully, slirp4netns log:" >&2
    cat "$dir/slirp.log" >&2
    exit 1
  fi

  # resolv.conf points to slirp's built-in DNS forwarder 10.0.2.3.
  # Bind-mounted into the holder's mount ns, so the outer /etc/resolv.conf stays untouched.
  printf 'nameserver 10.0.2.3\noptions ndots:0\n' > "$dir/resolv.conf"
  nsenter -t "$pid" -m mount --bind "$dir/resolv.conf" /etc/resolv.conf

  echo "state dir:  $dir"
  echo "holder PID: $pid"
  nsenter -t "$pid" -n ip addr show dev tap0
  nsenter -t "$pid" -n ip route show
  echo
  echo "Run commands inside: $0 run $pid CMD..."
  echo "Clean up when done:    $0 teardown $pid"
}

cmd_run() {
  local pid=$1
  shift
  nsenter -t "$pid" -n -m "$@"
}

cmd_teardown() {
  local pid=$1 dir
  dir=$(state_dir_for "$pid")
  if [[ -f "$dir/slirp.pid" ]]; then
    kill "$(cat "$dir/slirp.pid")" 2>/dev/null || true
  fi
  kill "$pid" 2>/dev/null || true
  rm -rf "$dir"
  echo "torn down netns of PID $pid"
}

case "${1:-}" in
  setup)    cmd_setup ;;
  run)      shift; cmd_run "$@" ;;
  teardown) shift; cmd_teardown "$@" ;;
  *)        sed -n '2,7p' "$0"; exit 1 ;;
esac

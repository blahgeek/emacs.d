#!/bin/bash

set -euo pipefail

NS_ID="$1"
DIR="$(realpath "$2")"

PID=$(lsns -t mnt --json | jq --argjson nsid "$NS_ID" '.namespaces[] | select(.ns == $nsid) | .pid')

# abort if PID is empty
if ! [ -d "/proc/$PID" ]; then
    echo "PID $PID not found"
    exit 1
fi

TMPDIR="$(mktemp -d)"
mount --bind / "$TMPDIR"
RES=0
nsenter --mount=/proc/$PID/ns/mnt -- mount --bind "$TMPDIR/$DIR" "$DIR" || RES=$?
umount "$TMPDIR"
rmdir "$TMPDIR"

exit $RES

#!/bin/bash

REMOTE_DEFAULT=$(git remote show origin | sed -n '/HEAD branch/s/.*: //p')
CURRENT=$(git branch --show-current)

if [[ "$REMOTE_DEFAULT" != "$CURRENT" ]]; then
    echo "$(pwd): Remote default $REMOTE_DEFAULT vs current $CURRENT"
else
    echo "$(pwd): ok"
fi

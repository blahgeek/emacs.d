#!/bin/bash

# used as $LESSOPEN. See env.xsh

# TODO: detect CJK chars in input

echo "~"
echo "~~"
echo "~~~"
echo "~~~~"
echo "~~~~~"
echo "~~~~~~"

exec cat "$1"

#!/bin/bash -ex

git fetch origin

REMOTE_DEFAULT=$(git remote show origin | sed -n '/HEAD branch/s/.*: //p')
git checkout "origin/$REMOTE_DEFAULT"

#!/usr/bin/env bash

set -u
set -ex

cd "$(dirname "$0")"
export INSTALL_DIR="$PWD/dist/"
mkdir -p "$INSTALL_DIR"

mkdir -p build
pushd build
git clone https://github.com/casouri/tree-sitter-module
cd ./tree-sitter-module/

languages=(
    'bash'
    'bison'
    'c'
    'clojure'
    'cmake'
    'cpp'
    'css'
    'dart'
    'dockerfile'
    'doxygen'
    'elisp'
    'glsl'
    'go'
    'gomod'
    'haskell'
    'html'
    'java'
    'javascript'
    'json'
    'lua'
    'make'
    'markdown'
    'nix'
    'org'
    'perl'
    'php'
    'proto'
    'python'
    'ruby'
    'rust'
    'scss'
    'sql'
    'toml'
    'tsx'
    'typescript'
    'typst'
    'wgsl'
    'yaml'
    'zig'
)

printf "%s\n" "${languages[@]}" | xargs -P"${JOBS:-8}" -n1 ./build.sh

popd
rm -rf build

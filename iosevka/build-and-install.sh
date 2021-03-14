#!/bin/bash -ex

cd "$(dirname "$0")"

mkdir -p build
pushd build

if [ ! -d "Iosevka-5.0.6" ]; then
    wget "https://github.com/be5invis/Iosevka/archive/v5.0.6.zip"
    unzip v5.0.6.zip
fi

pushd Iosevka-5.0.6
ln -sf ../../build-plans.toml ./private-build-plans.toml

npm install
npm run build -- ttf::iosevka-blah-mono
npm run build -- ttf::iosevka-blah-proportional

read -p "Build complete, install to ~/.local/share/fonts and call fc-cache? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    cp dist/iosevka-blah-mono/ttf/*.ttf ~/.local/share/fonts/
    cp dist/iosevka-blah-proportional/ttf/*.ttf ~/.local/share/fonts/
    fc-cache -vf
fi

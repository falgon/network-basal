#!/bin/bash -x

export DEBIAN_FRONTEND=noninteractive

APT='apt-get -qq'
CP='cp -rfp'
ENVUSER=vagrant
USERHOME=/home/$ENVUSER

$APT -y update
$APT -y -f --no-show-upgraded purge haskell-stack
$APT -y -f --no-show-upgraded --autoremove install build-essential g++-7 gcc-7 libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg zsh vim vim-gnome hlint arping arp-scan libc6-dev-i386 libpcap-dev > /dev/null
which stack > /dev/null
RS=$?
if [ $RS = 1 ]; then
    curl -sSL https://get.haskellstack.org/ | sh > /dev/null 2>&1
    echo "export PATH=~/.local/bin:$PATH" >> ~/.zshrc
fi
${APT} clean

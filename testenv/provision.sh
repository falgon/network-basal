#!/bin/bash -x

export DEBIAN_FRONTEND=noninteractive

APT='apt-get -qq'
CP='cp -rfp'
ENVUSER=vagrant
USERHOME=/home/$ENVUSER

$APT -y -f --no-show-upgraded purge haskell-stack
$APT -y -f --no-show-upgraded --autoremove install build-essential g++-7 gcc-7 libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg zsh vim vim-gnome hlint arping arp-scan libc6-dev-i386 libpcap-dev > /dev/null
which stack > /dev/null
RS=$?
if [ $RS = 1 ]; then
    curl -sSL https://get.haskellstack.org/ | sh > /dev/null 2>&1
    echo "export PATH=~/.local/bin:$PATH" >> ~/.zshrc
fi
${APT} clean
if [ ! -e "SetUp" ]; then
    git clone https://github.com/falgon/SetUp.git > /dev/null # my config
fi

# shell
$CP SetUp/.zshrc $USERHOME
$CP SetUp/.zshrc.antigen $USERHOME

if [ ! -e "antigen" ]; then
    git clone https://github.com/zsh-users/antigen.git > /dev/null
fi

$CP -r antigen $USERHOME/.antigen
chsh -s `which zsh` $ENVUSER

# editor
mkdir -p $USERHOME/.vim/bundle

if [ ! -e "neobundle.vim" ]; then
    git clone https://github.com/Shougo/neobundle.vim > /dev/null
fi
$CP neobundle.vim $USERHOME/.vim/bundle/
$CP SetUp/.vimrc $USERHOME
$CP SetUp/cpp_vimrc $USERHOME/.vim
chown -hR $ENVUSER:$ENVUSER $USERHOME/.vim $USERHOME/.vimrc $USERHOME/.antigen $USERHOME/.zshrc $USERHOME/.zshrc.antigen

# clean
rm -rf SetUp antigen neobundle.vim

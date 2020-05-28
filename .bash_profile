# sort files recursively by size biggest to smallest
# find ./ -type f -name '*.o' -printf '%s\t\t%p\n' | sort -rn

# sort files recursively by size biggest to smallest
# including accumulated size of directories
# du -ah ./ | sort -rh

# see list of symbols within an lib-, exe-, object-file(s)
# nm -CglS --size-sort --defined-only <libs,exes,objs,...>

# Normal Colors
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# Bold
BBlack='\e[1;30m'       # Black
BRed='\e[1;31m'         # Red
BGreen='\e[1;32m'       # Green
BYellow='\e[1;33m'      # Yellow
BBlue='\e[1;34m'        # Blue
BPurple='\e[1;35m'      # Purple
BCyan='\e[1;36m'        # Cyan
BWhite='\e[1;37m'       # White

# Background
On_Black='\e[40m'       # Black
On_Red='\e[41m'         # Red
On_Green='\e[42m'       # Green
On_Yellow='\e[43m'      # Yellow
On_Blue='\e[44m'        # Blue
On_Purple='\e[45m'      # Purple
On_Cyan='\e[46m'        # Cyan
On_White='\e[47m'       # White

NC="\e[m"               # Color Reset

# prompt
PS1="${Green}\w${NC}\n> "

# history handling
#
# erase dublicates
export HISTCONTROL=erasedups
# resize history size
export HISTSIZE=100
# append to bash_history if terminal quits
shopt -s histappend

# add b2 for executing boost.build scripts
if [ -d "$HOME/projects/programming/boost_build" ] ; then
    PATH="$HOME/projects/programming/boost_build:$PATH"
fi

# path to the boost-build.jam for finding the jam-code that implements the build system
export BOOST_BUILD_PATH="$HOME/projects/programming/boost_build"

export BOOST_ROOT="$HOME/projects/programming/boost"

# pip install executeables here, so add it to the path var
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# eerie is io's package manager
# Make sure to update your shell's environment variables before using Eerie.
# Here's a sample code you could use:
# Eerie config
if [ -d "$HOME/.eerie" ] ; then
    EERIEDIR="$HOME/.eerie"
    PATH=$PATH:$EERIEDIR/base/bin:$EERIEDIR/activeEnv/bin
    export EERIEDIR PATH
fi

# makes vpkg command complation available for bash
source ~/projects/programming/vcpkg/scripts/vcpkg_completion.bash

# sets environment such that the nix-packagemanager can work
source ~/.nix-profile/etc/profile.d/nix.sh

# make rust-tools like cargo, rustup, rustc available
export PATH="$HOME/.cargo/bin:$PATH"

# make swift tooling available
# this takes precedance over my installed tools
# like all the clang-family tools
# and I do not think that I want that
# export PATH="$HOME/projects/programming/swift/usr/bin:$PATH"

# all my bash aliases are here
if [ -f ${HOME}/.bash_aliases ]; then
    . ${HOME}/.bash_aliases
fi

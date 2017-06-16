
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

# dir listing
alias ll="ls -laFG"

# wifi networking
alias wifi_off="networksetup -setairportpower en0 off"
alias wifi_on="networksetup -setairportpower en0 on"
alias wifi_list="/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport scan"
wifi_join() {
    networksetup -setairportnetwork en0 $1 $2
}
alias wifi_name="networksetup -listallhardwareports"

# emacs
alias emacs_gui="nohup /Applications/Emacs.app/Contens/MacOs/Emacs"

# history handling
#
# erase dublicates
export HISTCONTROL=erasedups
# resize history size
export HISTSIZE=100
# append to bash_history if terminal quits
shopt -s histappend

# add rtags to path
export PATH="$PATH:/Users/Tobi/projects/programming/rtags/build/bin"

# add b2 for executing boost.build scripts
export PATH="$PATH:/Users/Tobi/projects/programming/c++/boost"
# directory where boost.build resides
export BOOST_BUILD_PATH="/Users/Tobi/projects/programming/c++/boost"

# set graphviz dot executeable var
export GRAPHVIZ_DOT="/opt/local/bin/dot"

# MacPorts Installer addition on 2014-12-20_at_17:09:32: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# Ensure that user .bash_profiles point back to .bashrc
if [ -f ${HOME}/.bashrc ]; then
    . ${HOME}/.bashrc
fi

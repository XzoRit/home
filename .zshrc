# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="steef"
ZSH_THEME="random"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
ZSH_THEME_RANDOM_CANDIDATES=( "amuse" "aussiegeek" "avit" "bira" "bureau" "candy" "crcandy" "dst" "dstufft" "duellj" "fino-time" "fino" "fox" "frisk" "frontcube" "funky" "gnzh" "imajes" "intheloop" "itchy" "jispwoso" "josh" "jtriley" "juanghurtado" "junkfood" "kphoen" "linuxonly" "nicoulaj" "peepcode" "re5et" "refined" "rixius" "simonoff" "Soliah" "sporty_256" "steeef" "strug" "suvash" "takashiyoshida" "terminalparty" "tjkirch" "trapd00r" "xiong-chiamiov" "ys"  )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# https://github.com/ohmyzsh/ohmyzsh/wiki/Plugins
plugins=(
    alias-finder # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/alias-finder
    aliases  # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/aliases
    docker # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/docker
    emacs # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/emacs
    emoji # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/emoji
    eza # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/eza
    fzf # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/fzf
    jira # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/jira
    python # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/python
    snap # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/snap
    ssh # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/ssh
    sudo # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/sudo
    themes # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/themes
    wd # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/wd
    web-search # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/web-search
    z # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/z
    zsh-interactive-cd # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/zsh-interactive-cd
    zsh-navigation-tools # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/zsh-navigation-tools
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacsclient'
else
  export EDITOR='emacs'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# alias-finder
zstyle ':omz:plugins:alias-finder' cheaper yes

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias ..="cd .."
alias ...="cd ../.."

# print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# eza: ls replacement alias
alias el="eza -alBhH --time-style=long-iso --icons=auto --no-user"
alias er="eza -alBhHT --time-style=long-iso --icons=auto --no-user"
alias ell="eza -alBhH --time-style=long-iso --icons=auto --no-user --total-size --git --git-repos"
alias erl="eza -alBhHT --time-style=long-iso --icons=auto --no-user --total-size --git --git-repos"

# bat: cat replacement alias
alias bt="bat -P --theme=GitHub"

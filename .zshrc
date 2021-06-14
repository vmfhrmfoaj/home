if [ -r /etc/os-release ]; then
  # for Arch Linux
  if [ $(grep -c 'Arch Linux' /etc/os-release) -gt 0 ]; then
    # NOTE
    #  you may need run following command: 'sudo pkgfile --update'
    if [ -r /usr/share/doc/pkgfile/command-not-found.zsh ]; then
      source /usr/share/doc/pkgfile/command-not-found.zsh
    fi
  fi

  # for Gento Linux
  if [ $(grep -c 'gentoo' /etc/os-release) -gt 0 ]; then
    if [ -r /etc/profile ]; then
      source /etc/profile
    fi
    autoload -U compinit promptinit
    compinit
    promptinit; prompt gentoo
  fi
fi

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
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


if [ ! -e "${ZSH_CUSTOM:-${HOME}/.oh-my-zsh/custom}/plugins/zsh-bash-completions-fallback" ]; then
    local cur_dir=`pwd`
    git clone https://github.com/3v1n0/zsh-bash-completions-fallback.git "${ZSH_CUSTOM:-${HOME}/.oh-my-zsh/custom}/plugins/zsh-bash-completions-fallback"
    cd "${ZSH_CUSTOM:-${HOME}/.oh-my-zsh/custom}/plugins/zsh-bash-completions-fallback"
    git reset --hard 274d42b54e6153b72ee1a159b4721afef7fe93ca
    cd "$cur_dir"
fi

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(ansible docker docker-compose git lein vi-mode zsh-bash-completions-fallback)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

[ -r "${HOME}"/.zsh_untracked_profile ] && source "${HOME}"/.zsh_untracked_profile
[ -r "${HOME}"/.script/setup ] && source "${HOME}"/.script/setup
[ -r "${HOME}"/.zsh_profile ] && source "${HOME}"/.zsh_profile

true # exit 0

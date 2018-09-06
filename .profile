function setEnv () {
  local os=`uname`

  # lang
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8

  # ~/.bin
  if [ -d $HOME/.bin ]; then
    export PATH=$HOME/.bin:$PATH
  fi

  # oh my zsh
  if [ ! -z $ZSH ]; then
    if [ ! -z "$(echo $plugins | grep 'vi-mode')" ]; then
      bindkey -M vicmd '^r' history-incremental-search-backward
      bindkey -M vicmd '^a' beginning-of-line
      bindkey -M vicmd '^e' end-of-line
      bindkey -M vicmd '^b' backward-char
      bindkey -M vicmd '^f' forward-char
      bindkey '^b' backward-char
      bindkey '^f' forward-char
      bindkey '^d' delete-char
    fi
  fi

  if [ ! -z $SCHROOT_CHROOT_NAME ]; then
    local NEWLINE=$'\n'
    local PS1_PREFIX='%{$fg[magenta]%}($SCHROOT_CHROOT_NAME)%{$reset_color%}'
    export PS1="${PS1_PREFIX}${NEWLINE}$PS1"
  fi
}

setEnv

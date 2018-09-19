function setEnv () {
  local os=`uname`

  # locale
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

  # elixir
  if [ 'elixir' = "$SCHROOT_CHROOT_NAME" ]; then
    export HEX_HTTP_CONCURRENCY=1
    export HEX_HTTP_TIMEOUT=30
  fi

  # schroot
  if [ ! -z $SCHROOT_CHROOT_NAME ]; then
    local NEWLINE=$'\n'
    local PS1_PREFIX='%{$fg[magenta]%}($SCHROOT_CHROOT_NAME)%{$reset_color%}'
    export PS1="${PS1_PREFIX}${NEWLINE}$PS1"
  fi

  # google cloud
  local GOOGLE_COULD="$HOME/.local/google-cloud-sdk"
  if [  -f "$GOOGLE_COULD/path.zsh.inc" ]; then
    source "$GOOGLE_COULD/path.zsh.inc"
  fi
  if [  -f "$GOOGLE_COULD/completion.zsh.inc" ]; then
    source "$GOOGLE_COULD/completion.zsh.inc"
  fi
}

setEnv

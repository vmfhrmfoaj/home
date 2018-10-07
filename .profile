function setEnv () {
  local os=`uname`

  # locale
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8

  # ~/.bin
  if [ -d $HOME/.bin ]; then
    export PATH=$HOME/.bin:$PATH
  fi

  # Oh My Zsh
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

  # Elixir
  if [ 'elixir' = "$SCHROOT_CHROOT_NAME" ]; then
    export HEX_HTTP_CONCURRENCY=1
    export HEX_HTTP_TIMEOUT=30
  fi

  # Clojure
  if [ 'clojure' = "$SCHROOT_CHROOT_NAME" ]; then
    local mem_in_gb=$(($(cat /proc/meminfo | grep MemTotal | grep -o "[0-9]\+") / 1024 / 1024))
    if   [ "$mem_in_gb" -gt 16 ]; then
      export JVM_OPTS="-Xms2g -Xmx8g"
    elif [ "$mem_in_gb" -gt  8 ]; then
      export JVM_OPTS="-Xms1g -Xmx4g"
    elif [ "$mem_in_gb" -gt  4 ]; then
      export JVM_OPTS="-Xms512m -Xmx2g"
    fi
  fi

  # Schroot
  if [ ! -z $SCHROOT_CHROOT_NAME ]; then
    local NEWLINE=$'\n'
    local PS1_PREFIX='%{$fg[magenta]%}($SCHROOT_CHROOT_NAME)%{$reset_color%}'
    export PS1="${PS1_PREFIX}${NEWLINE}$PS1"
  fi

  # Google Cloud Platform
  local GOOGLE_COULD="$HOME/.local/google-cloud-sdk"
  if [  -f "$GOOGLE_COULD/path.zsh.inc" ]; then
    source "$GOOGLE_COULD/path.zsh.inc"
  fi
  if [  -f "$GOOGLE_COULD/completion.zsh.inc" ]; then
    source "$GOOGLE_COULD/completion.zsh.inc"
  fi

  # Heroku
  local HEROKU="$HOME/.local/heroku"
  if [ -d "$HEROKU/bin/" ]; then
    export PATH="$HEROKU/bin/":$PATH
  fi
}

zle-keymap-select () {
  local CURSOR_BOX='\e[2 q'
  local CURSOR_UNDERBAR='\e[4 q'
  local CURSOR_LINE='\e[6 q'
  if [ "$TERM" = "xterm-256color" ]; then
    if [ "$KEYMAP" = 'vicmd' ]; then
      # the command mode for vi
      echo -ne $CURSOR_BOX
    else
      # the insert mode for vi
      echo -ne $CURSOR_LINE
    fi
  fi
}

zle-keymap-select
setEnv

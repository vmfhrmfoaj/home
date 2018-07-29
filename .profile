function setEnv () {
  local os=`uname`
  if [ ! -z $(echo $TERM | grep -o "^eterm") ]; then
    EMACS_INIT=1
  fi

  # cask
  if [ -d $HOME/.cask/bin ]; then
    export PATH=$HOME/.cask/bin:$PATH
  fi

  # react
  export REACT_EDITOR=emacsclient

  # node
  if [ -d /usr/local/lib/node_modules ]; then
    export NODE_PATH=/usr/local/lib/node_modules
  fi

  # perl
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8
  export PERLBREW_ROOT=$HOME/.perlbrew
  if [ -z $EMACS_INIT ] && [ -e $PERLBREW_ROOT/etc/bashrc ]; then
    source $PERLBREW_ROOT/etc/bashrc
  fi

  # clojure
  local java_home=$HOME/.openjdk/current
  if [ -d $java_home ]; then
    export PATH=$java_home/bin:$PATH
    export JAVA_HOME=$java_home
  fi
  local mem_in_gb=0
  if   [ "$os" = "Darwin" ]; then
    mem_in_gb=$(($(sysctl -n hw.memsize) / 1024 / 1024 / 1024))
  elif [ "$os" = "Linux" ]; then
    mem_in_gb=$(($(cat /proc/meminfo | grep MemTotal | grep -o "[0-9]\+") / 1024 / 1024))
  fi
  if   [ $mem_in_gb -gt 16 ]; then
    export JVM_OPTS="-Xms2g -Xmx8g"
  elif [ $mem_in_gb -gt  8 ]; then
    export JVM_OPTS="-Xms1g -Xmx4g"
  elif [ $mem_in_gb -gt  4 ]; then
    export JVM_OPTS="-Xms512m -Xmx2g"
  else
    export JVM_OPTS="-Xms128m -Xmx1g"
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

  # emacs tramp
  # - https://wxchen.wordpress.com/2012/05/20/getting-tramp-in-emacs-to-work-with-zsh-as-default-shell/
  if [ ! -z $ZSH ] && [ "$TERM" = "dumb" ]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
  fi
}
setEnv

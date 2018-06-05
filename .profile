function setEnv () {
  local os=`uname`

  # brew
  if [ -z $EMACS_INIT ] && [ "$os" = "Darwin" ]; then
    export PATH=$(brew --prefix)/sbin:$PATH
  fi

  # android
  if [ -d $HOME/Android_SDK ]; then
    export ANDROID_SDK=$HOME/Android_SDK
    export ANDROID_HOME=$ANDROID_SDK
    export PATH=$ANDROID_SDK/tools:$ANDROID_SDK/tools/bin:$PATH
    export PATH=$ANDROID_SDK/platform-tools:$PATH
    if [ "$os" = "Linux" ]; then
      function emulator {
        local cur_dir=$PWD;
        cd $ANDROID_SDK/tools && ./emulator $@;
        cd $cur_dir
      }
    fi
  fi

  # cocoapod
  if [ -d $HOME/.gem/ruby/2.0.0 ]; then
    export GEM_HOME=$HOME/.gem/ruby/2.0.0
    export PATH=$GEM_HOME/bin:$PATH
  fi

  # cask
  if [ -d $HOME/.cask/bin ]; then
    export PATH=$HOME/.cask/bin:$PATH
  fi

  # latex
  if [ "$os" = "Darwin" ] && [ -d /Library/TeX/texbin ]; then
    export PATH=/Library/TeX/texbin:$PATH
    export GS_FONTPATH=/System/Library/Fonts/:/Library/Fonts/:$HOME/Library/Fonts/
  fi

  # texinfo
  if [ -d /usr/local/opt/texinfo/bin ]; then
    export PATH=/usr/local/opt/texinfo/bin:$PATH
  fi

  # react
  export REACT_EDITOR=emacsclient

  # node
  if [ -d /usr/local/lib/node_modules ]; then
    export NODE_PATH=/usr/local/lib/node_modules
  fi

  # go lang
  if [ -d $HOME/.go_lang ]; then
    export GOROOT=$HOME/.go_lang
    export PATH=$PATH:$GOROOT/bin
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
  fi

  # clojurescript
  if [ -z $EMACS_INIT ]; then
    if [ -x "$(which d8 2> /dev/null)" ]; then
      export V8_HOME=$(which d8 | xargs dirname)
    fi
    if [ -x "$(which js 2> /dev/null)" ]; then
      export SPIDERMONKEY_HOME=$(which js | xargs dirname)
    fi
    if [ -x "$(which jjs 2> /dev/null)" ]; then
      export NASHORN_HOME=$(which jjs | xargs dirname)
    fi
  fi

  # oh my zsh
  if [ -z $EMACS_INIT ]; then
    if [ ! -z $(echo $SHELL | grep 'zsh$') ]; then
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
    if [ "$SHELL" = "/bin/zsh" ] && [ "$TERM" = "dumb" ]; then
      unsetopt zle
      unsetopt prompt_cr
      unsetopt prompt_subst
      unfunction precmd
      unfunction preexec
      PS1='$ '
    fi
  fi
}
setEnv

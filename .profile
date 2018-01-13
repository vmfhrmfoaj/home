function setEnv () {
  local os=`uname`

  # android
  export ANDROID_SDK=$HOME/Android_SDK
  export ANDROID_HOME=$ANDROID_SDK
  export PATH=$ANDROID_SDK/tools:$ANDROID_SDK/tools/bin:$PATH
  export PATH=$ANDROID_SDK/platform-tools:$PATH
  if [[ "$os" == "Linux" ]]; then
    function emulator {
      local cur_dir=$PWD;
      cd $ANDROID_SDK/tools && ./emulator $@;
      cd $cur_dir
    }
  fi

  # cocoapod
  export GEM_HOME=$HOME/.gem/ruby/2.0.0
  export PATH=$GEM_HOME/bin:$PATH

  # cask
  export PATH=$HOME/.cask/bin:$PATH

  # latex
  export PATH=/Library/TeX/texbin:$PATH
  export GS_FONTPATH=/System/Library/Fonts/:/Library/Fonts/:$HOME/Library/Fonts/

  # texinfo
  export PATH=/usr/local/opt/texinfo/bin:$PATH

  # react
  export REACT_EDITOR=emacsclient

  # java
  local java_dir=$(java -XshowSettings:properties -version 2>&1 | grep "java.home" | cut -d'=' -f2)
  if [[ "$os" == "Linux" ]]; then
    java_dir=$(dirname $java_dir)
  fi
  export PATH=$java_dir/bin:$PATH
  local mem_in_gb=0
  if   [[ "$os" == "Darwin" ]]; then
    mem_in_gb=$(($(sysctl -n hw.memsize) / 1024 / 1024 / 1024))
  elif [[ "$os" == "Linux" ]]; then
    mem_in_gb=$(($(cat /proc/meminfo | grep MemTotal | grep -o "[0-9]\+") / 1024 / 1024))
  fi
  local jvm_opts="--add-opens java.base/java.io=ALL-UNNAMED"
  if   [[ $mem_in_gb -gt 16 ]]; then
    export JVM_OPTS="$jvm_opts -Xms2g -Xmx8g"
  elif [[ $mem_in_gb -gt  8 ]]; then
    export JVM_OPTS="$jvm_opts -Xms1g -Xmx4g"
  elif [[ $mem_in_gb -gt  4 ]]; then
    export JVM_OPTS="$jvm_opts -Xms512m -Xmx2g"
  fi

  # node
  export NODE_PATH=/usr/local/lib/node_modules

  # perl
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8
  export PERLBREW_ROOT=$HOME/.perlbrew
  [ -e $PERLBREW_ROOT/etc/bashrc ] && source $PERLBREW_ROOT/etc/bashrc

  # mpv
  alias mplay="mpv --sub-codepage=utf8:cp949 --cache=8192 --hwdec=videotoolbox --vo=opengl --volume=70"
  alias aplay="mplay --no-video"
  alias vplay="mplay --no-audio"
  alias ylplay="aplay --ytdl-format 95"

  # emacs tramp
  # - https://wxchen.wordpress.com/2012/05/20/getting-tramp-in-emacs-to-work-with-zsh-as-default-shell/
  if [[ "$SHELL" == "/bin/zsh" && "$TERM" == "dumb" ]]
  then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PS1='$ '
  fi

  # gtag
  # - https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btags/gtags#install
  export GTAGSLABEL=pygments

  # go lang
  export GOROOT=$HOME/.go_lang
  export PATH=$PATH:$GOROOT/bin

  # groovy
  if   [[ "$os" == "Darwin" ]]; then
    export GROOVY_HOME=/usr/local/opt/groovy/libexec
  elif [[ "$os" == "Linux" ]]; then
  fi

  # oh my zsh
  if [[ ! -z $(echo $SHELL | grep "zsh") ]]; then
    if [[ ! -z $(echo $plugins | grep "vi-mode") ]]; then
      bindkey -M vicmd '^r' history-incremental-search-backward
    fi
  fi
}
setEnv

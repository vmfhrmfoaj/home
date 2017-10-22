# android
export ANDROID_SDK=$HOME/Android_SDK
export ANDROID_HOME=$ANDROID_SDK
export PATH=$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools:$PATH

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

# perlbrew
export PERLBREW_ROOT=$HOME/.perlbrew
[ -e $PERLBREW_ROOT/etc/bashrc ] && source $PERLBREW_ROOT/etc/bashrc

# react
export REACT_EDITOR=emacsclient

# lein
export JVM_OPTS="$(if [ `uname -a | grep -o iMac` ]; then echo '-Xms2g -Xmx4g'; fi)"

# node
export NODE_PATH=/usr/local/lib/node_modules

# perl
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

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

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
source $PERLBREW_ROOT/etc/bashrc

# react
export REACT_EDITOR=emacsclient

# lein
export JVM_OPTS="$(if [ `uname -a | grep -o iMac` ]; then echo '-Xms2g -Xmx4g'; fi)"

# node
export NODE_PATH=/usr/local/lib/node_modules

# android
export ANDROID_SDK_ROOT=$HOME/Android_SDK
export PATH=$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/platform-tools:$PATH

# cocoapod
export GEM_HOME=$HOME/.gem/ruby/2.0.0
export PATH=$GEM_HOME/bin:$PATH

# cask
export PATH=$HOME/.cask/bin:$PATH

# latex
export PATH=/Library/TeX/texbin:$PATH
export GS_FONTPATH=/System/Library/Fonts/:/Library/Fonts/:$HOME/Library/Fonts/

# perlbrew
export PERLBREW_ROOT=$HOME/.perlbrew
source $PERLBREW_ROOT/etc/bashrc

# lein
export JVM_OPTS="$(if [ `uname -a | grep -o iMac` ]; then echo '-Xms1g -Xmx2g'; fi)"

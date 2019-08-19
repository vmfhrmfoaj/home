#!/bin/bash

_add_to_path() {
  local new_path=$1
  if [ ! -z ${new_path} ]; then
    if [ -d "${new_path}" ] && [ 0 = $(echo ${PATH} | grep -c "${new_path}") ]; then
      export PATH="${new_path}:${PATH}"
    fi
  fi
}

_setup_for_android() {
  local android_home="${HOME}/.android/sdk"
  if [ -d ${android_home} ]; then
    export ANDROID_HOME="${android_home}"
    _add_to_path "${android_home}/tools/bin"
    _add_to_path "${android_home}/platform-tools"
  fi
}

_setup_for_asdf() {
  local asdf_home="${HOME}/.asdf"
  if [ ! -d "${asdf_home}" ]; then
    git clone 'https://github.com/asdf-vm/asdf.git' "${HOME}/.asdf" --branch v0.7.4
  fi
  [ -f "${asdf_home}/asdf.sh" ]               && source "${asdf_home}/asdf.sh"
  [ -f "${asdf_home}/completions/asdf.bash" ] && source "${asdf_home}/completions/asdf.bash"
}

_setup_for_clojure() {
  which lein > /dev/null 2>&1
  if [ $? -ne 0 ]; then
    local lein_path="${HOME}/.bin/lein"
    mkdir -p $(dirname ${lein_path})
    wget -O ${lein_path} 'https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein'
    chmod +x ${lein_path}
  fi
}

_setup_for_java() {
  local java_home=$(asdf where java 2> /dev/null)
  if [ ! -z ${java_home} ]; then
    export JAVA_HOME=${java_home}
  fi
}

_setup_for_locale() {
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8
}

_setup_for_perlbrew() {
  export PERLBREW_ROOT="${HOME}/.perlbrew"
  if [ ! -d ${PERLBREW_ROOT} ]; then
    curl -fsSL 'https://install.perlbrew.pl' | bash
  fi
  [ -f "${PERLBREW_ROOT}/etc/bashrc" ] && source "${PERLBREW_ROOT}/etc/bashrc"
}

_setup_for_rust() {
  if [ ! -d "${HOME}/.cargo" ]; then
    # see, https://github.com/rust-lang/rustup.rs/issues/953#issuecomment-318650338
    curl 'https://sh.rustup.rs' -sSf | RUSTUP_INIT_SKIP_PATH_CHECK=yes sh -s -- --no-modify-path --default-toolchain nightly
    if [ -f "${HOME}/.cargo/env" ]; then
      source "${HOME}/.cargo/env"
      rustup toolchain add stable
      rustup default stable
      rustup toolchain add nightly
      rustup component add rls rust-analysis rust-src # for rls (Rust Language Server)
    fi
  fi
  [ -f "${HOME}/.cargo/env" ] && source "${HOME}/.cargo/env"
}

_setup_for_schroot() {
  if [ -z ${DISPLAY} ]; then
    # See, https://lukeplant.me.uk/blog/posts/chroot-with-x-applications/
    export DISPLAY=:0.0
  fi
}

_is_ssh_agent_running() {
  if [ ! -z ${SSH_AGENT_PID} ] && [ 0 != $(ps -o cmd= -q ${SSH_AGENT_PID} | grep -c 'ssh-agent') ] && \
       [ ! -z ${SSH_AUTH_SOCK} ] && [ -S ${SSH_AUTH_SOCK} ]; then
    echo 'OK'
    return 0
  else
    echo 'NO'
    return 255
  fi
}

_setup_for_ssh() {
  local cache_file=${1:-"${HOME}/.ssh/ssh-agent-for-remote"}
  if [ 'OK' = "$(_is_ssh_agent_running)" ]; then
    return 0
  fi
  if [ -f "${cache_file}" ]; then
    source "${cache_file}"
    if [ 'OK' = "$(_is_ssh_agent_running)" ]; then
      return 0
    fi
  fi
  mkdir -p $(dirname "${cache_file}")
  ssh-agent -s > "${cache_file}"
  source "${cache_file}"
}

_setup_for_wsl() {
  if [ -z ${DISPLAY} ]; then
    export DISPLAY=localhost:0.0
  fi
  which xrdb > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    xrdb -merge "${HOME}/.Xresources"
  fi
}

_setup_for_xorg() {
  which numlockx > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    numlockx on
  fi
}


# add dirs to PATH
_add_to_path '/sbin'
_add_to_path '/usr/sbin'
_add_to_path "${HOME}/.bin"

# basic setup
_setup_for_android
_setup_for_asdf
_setup_for_clojure
_setup_for_locale
_setup_for_perlbrew
_setup_for_rust
_setup_for_xorg

# depend on `asdf`
_setup_for_java

# for only terminal environment
if [ -t 1 ] && [ ! -z ${SHELL} ]; then
  _setup_for_ssh
fi

if [ 'yes' = "${SCHROOT_CHROOT_NAME+yes}" ]; then
  _setup_for_schroot
fi

if [ 'yes' = "${WSLENV+yes}" ]; then
  _setup_for_wsl
fi

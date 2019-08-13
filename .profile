#!/bin/zsh

_update_java_home() {
  local java_home=$(asdf where java 2> /dev/null)
  if [ ! -z ${java_home} ]; then
    export JAVA_HOME=${java_home}
  fi
}

_setup() {
  # locale
  export LANG=en_US.UTF-8
  export LC_ALL=en_US.UTF-8

  # Xorg
  which numlockx > /dev/null 2>&1
  if [ $? -ne 0 ]; then
    /usr/bin/numlockx on
  fi

  # PATH
  if [ 0 = $(echo ${PATH} | grep -c '/sbin') ]; then
    export PATH="/sbin:/usr/sbin:${PATH}"
  fi
  if [ -d "${HOME}/.bin" ] && [ 0 = $(echo ${PATH} | grep -c "${HOME}/.bin") ]; then
    export PATH="${HOME}/.bin:${PATH}"
  fi

  # .asdf
  local asdf_home="${HOME}/.asdf"
  if [ ! -d "${asdf_home}" ]; then
    git clone 'https://github.com/asdf-vm/asdf.git' "${HOME}/.asdf" --branch v0.6.2
  fi
  source "${asdf_home}/asdf.sh"
  source "${asdf_home}/completions/asdf.bash"

  # Java
  #_update_java_home

  # Android
  local android_home="${HOME}/.android/sdk"
  if [ -d ${android_home} ]; then
    export ANDROID_HOME="${android_home}"
    if [ -d "${android_home}/tools/bin" ]      && [ 0 = $(echo ${PATH} | grep -c "${android_home}/tools/bin") ]; then
      export PATH="${android_home}/tools/bin:${PATH}"
    fi
    if [ -d "${android_home}/platform-tools" ] && [ 0 = $(echo ${PATH} | grep -c "${android_home}/platform-tools") ]; then
      export PATH="${android_home}/platform-tools:${PATH}"
    fi
  fi

  # Perlbrew
  export PERLBREW_ROOT="${HOME}/.perlbrew"
  if [ ! -d ${PERLBREW_ROOT} ]; then
    curl -fsSL 'https://install.perlbrew.pl' | bash
  fi
  source "${PERLBREW_ROOT}/etc/bashrc"

  # Clojure
  which lein > /dev/null 2>&1
  if [ $? -ne 0 ]; then
    local lein_path="${HOME}/.bin/lein"
    mkdir -p $(dirname ${lein_path})
    wget -O ${lein_path} 'https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein'
    chmod +x ${lein_path}
  fi

  # Rust lang
  if [ ! -d "${HOME}/.cargo" ]; then
    # see, https://github.com/rust-lang/rustup.rs/issues/953#issuecomment-318650338
    curl 'https://sh.rustup.rs' -sSf | RUSTUP_INIT_SKIP_PATH_CHECK=yes sh -s -- --no-modify-path --default-toolchain nightly
    source "${HOME}/.cargo/env"
    rustup toolchain add stable
    rustup default stable
    rustup toolchain add nightly
    rustup component add rls rust-analysis rust-src # for rls (Rust Language Server)
  fi
  if [ -f "${HOME}/.cargo/env" ]; then
    source "${HOME}/.cargo/env"
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

_setup
if [ -t 1 ] && [ ! -z ${SHELL} ]; then
  _setup_for_ssh
  if [ ! -z ${ZSH} ] && [ -f "${HOME}/.zsh_profile" ]; then
    source "${HOME}/.zsh_profile"
  fi
fi

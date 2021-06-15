FROM centos:7

ENV USER_ID=1000
ENV GROUP_ID=1000

RUN yum -y update && \
    yum -y group install "Development Tools" && \
    yum -y install epel-release sudo vim && \
    yum -y install bash-completion-extras
RUN groupadd -g ${GROUP_ID} user && \
    useradd -u ${USER_ID} -g ${GROUP_ID} user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user && \
    chmod 440 /etc/sudoers.d/user && \
    chsh -s /bin/bash user

USER user
WORKDIR /workspace
CMD [ "bash" ]

FROM ubuntu:20.04

ENV USER_ID=1000
ENV GROUP_ID=1000

RUN apt -y update && \
    apt -y install build-essential sudo vim bash-completion
RUN groupadd -g ${GROUP_ID} user && \
    useradd -u ${USER_ID} -g ${GROUP_ID} user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user && \
    chmod 440 /etc/sudoers.d/user && \
    chsh -s /bin/bash user

USER user
WORKDIR /workspace
CMD [ "bash" ]

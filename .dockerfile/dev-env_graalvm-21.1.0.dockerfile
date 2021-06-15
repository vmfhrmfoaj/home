FROM ghcr.io/graalvm/graalvm-ce:21.1.0

ENV USER_ID=1000
ENV GROUP_ID=1000

RUN curl -L https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o /usr/bin/lein && \
    chmod +x /usr/bin/lein && \
    lein help
RUN microdnf install sudo util-linux-user && \
    groupadd -g ${GROUP_ID} user && \
    useradd -u ${USER_ID} -g ${GROUP_ID} user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user && \
    chmod 440 /etc/sudoers.d/user && \
    chsh -s /bin/bash user

USER user
WORKDIR /workspace
CMD [ "bash" ]

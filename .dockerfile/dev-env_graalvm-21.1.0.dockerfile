FROM ghcr.io/graalvm/graalvm-ce:21.1.0

ENV USER_ID=1000
ENV GROUP_ID=1000

RUN microdnf install sudo util-linux-user && \
    groupadd -g ${GROUP_ID} user && \
    useradd -u ${USER_ID} -g ${GROUP_ID} user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user && \
    chmod 440 /etc/sudoers.d/user && \
    chsh -s /bin/bash user
RUN curl -L https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -o /usr/bin/lein && \
    chmod +x /usr/bin/lein && \
    lein help
RUN curl -L https://mirror.navercorp.com/apache/maven/maven-3/3.8.1/binaries/apache-maven-3.8.1-bin.tar.gz -o apache-maven.tar.gz && \
    tar xzvf apache-maven.tar.gz -C /opt && \
    rm -f apache-maven.tar.gz && \
    ln -s /opt/apache-maven-*/bin/mvn /usr/bin/mvn
RUN curl -L https://mirror.navercorp.com/apache/ant/binaries/apache-ant-1.10.10-bin.tar.gz -o apache-ant.tar.gz && \
    tar xzvf apache-ant.tar.gz -C /opt && \
    rm -f apache-ant.tar.gz && \
    ln -s /opt/apache-ant-*/bin/ant /usr/bin/ant

USER user
WORKDIR /workspace
CMD [ "bash" ]

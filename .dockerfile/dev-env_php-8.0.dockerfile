FROM php:8.0-fpm

ENV USER_ID=1000
ENV GROUP_ID=1000

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer
RUN apt -y update && \
    apt -y install bash-completion sudo zip libzip-dev vim && \
    docker-php-ext-install zip
RUN groupadd -g ${GROUP_ID} user && \
    useradd -u ${USER_ID} -g ${GROUP_ID} user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user && \
    chmod 440 /etc/sudoers.d/user && \
    chsh -s /bin/bash user

USER user
WORKDIR /workspace
CMD [ "bash" ]

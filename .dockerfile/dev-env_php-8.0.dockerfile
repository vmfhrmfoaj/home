FROM php:8.0-fpm

ENV USER_ID=1000
ENV GROUP_ID=1000

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer
RUN apt -y update && \
    apt -y install bash-completion sudo vim zip libzip-dev libicu-dev && \
    docker-php-ext-install intl zip mysqli pdo pdo_mysql
# You can install extension on container by using following command:
#  PHP_INI_DIR=/usr/local/etc/php docker-php-ext-install ...

RUN groupadd -g ${GROUP_ID} user && \
    useradd -u ${USER_ID} -g ${GROUP_ID} user && \
    echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user && \
    chmod 440 /etc/sudoers.d/user && \
    chsh -s /bin/bash user

USER user
WORKDIR /workspace
CMD [ "bash" ]

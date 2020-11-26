FROM php:7.4-fpm

RUN apt-get update -y\
    && apt-get install -y build-essential  libzip-dev libpng-dev libjpeg62-turbo-dev \
    && apt-get install -y libfreetype6-dev libjpeg62-turbo-dev libfreetype6-dev libonig-dev locales \
    && apt-get install -y zip vim unzip git curl libpq-dev


RUN docker-php-ext-install pdo pdo_mysql mbstring zip exif pcntl gd opcache

ENV PHP_OPCACHE_VALIDATE_TIMESTAMPS="0"
ADD config/opcache.ini "$PHP_INI_DIR/conf.d/opcachle.ini"

ADD ./src /usr/share/nginx/html
WORKDIR /usr/share/nginx/html

RUN usermod -a -G www-data root
RUN chgrp -R www-data storage

RUN chown -R www-data:www-data ./storage
RUN chmod -R 0777 ./storage

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer &&\
    chmod +x /usr/local/bin/composer
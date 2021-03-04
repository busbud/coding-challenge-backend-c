FROM php:7.4-apache
RUN docker-php-ext-install pdo pdo_mysql
RUN a2enmod rewrite
ADD . /var/www/html



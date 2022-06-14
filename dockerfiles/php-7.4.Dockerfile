FROM php:7.4-cli-alpine

RUN docker-php-ext-install sockets


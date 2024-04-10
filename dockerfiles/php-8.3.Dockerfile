FROM php:8.3-cli-alpine

# For ext-sockets installation in php8.3
RUN apk add --no-cache linux-headers=6.5-r0

RUN docker-php-ext-install sockets


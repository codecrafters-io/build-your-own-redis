FROM gcc:12.2.0-bullseye

RUN apt-get update
RUN apt-get install -y cmake
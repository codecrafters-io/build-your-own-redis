FROM gcc:12.2.0-bullseye

RUN apt-get update && \
    apt-get install --no-install-recommends -y cmake=3.18.* && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

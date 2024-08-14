FROM maven:3.9.6-eclipse-temurin-17-focal

RUN apt-get update && \
    apt-get install --no-install-recommends apt-transport-https=* curl=* gnupg=* -yqq && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list 

RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import && \
    chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg

RUN apt-get update && \
    apt-get install --no-install-recommends sbt=1.9.9 -yqq && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=/usr/local/sbt/bin:$PATH

COPY build.sbt /app/build.sbt
COPY project /app/project

WORKDIR /app

RUN sbt update

RUN printf "cd \${CODECRAFTERS_REPOSITORY_DIR} && sbt assembly && sed -i 's/^\(sbt .*\)/#\1/' ./spawn_redis_server.sh\n" > /codecrafters-precompile.sh

RUN chmod +x /codecrafters-precompile.sh

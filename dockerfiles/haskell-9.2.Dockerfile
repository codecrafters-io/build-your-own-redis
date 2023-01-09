FROM haskell:9.2.5-buster

WORKDIR /tmp/app

RUN mkdir -p /etc/stack

# Absence of this causes `stack run` to raise a permissions error
RUN echo "allow-different-user: true" >> /etc/stack/config.yaml

# Force usage of the system GHC installation
RUN echo "install-ghc: false" >> /etc/stack/config.yaml
RUN echo "system-ghc: true" >> /etc/stack/config.yaml

COPY stack.yaml package.yaml stack.yaml.lock /tmp/app/

# Dummy static content to circumvent the /app doesn't exist warning
RUN mkdir /tmp/app/app
RUN echo 'main :: IO ()' >> /tmp/app/app/Main.hs
RUN echo 'main = putStrLn "Hello, World!"' >> /tmp/app/app/Main.hs

ENV STACK_ROOT=/tmp/app/.stack

RUN stack build
RUN stack clean hs-redis-clone
RUN mkdir /app-cached
RUN mv .stack-work /app-cached/.stack-work
RUN mv .stack /app-cached/.stack

RUN rm -rf /tmp/app/app

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && stack build" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="stack.yaml,package.yaml,stack.yaml.lock"
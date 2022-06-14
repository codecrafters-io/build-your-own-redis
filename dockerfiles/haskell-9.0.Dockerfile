FROM haskell:9.0.2-buster

WORKDIR /app

RUN mkdir -p /etc/stack

# Absence of this causes `stack run` to raise a permissions error
RUN echo "allow-different-user: true" >> /etc/stack/config.yaml

# Force usage of the system GHC installation
RUN echo "install-ghc: false" >> /etc/stack/config.yaml
RUN echo "system-ghc: true" >> /etc/stack/config.yaml

COPY stack.yaml package.yaml stack.yaml.lock /app/

# Dummy static content to circumvent the /app doesn't exist warning
RUN mkdir /app/app
RUN echo 'main :: IO ()' >> /app/app/Main.hs
RUN echo 'main = putStrLn "Hello, World!"' >> /app/app/Main.hs

RUN stack build

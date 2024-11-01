FROM haskell:9.10.1-bullseye

COPY . /app/glance

WORKDIR /app/glance

RUN cabal update
RUN cabal install

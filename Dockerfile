FROM haskell:latest
MAINTAINER Sean Clemmer <sclemmer@bluejeans.com>
COPY . /tmp/build
RUN apt-get update && apt-get install -y libsnappy-dev git build-essential make && cd /tmp && git clone https://github.com/edenhill/librdkafka && cd librdkafka && ./configure && make && make install && cd /tmp/build && cabal update && cabal install c2hs && cabal install split && cabal install text && cabal install && cabal configure && cabal build && cabal install
ENTRYPOINT [ "theon" ]

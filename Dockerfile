FROM haskell:latest
MAINTAINER Sean Clemmer <sclemmer@bluejeans.com>
RUN apt-get update && apt-get install -y libsnappy-dev git build-essential make
RUN cd /tmp && git clone https://github.com/edenhill/librdkafka && cd librdkafka && ./configure && make && make install
RUN cabal update && cabal install c2hs
COPY . /tmp/build
RUN cd /tmp/build && cabal install && cabal build && cp dist/build/theon/theon /usr/local/bin
ENTRYPOINT [ "theon" ]
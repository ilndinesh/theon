FROM haskell:latest
MAINTAINER Sean Clemmer <sclemmer@bluejeans.com>
RUN cabal update
COPY . /tmp/build
RUN cd /tmp/build && cabal install && cabal build && ./dist/build/theon/theon /usr/local/bin
RUN rm -rf /tmp/build
ENTRYPOINT [ "theon" ]
#!/usr/bin/env ruby
# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure('2') do |config|
  config.vm.provider :virtualbox do |vbox|
    vbox.customize [ 'modifyvm', :id, '--memory', 4096 ]
    vbox.customize [ 'modifyvm', :id, '--cpus', 4 ]
  end

  config.vm.define 'build' do |node|
    node.vm.box = 'bento/ubuntu-14.04'
    node.vm.hostname = 'build'
    node.vm.provision :shell, inline: <<-END
      export PATH="/root/.cabal/bin:$PATH"
      rm -r /vagrant/pkg
      mkdir -p /vagrant/pkg
      apt-get update
      apt-get install -y libsnappy-dev git build-essential make haskell-platform

      rdkafka_version=0.8.6
      rm -rf /tmp/librdkafka /tmp/rdkafka
      git clone https://github.com/edenhill/librdkafka /tmp/librdkafka
      cd /tmp/librdkafka
      git checkout tags/$rdkafka_version
      ./configure --prefix=/usr
      make
      make install DESTDIR=/tmp/rdkafka

      apt-get install -y ruby ruby-dev libxml2-dev libxslt1-dev
      gem install --no-rdoc --no-ri fpm
      cd /vagrant/pkg
      fpm --verbose \
        -s dir -t deb \
        -n librdkafka-dev -v $rdkafka_version \
        -d libsnappy1 \
        -d libsnappy-dev \
        -C /tmp/rdkafka \
        --license BSD \
        --description "The Apache Kafka C/C++ library" \
        --maintainer "Magnus Edenhill" \
        --vendor "Confluent" \
        --url "https://github.com/edenhill/librdkafka" \
        usr/include/librdkafka/rdkafkacpp.h=/usr/include/rdkafkacpp.h \
        usr/include/librdkafka/rdkafka.h=/usr/include/rdkafka.h \
        usr/include usr/lib
      dpkg -i librdkafka*.deb

      rm -rf /tmp/build
      cp -R /vagrant /tmp/build
      cd /tmp/build
      rm -rf dist

      cabal update
      cabal install c2hs
      cabal configure
      cabal build
      cabal install

      apt-get install -y ruby ruby-dev libxml2-dev libxslt1-dev
      gem install --no-rdoc --no-ri fpm
      cd /vagrant/pkg
      version=$(grep '^version:' ../*.cabal | cut -d: -f2 | xargs)
      fpm --verbose \
        -s dir -t deb \
        -n theon -v "$version" \
        -d libsnappy1 \
        -d libsnappy-dev \
        -d librdkafka-dev \
        --license ISC \
        --description "A simple HTTP-to-Kafka relay built for speed" \
        --maintainer "Sean Clemmer <sczizzo@gmail.com>" \
        --vendor "Blue Jeans Network" \
        --url "https://github.com/sczizzo/theon" \
        /tmp/build/dist/build/theon/theon=/usr/local/bin/theon
    END
  end

  config.vm.define 'test' do |node|
    node.vm.box = 'bento/ubuntu-14.04'
    node.vm.hostname = 'test'
    node.vm.provision :shell, inline: <<-END
      apt-get update
      apt-get install -y libsnappy1 libsnappy-dev
      cd /vagrant/pkg
      dpkg -i librdkafka*.deb
      dpkg -i theon*.deb
    END
  end
end
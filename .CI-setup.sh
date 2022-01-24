#!/bin/bash

### Setup singularity ###
# adapted from: https://sylabs.io/guides/3.0/user-guide/installation.html
# install dependencies
apt-get update
apt-get install -y \
    build-essential \
    libssl-dev \
    uuid-dev \
    libgpgme11-dev \
    squashfs-tools \
    libseccomp-dev \
    pkg-config \
    wget
# yum update -y && \
#     yum groupinstall -y 'Development Tools' && \
#     yum install -y \
#     openssl-devel \
#     libuuid-devel \
#     libseccomp-devel \
#     wget \
#     squashfs-tools



# install go
export GOVERSION=1.15.6 OS=linux ARCH=amd64
wget https://dl.google.com/go/go$GOVERSION.$OS-$ARCH.tar.gz
tar -C /usr/local -xzvf go$GOVERSION.$OS-$ARCH.tar.gz
rm go$GOVERSION.$OS-$ARCH.tar.gz

# install singularity
export VERSION=3.9.1
mkdir -p $GOPATH/src/github.com/sylabs
cd $GOPATH/src/github.com/sylabs
wget https://github.com/sylabs/singularity/releases/download/v${VERSION}/singularity-ce-${VERSION}.tar.gz
tar -xzf singularity-ce-${VERSION}.tar.gz
cd ./singularity
./mconfig
make -C ./builddir
make -C ./builddir install
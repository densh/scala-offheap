#!/bin/bash

set -ex

CWD=$PWD

cd /tmp

rm -rf /tmp/usr
mkdir -p /tmp/usr

rm -rf jemalloc_latest
mkdir -p jemalloc_latest

cd jemalloc_latest

git clone https://github.com/jemalloc/jemalloc.git
cd jemalloc

autoconf
./configure --prefix=/tmp/usr --with-jemalloc-prefix=je
sed -i 's/^CFLAGS := \(.*\)/CFLAGS := -fPIC \1/g' Makefile

make -j
make install_bin install_include install_lib

cd $CWD
cd c/
make clean
make
mv *.so ../main/resources

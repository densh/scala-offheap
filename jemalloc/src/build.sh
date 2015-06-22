#!/bin/bash

set -ex

ROOT=$PWD

cd ../
mkdir -p target
cd target
WORK=$PWD

rm -rf $WORK/usr
mkdir -p $WORK/usr

rm -rf jemalloc_latest
mkdir -p jemalloc_latest

cd jemalloc_latest

git clone https://github.com/jemalloc/jemalloc.git
cd jemalloc

autoconf
./configure --prefix=$WORK/usr --with-jemalloc-prefix=je
sed -i 's/^CFLAGS := \(.*\)/CFLAGS := -fPIC \1/g' Makefile

make -j
make install_bin install_include install_lib

cd $ROOT
cd c/
make clean
make
mv *.so ../main/resources

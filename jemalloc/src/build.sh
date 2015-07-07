#!/bin/bash

set -ex

ROOT=$PWD

cd ../
mkdir -p target
cd target
WORK=$PWD

rm -rf $WORK/usr
mkdir -p $WORK/usr

while [[ $# > 0 ]]
do
key="$1"

case $key in
    -ng)
    USE_GIT=false
    ;;
    *)
    ;;
esac
shift 
done

if $USE_GIT; then
    if [ -d "jemalloc_latest" ]; then
        cd jemalloc_latest
        git clean -xfd
        git pull
    else
        mkdir -p jemalloc_latest
        git clone https://github.com/jemalloc/jemalloc.git
    fi
else
    cd jemalloc_latest
fi

cd jemalloc

autoconf
./configure --prefix=$WORK/usr --with-jemalloc-prefix=je
sed -i 's/^CFLAGS := \(.*\)/CFLAGS := -fPIC \1/g' Makefile
sed -i 's/^LDFLAGS := \(.*\)/LDFLAGS := -fPIC \1/g' Makefile

make -j
make install_bin install_include install_lib

cd $ROOT

rm -rf main/resources/*

cd c/
make clean
make
mv *.so ../main/resources

#!/bin/sh

# This is because Travis CI uses very old GCC version
# which still not have C99 as a default.
# .travis.yml has installation instructions for gcc-7
# Thus we enable it as a default compiler for dependencies
export CC=gcc-7
export CXX=g++-7
echo "[*] Getting radare2"
git clone https://github.com/radare/radare2 || exit 1
cd radare2 || exit 1
./sys/install.sh  || exit 1
echo "[*] radare2 install success"

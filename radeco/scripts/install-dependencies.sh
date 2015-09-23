#!/bin/sh

echo "[*] Getting radare2"
git clone https://github.com/radare/radare2 || exit 1
cd radare2 || exit 1
./sys/install.sh  || exit 1
echo "[*] radare2 install success"

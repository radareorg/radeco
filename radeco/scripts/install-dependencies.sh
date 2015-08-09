#!/usr/bin/env sh

echo "[*] Getting radare2" && \
git clone https://github.com/radare/radare2 && \
cd radare2 && \
./sys/install.sh && \
echo "[*] radare2 install success"

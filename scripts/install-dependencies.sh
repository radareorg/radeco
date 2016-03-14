#!/usr/bin/env sh

echo "[*] Getting radare2" && \
git clone https://github.com/radare/radare2 && \
cd radare2 && \
./sys/user.sh && \
export PATH=$PATH:${HOME}/bin && \
echo "[*] radare2 install success"

#!/usr/bin/env sh

echo "[*] Getting radare2" && \
cd ${HOME} && \
git clone https://github.com/radare/radare2 && \
cd radare2 && \
./sys/user.sh && \
export PATH=$PATH:${HOME}/bin && \
cd ${HOME}/radare/radeco-lib && \
echo "[*] radare2 install success"

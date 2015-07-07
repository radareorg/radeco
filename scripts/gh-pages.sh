#!/bin/sh
cd ../ \
cargo doc && \
echo "<meta http-equiv=refresh content=0;url=`echo radeco | cut -d '/' -f 2`/index.html>" > target/doc/index.html && \
ghp-import -n target/doc && \
git push -f origin gh-pages

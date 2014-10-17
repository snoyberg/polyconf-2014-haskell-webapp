#!/bin/bash -ex
for f in Client/*.hs
do
    ghcjs $f
    cp -r ${f%.hs}.jsexe static
done

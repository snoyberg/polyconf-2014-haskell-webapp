#!/bin/bash -ex
ghcjs Client/Home.hs
cp -r Client/Home.jsexe static

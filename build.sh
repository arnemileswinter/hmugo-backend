#!/bin/sh
VERSION=$(yq -r .version package.yaml)
hpack && \ 
mkdir -p build && \
stack install hmugo:exe:hmugo --copy-bins --local-bin-path build && \
docker build . -t arnemileswinter.de/hmugo:$VERSION
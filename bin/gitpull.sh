#!/bin/bash

for i in `ls`; do
    if [ -d  "$i/.git" ]; then
        pushd $i
        git pull
        popd
    fi
done

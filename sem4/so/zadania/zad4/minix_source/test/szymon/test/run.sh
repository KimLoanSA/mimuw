#!/bin/sh

make
for f in ./*; do
    if test -x $f && ! test $f = $0; then
        ./$f
    fi
done

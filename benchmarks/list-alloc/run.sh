#!/usr/bin/env bash

set -e

if [ -z "$GHC" ]; then GHC=_build/stage1/bin/ghc; fi

$GHC Main.hs -ddump-simpl -rtsopts

run() {
  ../ghc-utils/ghc_perf.py --append --name=$NAME -o out.tsv -- ./Main $N +RTS $RTS_ARGS
}

for n in 1 5 10 50 100; do
  RTS_ARGS="-xn" NAME="xn//$n" N=$(($n * 1000 * 1000)) run
  RTS_ARGS=""    NAME="no//$n" N=$(($n * 1000 * 1000)) run
done

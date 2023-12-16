#!/bin/bash
BIN=$1
shift 1
for f in "$@"
do
    dir=$(dirname $f)
    file=$(basename $f)
    echo "Running with $f as input"
    time $BIN +RTS -M10g -s.rts_stats_${dir}_${file}.log < $f
done

#!/bin/bash
BIN=$1
shift 1
for f in "$@"
do
    dir=$(dirname $f)
    file=$(basename $f)
    if [ -e "$f" ]; then
        >&2 echo "Running $BIN with $f as input"
        time taskset --cpu-list 0-1 $BIN +RTS -M10g -s.rts_stats_${dir}_${file}.log < $f
    fi
done

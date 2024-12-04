#!/bin/bash
set -e

YEARS=`seq 2015 2024`
DAYS=`seq 1 25`

# Build markdown table of execution times

for y in $YEARS; do
    make -C $y -j all >&2
done

echo -n "| Day |"
for y in $YEARS; do
    echo -n "   $y |"
done
echo
echo -n "|-----|"
for y in $YEARS; do
    echo -n "-------:|"
done
echo
for d in $DAYS; do
    echo -n "| $d  | "
    for y in $YEARS; do
        DAY=$(printf "%02d" $d)
        if [ -e "$y/$DAY.hs" ]; then
            TIME=`make -sC $y bench day=$DAY ghcopts=-v0 | sed -e 's/^0m//' -e 's/\(m[0-9]\+\.[0-9]\)[0-9]\+/\1/'`
            echo -n " $TIME |"
        else
            echo -n "       |"
        fi
    done
    echo
done

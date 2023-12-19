#!/bin/bash
set -e

YEARS=`seq 2016 2023`
DAYS=`seq 1 25`

# Build markdown table of execution times

for y in $YEARS; do
    make -C $y -j all >&2
done

echo -n "| Day |"
for y in $YEARS; do
    echo -n "   $y   |"
done
echo
echo -n "|-----|"
for y in $YEARS; do
    echo -n "---------:|"
done
echo
for d in $DAYS; do
    echo -n "| $d  | "
    for y in $YEARS; do
        DAY=$(printf "%02d" $d)
        if [ -e "$y/$DAY.hs" ]; then
            if [ "$y/$d" == "2019/25" ]; then
                echo -n " Interactive |"
            else
                TIME=`make -sC $y bench day=$DAY ghcopts=-v0 | sed -e 's/^0m/  /' -e 's/m/m /'`
                echo -n " $TIME |"
            fi
        else
            echo -n "       |"
        fi
    done
    echo
done

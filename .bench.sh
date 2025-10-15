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
            TIMECMD="make -sC $y bench day=$DAY ghcopts=-v0"
            TIMES=$(for i in $(seq 1 5); do
                echo $($TIMECMD) | sed 's/s$//'
            done)
            MEDIAN_SECONDS=$(echo "$TIMES" | sort -n | sed -n '3p')
            INT_SECONDS=${MEDIAN_SECONDS%.*}
            FRACTIONAL_PART=${MEDIAN_SECONDS#*.}
            if (( $INT_SECONDS >= 60 )); then
                MINUTES=$(( $INT_SECONDS / 60 ))
                REM_SECONDS=$(( ${INT_SECONDS%.*} % 60 ))
                TIME=$(printf "%dm%02d.%s" "$MINUTES" "$REM_SECONDS" "${FRACTIONAL_PART:0:1}")
            else
                TIME=${MEDIAN_SECONDS}
            fi
            echo -n " ${TIME}s |"
        else
            echo -n "       |"
        fi
    done
    echo
    sleep 60
done
